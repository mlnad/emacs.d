;;; python-ext.el --- Python editing with the Astral toolchain -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Python in this configuration is driven by three programs from Astral, all
;; of them installed by `moyue install uv ruff ty' and reported by
;; `moyue doctor' (`config/tool-uv', `config/tool-ruff', `config/tool-ty' and
;; `config/lsp-python'):
;;
;;   uv    owns the interpreters and the project environment.  It puts a
;;         `.venv' next to the `pyproject.toml' that `uv sync' reads.
;;   ty    is the language server eglot prefers (`ty server'), so it is what
;;         provides navigation, completion and type diagnostics.
;;   ruff  is the linter and the formatter: apheleia runs `ruff check
;;         --select I --fix' and then `ruff format', and flymake runs
;;         `ruff check' beside the type checker through the backend below.
;;
;; This library holds the parts that are more than a `use-package' form:
;;
;;   `moyu/python-project-root'     where the project starts
;;   `moyu/python-venv-root'        the `.venv' uv created there, if any
;;   `moyu/python-activate-venv'    make it this buffer's environment
;;   `moyu/python-setup'            the `python-base-mode-hook' entry point
;;   `moyu/python-uv-map'           `uv' commands, bound to `C-c u' in
;;                                  `python-base-mode-map' by init.org
;;   `moyu/python-eglot-servers'    the language-server preference eglot gets
;;   `moyu/python-ruff-backend'     ruff findings as flymake diagnostics
;;   `moyu/org-ipython-*'           the `#+OB_IPYTHON:' bridge for Org files
;;
;; The environment is activated buffer-locally rather than globally: two
;; Python buffers from two projects must not fight over `exec-path', and a
;; remote workspace must keep using the tools of the host that owns its
;; files: the functions that look for tools in the environment stop at
;; `file-remote-p', while project discovery and the uv commands stay
;; remote-aware because `remote-ext' routes their processes.
;;
;; The ERT tests at the end of this file run in batch; the exact command and
;; the `moyue test' selector are at the top of the test section.
;;
;;; Code:

(require 'cl-lib)
(require 'project)

;; Variables and functions owned by libraries that are loaded later than this
;; one.  None of apheleia, ob-ipython or eglot is loaded at startup, and the
;; byte compiler should not have to guess about them.
(defvar apheleia-formatter)
(defvar eglot-server-programs)
(defvar ob-ipython-command)
(defvar org-babel-default-header-args:ipython)
(defvar python-shell-interpreter)
(defvar python-shell-virtualenv-root)
(declare-function compile "compile" (command &optional comint))
(declare-function eglot--lookup-mode "eglot" (mode))
(declare-function eglot-alternatives "eglot" (alternatives))
(declare-function flymake-diag-region "flymake" (buffer line col))
(declare-function flymake-diagnostic-beg "flymake" (diagnostic))
(declare-function flymake-diagnostic-end "flymake" (diagnostic))
(declare-function flymake-diagnostic-text "flymake" (diagnostic))
(declare-function flymake-diagnostic-type "flymake" (diagnostic))
(declare-function flymake-make-diagnostic "flymake"
                  (buffer beg end type text &optional backend))
(declare-function ob-ipython-inspect "ob-ipython" (buffer pos))
(declare-function org-babel-execute:ipython "ob-ipython" (body params))
(declare-function org-collect-keywords "org" (keywords &optional unique directory))
(declare-function org-src-edit-buffer-p "org-src" (&optional buffer))
(declare-function org-src-source-buffer "org-src" ())
(declare-function project-root "project" (project))

(defgroup moyu/python nil
  "Editing Python with the Astral toolchain."
  :group 'languages
  :prefix "moyu/python-")

(defcustom moyu/python-uv-program "uv"
  "Name of the uv executable, as looked up in the variable `exec-path'."
  :type 'string
  :group 'moyu/python)

(defcustom moyu/python-project-markers
  '("pyproject.toml" "uv.lock" "requirements.txt" "setup.py" "setup.cfg" "Pipfile")
  "Files that mark the root of a Python project.
Only consulted when `project-current' does not already know the project."
  :type '(repeat string)
  :group 'moyu/python)

(defcustom moyu/python-venv-directory ".venv"
  "Name of the environment directory, relative to the project root.
This is where uv puts the environment its `sync' and `run' commands use."
  :type 'string
  :group 'moyu/python)

(defcustom moyu/python-eglot-servers
  '(("ty" "server")
    ("ruff" "server")
    ("basedpyright-langserver" "--stdio")
    ("pyright-langserver" "--stdio")
    "pylsp"
    "pyls"
    "jedi-language-server")
  "Language servers eglot tries for Python, the preferred one first.
Passed to `eglot-alternatives', so a string or a (PROGRAM ARGS...) list, and
installed by `moyu/python-register-eglot-servers' when eglot is loaded; nil
leaves eglot's own Python entry in place.  ty is first because it is the
server this configuration installs and because it is the one that navigates
and type checks; ruff's server is the fallback for a machine that has ruff
but no ty."
  :type '(repeat (choice string (repeat string)))
  :group 'moyu/python)

(defcustom moyu/python-ruff-arguments
  '("check" "--output-format" "concise" "--exit-zero" "--quiet")
  "Arguments for `ruff', before the file name and the final `-'.
`--exit-zero' keeps a finding from looking like a failure, `--quiet' drops
the summary that would otherwise be parsed as a diagnostic, and concise
output is one `FILE:LINE:COL: CODE MESSAGE' per line."
  :type '(repeat string)
  :group 'moyu/python)

;;;; ── Where the project and its environment are ─────────────────────────────

(defun moyu/python--directory (&optional directory)
  "Return DIRECTORY as an absolute directory name.
DIRECTORY defaults to `default-directory'.  The trailing slash is what the
`file-name-*' functions called on the result expect."
  (file-name-as-directory
   (expand-file-name (or directory default-directory))))

(defun moyu/python--project-marker-p (directory)
  "Return non-nil when DIRECTORY looks like the root of a Python project."
  (or (file-exists-p (expand-file-name moyu/python-venv-directory directory))
      (cl-find-if (lambda (marker)
                    (file-exists-p (expand-file-name marker directory)))
                  moyu/python-project-markers)))

(defun moyu/python-local-directory-p (directory)
  "Return non-nil when DIRECTORY is on this machine.
A remote workspace has its environment on the remote host, where this
library cannot reach it; its own tools are the ones that should run."
  (not (file-remote-p directory)))

(defun moyu/python-project-root (&optional directory)
  "Return the root of the Python project containing DIRECTORY, or nil.
DIRECTORY defaults to `default-directory'.  The nearest ancestor carrying a
`.venv' or one of `moyu/python-project-markers' is the project.  A VCS
checkout root is used only when it carries such a marker itself, so a
checkout that merely contains a nested Python project does not make that
project's `uv' commands run at the checkout's root; with no Python marker
anywhere, the checkout root is the fallback.  The result is an absolute
directory name, with its trailing slash."
  (let* ((dir (moyu/python--directory directory))
         (marker-root (locate-dominating-file
                       dir #'moyu/python--project-marker-p))
         (vcs-root (when-let* ((project (project-current nil dir)))
                     (moyu/python--directory (project-root project)))))
    (cond
     ((and vcs-root (moyu/python--project-marker-p vcs-root)) vcs-root)
     (marker-root (file-name-as-directory marker-root))
     (t vcs-root))))

(defun moyu/python--venv-bin-directory (venv)
  "Return the directory VENV keeps its programs in.
A virtualenv spells that `bin' on POSIX and `Scripts' on Windows."
  (expand-file-name (if (eq system-type 'windows-nt) "Scripts" "bin") venv))

(defun moyu/python--venv-program (venv name)
  "Return the path of program NAME inside the virtualenv VENV.
This is where a package the project declared is looked for first, so the
project's own copy is the one that is found; PATH is each caller's fallback."
  (expand-file-name (if (eq system-type 'windows-nt)
                        (concat name ".exe")
                      name)
                    (moyu/python--venv-bin-directory venv)))

(defun moyu/python--venv-directory (directory)
  "Return DIRECTORY when it carries a Python interpreter, else nil.
A `.venv' that stopped halfway through being created has no `bin/python',
and pointing `python-shell-virtualenv-root' at one breaks every Python
command with no visible reason."
  (and (file-executable-p (moyu/python--venv-program directory "python"))
       directory))

(defun moyu/python--venv-in (directory)
  "Return the virtualenv directly inside DIRECTORY, when it is usable."
  (moyu/python--venv-directory
   (expand-file-name moyu/python-venv-directory directory)))

(defun moyu/python-venv-root (&optional directory)
  "Return the nearest usable virtualenv of the project containing DIRECTORY.
DIRECTORY defaults to `default-directory'.  The nearest `.venv' with an
interpreter wins, so a nested package that uv initialised on its own is used
rather than an outer environment.  A `.venv' without an interpreter -- one uv
left half-created -- is skipped instead of being reported as the environment,
so it cannot hide a working one above it.  A remote DIRECTORY has its
environment on the remote host, where this library cannot use it, so nil is
returned there."
  (let ((dir (moyu/python--directory directory)))
    (when (moyu/python-local-directory-p dir)
      (let (venv)
        (while (and dir (not venv))
          (setq venv (moyu/python--venv-in dir))
          (setq dir (file-name-parent-directory dir)))
        venv))))

;;;; ── Programs from the project's environment ──────────────────────────────

(defun moyu/python-environment-program (name)
  "Return program NAME from the project's environment, or nil.
The environment uv made for the project is asked first, so a package the
project declared is the one that is found; callers fall back to
`executable-find' on PATH when this returns nil."
  (when-let* ((venv (moyu/python-venv-root))
              (program (moyu/python--venv-program venv name)))
    (and (file-executable-p program) program)))

(defun moyu/python--program (name)
  "Return the program NAME the project should run, or nil.
The project's environment comes first, because that is where a version the
project pinned lives; the PATH of the current buffer is the fallback."
  (or (moyu/python-environment-program name)
      (executable-find name)))

(defun moyu/python-formatter ()
  "Return the apheleia formatter chain for the current Python buffer.
Ruff sorts the imports and then formats; black is the fallback for a machine
that has no ruff."
  (cond ((moyu/python--program "ruff") '(ruff-isort ruff))
        ((moyu/python--program "black") 'black)
        (t nil)))

;;;; ── Activating the environment ────────────────────────────────────────────

(defvar-local moyu/python-venv-indicator nil
  "The mode line text naming the virtualenv this buffer uses.
Empty, and therefore invisible, unless `moyu/python-activate-venv' found one.
It is kept in a variable of its own rather than behind `:eval' so that the
mode line only has to look the value up, not run code on every redisplay.")

(defvar-local moyu/python--active-bin nil
  "The environment's bin directory `moyu/python-activate-venv' put on PATH.
Kept buffer-locally so that activating another environment can take the
previous one back off instead of leaving both in the variable `exec-path'.")

(defun moyu/python--venv-indicator (venv)
  "Name VENV on the mode line of the current buffer, once.
The name goes into `mode-line-misc-info', which doom-modeline renders in its
`misc-info' slot, so it appears beside the other miscellaneous information
rather than inside the mode name."
  (let ((name (file-name-base (directory-file-name venv))))
    ;; A virtualenv is normally called `.venv', which says nothing about
    ;; which project it belongs to, so the project's own name stands in.
    (when (equal name moyu/python-venv-directory)
      (setq name (file-name-base
                  (directory-file-name
                   (file-name-directory (directory-file-name venv))))))
    (setq moyu/python-venv-indicator
          (propertize (format " venv:%s " name)
                      'face 'mode-line-emphasis
                      'help-echo venv)))
  (unless (memq 'moyu/python-venv-indicator mode-line-misc-info)
    (setq-local mode-line-misc-info
                (cons 'moyu/python-venv-indicator mode-line-misc-info))))

(defun moyu/python--export (name value)
  "Set NAME to VALUE in this buffer's `process-environment'.
An old entry with the same name is replaced rather than shadowed, so that
repeated activations do not grow the list."
  (setq-local process-environment
              (cons (concat name "=" value)
                    (cl-remove-if
                     (lambda (entry)
                       (string-prefix-p (concat name "=") entry))
                     process-environment))))

(defun moyu/python--activate-path (venv)
  "Make VENV's programs reachable from the current buffer, once.
The environment's bin directory goes to the front of the variable
`exec-path' and PATH, and VIRTUAL_ENV names the environment.  Calling this
again with the same environment is a no-op; activating a different one first
takes the previous environment back off, so a buffer that changes project
does not end up with both environments on its path."
  (let* ((bin (moyu/python--venv-bin-directory venv))
         (old moyu/python--active-bin))
    (when (and old (not (equal old bin)))
      (setq-local exec-path (cl-remove old exec-path :test #'equal))
      (moyu/python--export
       "PATH"
       (mapconcat
        #'identity
        (cl-remove old (split-string (or (getenv "PATH") "")
                                     path-separator t)
                   :test #'equal)
        path-separator)))
    (unless (member bin exec-path)
      (setq-local exec-path (cons bin exec-path)))
    (let ((path (getenv "PATH")))
      (unless (equal bin (car (split-string (or path "") path-separator)))
        (moyu/python--export
         "PATH"
         (if (or (null path) (equal path ""))
             bin
           (concat bin path-separator path)))))
    (moyu/python--export "VIRTUAL_ENV" (directory-file-name venv))
    (setq-local moyu/python--active-bin bin)))

(defun moyu/python-activate-venv (&optional directory)
  "Make the project's virtualenv the one the current buffer uses.
DIRECTORY defaults to `default-directory'.  `python-shell-virtualenv-root'
is set buffer-locally, the environment's bin directory goes to the front of
the variable `exec-path' and PATH, and `VIRTUAL_ENV' is exported -- so that
`run-python', eglot, apheleia and flymake all reach the interpreter and the
tools the project pinned; its name goes on the mode line as well.  Returns
the environment, or nil when there is none or DIRECTORY is remote.

`pyvenv-activate' -- and the `pythonic' that `uv-mode' wraps -- would do this
too, but for the whole session: they set the globals and prepend to global
PATH, so two projects open at once would take turns pointing the variable
`exec-path' at each other's environment.  Doing it here is what keeps them
apart."
  (let ((dir (or directory default-directory)))
    (when (moyu/python-local-directory-p dir)
      (when-let* ((venv (moyu/python-venv-root dir)))
        (setq-local python-shell-virtualenv-root venv)
        (moyu/python--venv-indicator venv)
        (moyu/python--activate-path venv)
        venv))))

;;;; ── ruff as a flymake backend ─────────────────────────────────────────────

(defconst moyu/python-ruff--regexp
  (rx line-start
      (group (*? nonl)) ":"                   ; file
      (group (+ digit)) ":"                   ; line
      (group (+ digit)) ": "                  ; column
      (group (any alpha) (* (any alnum "-"))) ; rule
      (? ":")                                 ; syntax errors print one
      (? " [*]")
      " "
      (group (* nonl))
      line-end)
  "Match one line of `ruff check --output-format concise'.
The groups are the file, the line, the column, the rule (which is
`invalid-syntax' for a syntax error) and the message.  The file is matched
lazily, so a message that itself contains `:LINE:COL:' text -- ruff does
produce such messages -- cannot be mistaken for the location.")

(defconst moyu/python-ruff-severity-alist
  '(("invalid-syntax" . :error)
    ("E" . :error)
    ("F" . :error)
    ("W" . :warning)
    ("B" . :warning)
    ("C90" . :warning)
    ("PERF" . :warning)
    ("N" . :note)
    ("I" . :note)
    ("UP" . :note)
    ("SIM" . :note))
  "Flymake severity for a ruff rule, by the first prefix here that matches.
A prefix only matches a whole rule family, so `E' matches `E501' but not
`ERA001'; a rule that matches nothing is reported as a warning, which is
what a diagnostic nobody classified deserves.")

(defun moyu/python-ruff--program ()
  "Return the ruff to run, or nil.
The project's environment comes first, because that is where a version the
project pinned lives; PATH is the fallback."
  (moyu/python--program "ruff"))

(defun moyu/python-ruff--severity (code)
  "Return the flymake severity of ruff rule CODE.
A rule is matched by its family: the entry's prefix has to be followed by a
digit in CODE (or be CODE itself), so `E' does not swallow `ERA001'."
  (or (cdr (cl-find-if
            (lambda (entry)
              (let ((prefix (car entry)))
                (and (string-prefix-p prefix code)
                     (or (equal prefix code)
                         (let ((next (aref code (length prefix))))
                           (and (>= next ?0) (<= next ?9)))))))
            moyu/python-ruff-severity-alist))
      :warning))

(defun moyu/python-ruff--check (source)
  "Return ruff's concise output for the buffer SOURCE.
The check runs from the project root and is handed SOURCE's file name, so
that ruff finds the same configuration it would for a saved file.  Standard
error is kept out of the parsed output and a non-zero exit status signals an
error, so that a broken ruff is not mistaken for a clean file."
  (let* ((default-directory (or (moyu/python-project-root) default-directory))
         (file (buffer-file-name source))
         (program (moyu/python-ruff--program))
         (arguments (append moyu/python-ruff-arguments
                            (when file (list "--stdin-filename" file))
                            '("-")))
         (errors (make-temp-file "python-ext-ruff-errors-"))
         (status nil))
    (unwind-protect
        (with-temp-buffer
          (insert-buffer-substring source)
          (setq status
                (apply #'call-process-region
                       (point-min) (point-max) program t
                       (list (current-buffer) errors) nil arguments))
          (if (and (integerp status) (zerop status))
              (buffer-string)
            (with-temp-buffer
              (insert-file-contents errors)
              (error "Ruff check failed (exit %s): %s"
                     status (string-trim (buffer-string))))))
      (ignore-errors (delete-file errors)))))

(defun moyu/python-ruff--diagnostics (output buffer)
  "Return the flymake diagnostics OUTPUT describes, for BUFFER."
  (let ((diagnostics '()))
    (dolist (line (split-string output "\n" t))
      (when (string-match moyu/python-ruff--regexp line)
        (let ((region (flymake-diag-region buffer
                                           (string-to-number (match-string 2 line))
                                           (string-to-number (match-string 3 line)))))
          (when region
            (push (flymake-make-diagnostic
                   buffer (car region) (cdr region)
                   (moyu/python-ruff--severity (match-string 4 line))
                   (format "ruff %s: %s" (match-string 4 line)
                           (match-string 5 line)))
                  diagnostics)))))
    (nreverse diagnostics)))

(defun moyu/python-ruff-backend (report-fn &rest _args)
  "Report ruff's linter for the current buffer to flymake.
REPORT-FN is flymake's callback; ARGS are ignored, because a fresh check is
what this backend does.  A failure of ruff itself is reported as a panic
rather than as an empty list of diagnostics, so that it is not mistaken for
a clean file."
  (if-let* ((program (moyu/python-ruff--program)))
      (let ((buffer (current-buffer)))
        (condition-case err
            (funcall report-fn
                     (moyu/python-ruff--diagnostics
                      (moyu/python-ruff--check buffer) buffer))
          (error (funcall report-fn :panic
                          :explanation (error-message-string err)))))
    (funcall report-fn :panic :explanation "ruff is not installed")))

(defun moyu/python-flymake-ruff-load ()
  "Let flymake report ruff's linter in this buffer.
Only when ty is installed as well: with `ty server' as the eglot server the
two report different things, while a fallback to `ruff server' already
carries every ruff diagnostic and would show each of them twice.  Remote
buffers are left alone, because a local ruff would be reading files it cannot
see."
  (when (and (moyu/python-local-directory-p default-directory)
             (moyu/python-ruff--program)
             (moyu/python--program "ty"))
    (require 'flymake)
    (add-hook 'flymake-diagnostic-functions #'moyu/python-ruff-backend nil t)))

;;;; ── ob-ipython, for the Org files that ask for it ────────────────────────

(defconst moyu/org-ipython-keywords
  '("OB_IPYTHON" "OB_IPYTHON_KERNEL" "OB_IPYTHON_SESSION")
  "The keywords an Org file uses to ask for `ob-ipython'.
`OB_IPYTHON' turns it on for that file and may name the environment; the
other two give the kernel and the session a block would otherwise repeat.")

(defun moyu/org-ipython--environment-pair (venv)
  "Return (JUPYTER . PYTHON) for the virtualenv VENV, or nil.
Only an environment that carries both can start a kernel and run the client
that talks to it."
  (when venv
    (let ((jupyter (moyu/python--venv-program venv "jupyter"))
          (python (moyu/python--venv-program venv "python")))
      (when (and (file-executable-p jupyter) (file-executable-p python))
        (cons jupyter python)))))

(defun moyu/org-ipython-environment (value)
  "Return (JUPYTER . PYTHON) for an `OB_IPYTHON' keyword VALUE, or nil.
VALUE may be empty, t or `yes' for the project's own environment -- the
nearest `.venv' that carries an interpreter, and PATH after that -- or the
directory of one, which may also be a project with a `.venv' in it."
  (if (member value '(nil "" "t" "yes"))
      (or (moyu/org-ipython--environment-pair (moyu/python-venv-root))
          (let ((jupyter (executable-find "jupyter"))
                (python (or (executable-find "python")
                            (executable-find "python3"))))
            (when (and jupyter python) (cons jupyter python))))
    (let* ((directory (file-name-as-directory (expand-file-name value)))
           (venv (if (file-executable-p (moyu/python--venv-program directory "python"))
                     directory
                   (expand-file-name moyu/python-venv-directory directory))))
      (moyu/org-ipython--environment-pair venv))))

(defun moyu/org-ipython-setup ()
  "Configure ob-ipython for this buffer when the file asks for it.
The file asks with `#+OB_IPYTHON:'; without that keyword this does nothing at
all, so an Org file that never mentions it keeps `python' blocks to ob-python
and no jupyter is ever looked for.  With it, the environment named there (or
the project's own) becomes the `jupyter' ob-ipython starts kernels with and
the interpreter its client runs with, while

  #+OB_IPYTHON_KERNEL:   the kernel a block gets by default
  #+OB_IPYTHON_SESSION:  the session a block gets by default, because
                         ob-ipython requires one; the file's name otherwise

Only an Org buffer is worth looking at.  A mode hook can be flushed later, in
a buffer that has changed mode since the hook was queued (see
`delayed-mode-hooks'), and the Org parser warns about every keyword-looking
line in a buffer that is not in Org mode."
  (when (derived-mode-p 'org-mode)
    (let ((keywords (org-collect-keywords moyu/org-ipython-keywords)))
      (when (assoc "OB_IPYTHON" keywords #'equal)
        (cl-labels ((value (name) (cadr (assoc name keywords #'equal))))
          (if-let* ((environment (moyu/org-ipython-environment (value "OB_IPYTHON"))))
              (let ((parameters (list (cons :session
                                            (or (value "OB_IPYTHON_SESSION")
                                                (file-name-base (or buffer-file-name "ipython")))))))
                (when-let* ((kernel (value "OB_IPYTHON_KERNEL")))
                  (push (cons :kernel kernel) parameters))
                (setq-local ob-ipython-command (car environment))
                (setq-local python-shell-interpreter (cdr environment))
                (setq-local org-babel-default-header-args:ipython parameters))
            (message "No jupyter for `%s' (ob-ipython)" (value "OB_IPYTHON"))))))))

(defvar moyu/org-ipython--client-python nil
  "The interpreter ob-ipython's client should use, while a block runs.
Deliberately never buffer-local: `ob-ipython' asks for its interpreter from a
temporary buffer, where only a dynamic binding reaches.  A `let' of
`python-shell-interpreter' itself would not do, because the buffer-local
binding the Org file sets shadows it, and the temporary buffer would then read
that variable's global default instead of this file's environment.")

(defun moyu/org-ipython-lift (function &rest arguments)
  "Call FUNCTION with the environment this Org file declared carried along.
ARGUMENTS are forwarded to FUNCTION unchanged.  The value travels in
`moyu/org-ipython--client-python' rather than in `python-shell-interpreter',
for the reason given there; a file that never asked for ob-ipython has no
buffer-local interpreter and carries nothing."
  (let ((moyu/org-ipython--client-python
         (when (local-variable-p 'python-shell-interpreter)
           python-shell-interpreter)))
    (apply function arguments)))

(defun moyu/org-ipython-get-python (get-python)
  "Return the interpreter ob-ipython should run its client with.
GET-PYTHON is ob-ipython's own answer, and is used unless a declared
environment was carried into this call."
  (if (and moyu/org-ipython--client-python
           (file-executable-p moyu/org-ipython--client-python))
      moyu/org-ipython--client-python
    (funcall get-python)))

(defun moyu/org-ipython-edit-lift (function &rest arguments)
  "Call FUNCTION with the source Org buffer's environment copied in.
ARGUMENTS are forwarded to FUNCTION unchanged.  `org-babel-edit-prep:ipython'
runs in a source-edit buffer, which carries
none of the buffer-local variables `moyu/org-ipython-setup' set in the Org
file, and it starts a kernel and prepares completion with them.  A buffer
that is not an edit buffer, or an Org file that declared nothing, is left
alone."
  (when (and (fboundp 'org-src-edit-buffer-p)
             (org-src-edit-buffer-p))
    (let ((source (ignore-errors (org-src-source-buffer)))
          command python)
      (when (buffer-live-p source)
        (with-current-buffer source
          (when (local-variable-p 'ob-ipython-command)
            (setq command ob-ipython-command))
          (when (local-variable-p 'python-shell-interpreter)
            (setq python python-shell-interpreter))))
      (when command (setq-local ob-ipython-command command))
      (when python (setq-local python-shell-interpreter python))))
  (apply function arguments))

(defun moyu/org-ipython-advise ()
  "Let ob-ipython use the environment the Org file declared."
  (dolist (function '(org-babel-execute:ipython ob-ipython-inspect))
    (unless (advice-member-p #'moyu/org-ipython-lift function)
      (advice-add function :around #'moyu/org-ipython-lift)))
  (when (fboundp 'org-babel-edit-prep:ipython)
    (unless (advice-member-p #'moyu/org-ipython-edit-lift
                             'org-babel-edit-prep:ipython)
      (advice-add 'org-babel-edit-prep:ipython :around
                  #'moyu/org-ipython-edit-lift)))
  (unless (advice-member-p #'moyu/org-ipython-get-python
                           'ob-ipython--get-python)
    (advice-add 'ob-ipython--get-python :around
                #'moyu/org-ipython-get-python)))

;;;; ── The language servers eglot is given ──────────────────────────────────

(defun moyu/python-eglot-contact (&optional interactive project)
  "Return the eglot contact for the current Python buffer.
INTERACTIVE and PROJECT are forwarded to `eglot-alternatives', which tries
`moyu/python-eglot-servers' in order; a nil list leaves the choice to eglot
and returns nil.  The project's environment is activated first, because eglot
probes servers from `prog-mode-hook', which runs before
`python-base-mode-hook' has put the project's bin directory on the variable
`exec-path'; a ty that exists only inside the project's `.venv' would
otherwise be invisible."
  (moyu/python-activate-venv)
  (when moyu/python-eglot-servers
    (funcall (eglot-alternatives moyu/python-eglot-servers)
             interactive project)))

(defvar moyu/python--eglot-builtin-entry nil
  "Eglot's own Python entry, kept so a nil preference can put it back.
Captured the first time `moyu/python-register-eglot-servers' replaces it.")

(defun moyu/python-register-eglot-servers ()
  "Make `moyu/python-eglot-contact' eglot's Python contact.
Eglot ships a Python entry of its own, so that entry is replaced rather than
shadowed, and remembered so that a nil `moyu/python-eglot-servers' can put it
back.  Calling this again is idempotent."
  (require 'eglot)
  (let* ((modes '(python-mode python-ts-mode))
         (entry (assoc modes eglot-server-programs)))
    ;; Our own contact has the same mode list as eglot's entry, so only the
    ;; other one is worth remembering.
    (when (and entry
               (not (eq (cdr entry) #'moyu/python-eglot-contact)))
      (setq moyu/python--eglot-builtin-entry entry))
    (setq eglot-server-programs
          (assoc-delete-all modes eglot-server-programs))
    (if (null moyu/python-eglot-servers)
        (when-let* ((builtin moyu/python--eglot-builtin-entry))
          (push builtin eglot-server-programs))
      (push (cons modes #'moyu/python-eglot-contact)
            eglot-server-programs))))

(with-eval-after-load 'eglot
  (moyu/python-register-eglot-servers))

;;;; ── Driving uv ───────────────────────────────────────────────────────────

(defun moyu/python--split-command (command)
  "Split COMMAND into its arguments, honouring quotes and backslashes.
`split-string-and-unquote' rather than `split-string', so that a quoted
argument such as `-c \"print(1)\"' reaches uv as one argument."
  (split-string-and-unquote command))

(defun moyu/python-uv--compile (arguments)
  "Run `uv ARGUMENTS' in the project root.
ARGUMENTS is a list of strings.  The process starts in the project root
rather than in the buffer's own directory so that uv finds the
`pyproject.toml' it is meant to change even from a file in a subdirectory."
  (let* ((root (or (moyu/python-project-root) default-directory))
         (default-directory (file-name-as-directory root))
         (command (mapconcat #'shell-quote-argument
                             (cons moyu/python-uv-program arguments) " ")))
    (compile command)))

;;;###autoload
(defun moyu/python-uv-init (directory)
  "Create a new uv project in DIRECTORY."
  (interactive "DProject directory: ")
  (moyu/python-uv--compile (list "init" (expand-file-name directory))))

;;;###autoload
(defun moyu/python-uv-sync ()
  "Create or refresh the project environment (`uv sync')."
  (interactive)
  (moyu/python-uv--compile '("sync")))

;;;###autoload
(defun moyu/python-uv-lock ()
  "Update the project lock file (`uv lock')."
  (interactive)
  (moyu/python-uv--compile '("lock")))

;;;###autoload
(defun moyu/python-uv-add (packages)
  "Add PACKAGES to the project (`uv add')."
  (interactive "sPackage(s): ")
  (moyu/python-uv--compile
   (cons "add" (moyu/python--split-command packages))))

;;;###autoload
(defun moyu/python-uv-run (command)
  "Run COMMAND in the project environment (`uv run')."
  (interactive "sCommand: ")
  (moyu/python-uv--compile
   (cons "run" (moyu/python--split-command command))))

;;;###autoload
(defun moyu/python-uv-run-file ()
  "Run the current file in the project environment (`uv run FILE').
This is also how a PEP 723 script runs: uv reads the inline metadata the
script carries and builds the environment it names."
  (interactive)
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (moyu/python-uv--compile (list "run" (expand-file-name buffer-file-name))))

(defvar-keymap moyu/python-uv-map
  :doc "uv commands for the current Python project."
  "a" #'moyu/python-uv-add
  "i" #'moyu/python-uv-init
  "l" #'moyu/python-uv-lock
  "r" #'moyu/python-uv-run
  "R" #'moyu/python-uv-run-file
  "s" #'moyu/python-uv-sync)

;;;; ── The mode hook ─────────────────────────────────────────────────────────

;;;###autoload
(defun moyu/python-setup ()
  "Prepare the current Python buffer for the Astral toolchain.
Added to `python-base-mode-hook': activate the project's uv environment,
choose the formatter, and start ruff's linter when that does not duplicate
what eglot already reports.  The interactive shell is left alone: it is
`python-shell-interpreter' that decides it, and activating the environment is
already what makes that name resolve to the project's own Python.

Nothing is done elsewhere.  A mode hook can be flushed later, in a buffer that
has since changed mode (see `delayed-mode-hooks'), and activating an
environment or adding a linter there would be wrong."
  (when (derived-mode-p 'python-base-mode)
    (moyu/python-activate-venv)
    (setq-local apheleia-formatter (moyu/python-formatter))
    (moyu/python-flymake-ruff-load)))

;;;; ── Tests ───────────────────────────────────────────────────────────────
;; Run:  emacs -q --batch -L lisp -l lisp/python-ext.el \
;;         --eval "(ert-run-tests-batch-and-exit)"
;; Or:   bin/moyue test '"^python-ext/"'

(with-eval-after-load 'ert

  ;; The helpers are defined inside `eval-and-compile' because the byte
  ;; compiler compiles this after-load body as a single form: without it they
  ;; are not known yet when the tests that call them are compiled, and the
  ;; tests compiled that way fail at run time.
  (eval-and-compile

    (defmacro python-test--project (&rest body)
      "Run BODY with `root' bound to an empty temporary project directory."
      (declare (indent 0))
      `(let ((root (file-name-as-directory (make-temp-file "python-ext-test-" t))))
         (unwind-protect (progn ,@body)
           (ignore-errors (delete-directory root t)))))

    (defun python-test--tool (venv name)
      "Create an executable NAME in VENV and return its path."
      (let ((file (moyu/python--venv-program venv name)))
        (make-directory (moyu/python--venv-bin-directory venv) t)
        (with-temp-file file (insert "#!/bin/sh\n"))
        (set-file-modes file #o755)
        file))

    (defun python-test--venv (root)
      "Create a minimal .venv under ROOT and return its directory."
      (let ((venv (expand-file-name ".venv" root)))
        (python-test--tool venv "python")
        venv))

    (defun python-test--marker (root name)
      "Create the project marker NAME under ROOT."
      (with-temp-file (expand-file-name name root) (insert "")))

    (defun python-test--org (root name &rest lines)
      "Return an Org buffer for ROOT/NAME whose body is LINES.
The buffer is in `org-mode' with `default-directory' at ROOT, so that the
keyword setup can be exercised the way a real file would exercise it."
      (let ((buffer (generate-new-buffer name)))
        (with-current-buffer buffer
          (setq buffer-file-name (expand-file-name name root))
          (setq-local default-directory root)
          (insert (mapconcat #'identity lines "\n") "\n")
          (org-mode))
        buffer)))

  ;;; moyu/python-project-root

  (ert-deftest python-ext/project-root-follows-markers ()
    "A pyproject in an ancestor is the project root; a subdirectory is not."
    (python-test--project
      (let ((nested (expand-file-name "pkg/sub" root)))
        (make-directory nested t)
        (python-test--marker root "pyproject.toml")
        (should (equal (moyu/python-project-root nested) root))
        ;; A directory with nothing above it but the temporary directory has
        ;; no project at all.
        (should-not (moyu/python-project-root
                     (file-name-as-directory (make-temp-file "python-ext-none-" t)))))))

  (ert-deftest python-ext/project-root-ignores-unrelated-directories ()
    (python-test--project
      (let ((nested (expand-file-name "src" root)))
        (make-directory nested t)
        (python-test--marker root "pyproject.toml")
        ;; `README.md' is not a Python marker, so the search continues past
        ;; the subdirectory that carries one to the root that does not.
        (python-test--marker nested "README.md")
        (should (equal (moyu/python-project-root nested) root)))))

  (ert-deftest python-ext/project-root-prefers-a-nested-python-project ()
    "A Python project nested in a VCS checkout is its own project."
    (python-test--project
      (let ((checkout root)
            (nested (expand-file-name "pkg" root)))
        (make-directory (expand-file-name ".git" checkout))
        (make-directory nested t)
        (python-test--marker nested "pyproject.toml")
        ;; The checkout carries no Python marker of its own, so uv commands
        ;; must run in the nested project.
        (should (equal (moyu/python-project-root nested)
                       (file-name-as-directory nested)))
        (should (file-name-absolute-p (moyu/python-project-root nested))))))

  (ert-deftest python-ext/project-root-widens-to-a-python-checkout-root ()
    "A checkout that is itself a Python project is the project of its members."
    (python-test--project
      (let ((nested (expand-file-name "pkg" root)))
        (make-directory (expand-file-name ".git" root))
        (python-test--marker root "pyproject.toml")
        (make-directory nested t)
        (python-test--marker nested "pyproject.toml")
        (should (equal (moyu/python-project-root nested) root)))))

  ;;; moyu/python-venv-root

  (ert-deftest python-ext/venv-requires-an-interpreter ()
    "A .venv without bin/python is not an environment."
    (python-test--project
      (let ((nested (expand-file-name "pkg" root)))
        (make-directory nested t)
        (make-directory (expand-file-name ".venv" root) t)
        (should-not (moyu/python-venv-root nested))
        (let ((venv (python-test--venv root)))
          (should (equal (moyu/python-venv-root nested) venv))
          ;; The nearest environment wins over an outer one.
          (let ((inner (expand-file-name "inner" nested)))
            (make-directory inner t)
            (let ((inner-venv (python-test--venv inner)))
              (should (equal (moyu/python-venv-root inner) inner-venv))))))))

  (ert-deftest python-ext/venv-root-skips-a-broken-inner-environment ()
    "A .venv without an interpreter must not hide a working one above it."
    (python-test--project
      (let* ((nested (expand-file-name "pkg" root))
             (venv (python-test--venv root)))
        (make-directory (expand-file-name ".venv" nested) t)
        (should (equal (moyu/python-venv-root nested) venv)))))

  (ert-deftest python-ext/remote-directories-have-no-local-environment ()
    "The environment of a remote workspace lives on the remote host."
    (should-not (moyu/python-venv-root "/ssh:host:/srv/app/"))
    (with-temp-buffer
      (let ((default-directory "/ssh:host:/srv/app/"))
        (should-not (moyu/python-environment-program "ruff")))))

  ;;; moyu/python-activate-venv

  (ert-deftest python-ext/activate-venv-sets-the-buffer-environment ()
    (python-test--project
      (let ((venv (python-test--venv root)))
        (with-temp-buffer
          (setq-local default-directory root)
          (should (equal (moyu/python-activate-venv) venv))
          (should (equal python-shell-virtualenv-root venv))
          (should (equal (getenv "VIRTUAL_ENV") venv))
          (should (equal (car exec-path)
                         (moyu/python--venv-bin-directory venv)))))))

  (ert-deftest python-ext/activate-venv-names-the-environment-on-the-mode-line ()
    "The mode line says which environment the buffer uses, and says it once."
    (python-test--project
      (python-test--venv root)
      (with-temp-buffer
        (setq-local default-directory root)
        (moyu/python-activate-venv)
        ;; `.venv' says nothing, so the project's name stands in for it.
        (should (string-match-p "venv:" moyu/python-venv-indicator))
        (should (string-match-p
                 (regexp-quote (file-name-base (directory-file-name root)))
                 moyu/python-venv-indicator))
        (should (memq 'moyu/python-venv-indicator mode-line-misc-info))
        ;; A mode hook can run twice for one buffer; the indicator must not
        ;; end up on the mode line twice.
        (moyu/python-activate-venv)
        (should (= 1 (cl-count 'moyu/python-venv-indicator
                               mode-line-misc-info))))))

  (ert-deftest python-ext/activate-venv-does-not-grow-the-path ()
    "Running the hook twice must not list the environment's bin twice."
    (python-test--project
      (let ((venv (python-test--venv root)))
        (with-temp-buffer
          (setq-local default-directory root)
          (moyu/python-activate-venv)
          (moyu/python-activate-venv)
          (should (= (cl-count (moyu/python--venv-bin-directory venv) exec-path
                               :test #'equal)
                     1))))))

  (ert-deftest python-ext/activate-venv-replaces-the-previous-environment ()
    "Changing project in one buffer must not stack both environments on PATH."
    (python-test--project
      (let* ((other (expand-file-name "other" root))
             (venv-a (python-test--venv root))
             (venv-b (python-test--venv other)))
        (with-temp-buffer
          (setq-local default-directory root)
          (moyu/python-activate-venv)
          (moyu/python-activate-venv other)
          (let ((bin-a (moyu/python--venv-bin-directory venv-a))
                (bin-b (moyu/python--venv-bin-directory venv-b))
                (path (split-string (getenv "PATH") path-separator t)))
            (should (equal (getenv "VIRTUAL_ENV") venv-b))
            (should (equal (car exec-path) bin-b))
            (should-not (member bin-a exec-path))
            (should (= 1 (cl-count bin-b path :test #'equal)))
            (should-not (member bin-a path)))))))

  (ert-deftest python-ext/activate-venv-is-nil-without-an-environment ()
    (python-test--project
      (with-temp-buffer
        (setq-local default-directory root)
        (should-not (moyu/python-activate-venv))
        (should-not (bound-and-true-p python-shell-virtualenv-root)))))

  (ert-deftest python-ext/activate-venv-leaves-remote-directories-alone ()
    "A remote workspace's environment belongs on the remote host."
    (cl-letf (((symbol-function 'moyu/python-venv-root)
               (lambda (&optional _) "/nonexistent")))
      (should-not (moyu/python-activate-venv "/ssh:host:/srv/app"))))

  (ert-deftest python-ext/setup-leaves-the-interpreter-alone ()
    "The interactive shell is the user's choice, not this hook's.
IPython in the environment is there for ob-ipython; `run-python' keeps
whatever `python-shell-interpreter' says."
    (python-test--project
      (let ((venv (python-test--venv root)))
        (dolist (tool '("ipython" "ruff" "jupyter" "ty"))
          (python-test--tool venv tool))
        (with-temp-buffer
          (setq-local default-directory root)
          (python-ts-mode)
          (moyu/python-setup)
          (should-not (local-variable-p 'python-shell-interpreter))))))

  (ert-deftest python-ext/setup-does-nothing-outside-a-python-buffer ()
    "A delayed mode hook can run in a buffer that has changed mode since."
    (python-test--project
      (let ((venv (python-test--venv root)))
        (with-temp-buffer
          (setq-local default-directory root)
          (fundamental-mode)
          (moyu/python-setup)
          (should-not (local-variable-p 'python-shell-virtualenv-root))
          (should-not moyu/python-venv-indicator)
          (should-not (member (moyu/python--venv-bin-directory venv)
                              exec-path))))))

  (ert-deftest python-ext/setup-chooses-the-formatter ()
    "The chain is buffer-local, and it takes the project's ruff."
    (python-test--project
      (let ((venv (python-test--venv root)))
        (python-test--tool venv "ruff")
        (with-temp-buffer
          (setq-local default-directory root)
          (python-ts-mode)
          (cl-letf (((symbol-function 'executable-find) (lambda (_name) nil)))
            (moyu/python-setup))
          (should (local-variable-p 'apheleia-formatter))
          (should (equal apheleia-formatter '(ruff-isort ruff)))))))

  ;;; ob-ipython, for the Org files that ask for it

  (ert-deftest python-ext/org-ipython-ignores-a-buffer-that-is-not-org ()
    "A mode hook can run in a buffer that is no longer in its mode.
Emacs flushes `delayed-mode-hooks' on the next `run-mode-hooks', and by then
the buffer may be in another mode -- so scanning it with the Org parser, which
warns about every keyword-looking line in a buffer that is not Org, is not
something this hook may do."
    (let ((buffer (generate-new-buffer "python-ext-probe.el")))
      (unwind-protect
          (with-current-buffer buffer
            (insert "  #+OB_IPYTHON_KERNEL: a line in an Elisp file\n")
            (emacs-lisp-mode)
            (let (warning)
              (cl-letf (((symbol-function 'display-warning)
                         (lambda (&rest arguments) (setq warning arguments))))
                (moyu/org-ipython-setup))
              (should-not warning)
              (should-not (local-variable-p 'ob-ipython-command))))
        (kill-buffer buffer))))

  (ert-deftest python-ext/org-ipython-is-off-by-default ()
    "An Org file that does not ask keeps ob-python and nothing else.
No jupyter is looked for, and the buffer is left exactly as it was."
    (python-test--project
      (let ((venv (python-test--venv root))
            (buffer nil))
        (python-test--tool venv "jupyter")
        (setq buffer (python-test--org root "plain.org"
                                       "#+begin_src python"
                                       "print(1 + 1)"
                                       "#+end_src"))
        (unwind-protect
            (with-current-buffer buffer
              (moyu/org-ipython-setup)
              (should-not (local-variable-p 'ob-ipython-command))
              (should-not (local-variable-p 'org-babel-default-header-args:ipython)))
          (kill-buffer buffer)))))

  (ert-deftest python-ext/org-ipython-takes-the-project-environment ()
    "`#+OB_IPYTHON: t' points the file at its own .venv, and names a session."
    (python-test--project
      (let ((venv (python-test--venv root))
            (buffer nil))
        (python-test--tool venv "jupyter")
        (setq buffer (python-test--org root "notes.org"
                                       "#+OB_IPYTHON: t"
                                       "#+begin_src ipython"
                                       "print(1 + 1)"
                                       "#+end_src"))
        (unwind-protect
            (with-current-buffer buffer
              (moyu/org-ipython-setup)
              (should (equal ob-ipython-command
                             (moyu/python--venv-program venv "jupyter")))
              (should (equal python-shell-interpreter
                             (moyu/python--venv-program venv "python")))
              ;; ob-ipython requires a session, so the file's name becomes one.
              (should (equal (alist-get :session org-babel-default-header-args:ipython)
                             "notes")))
          (kill-buffer buffer)))))

  (ert-deftest python-ext/org-ipython-honours-kernel-and-session ()
    (python-test--project
      (let ((venv (python-test--venv root))
            (buffer nil))
        (python-test--tool venv "jupyter")
        (setq buffer (python-test--org root "k.org"
                                       "#+OB_IPYTHON: t"
                                       "#+OB_IPYTHON_KERNEL: python3"
                                       "#+OB_IPYTHON_SESSION: shared"))
        (unwind-protect
            (with-current-buffer buffer
              (moyu/org-ipython-setup)
              (should (equal (alist-get :session org-babel-default-header-args:ipython)
                             "shared"))
              (should (equal (alist-get :kernel org-babel-default-header-args:ipython)
                             "python3")))
          (kill-buffer buffer)))))

  (ert-deftest python-ext/org-ipython-takes-a-named-directory ()
    "A directory is taken as an environment, or as a project holding one."
    (python-test--project
      (let ((project (expand-file-name "other" root))
            (buffer nil))
        (make-directory project t)
        (python-test--tool (python-test--venv project) "jupyter")
        (setq buffer (python-test--org root "named.org"
                                       (format "#+OB_IPYTHON: %s" project)))
        (unwind-protect
            (with-current-buffer buffer
              (moyu/org-ipython-setup)
              (should (equal ob-ipython-command
                             (moyu/python--venv-program
                              (expand-file-name ".venv" project) "jupyter"))))
          (kill-buffer buffer)))))

  (ert-deftest python-ext/org-ipython-reports-an-environment-without-jupyter ()
    "An environment that cannot start a kernel leaves the file alone."
    (python-test--project
      (python-test--venv root)
      (let ((buffer nil))
        (setq buffer (python-test--org root "broken.org" "#+OB_IPYTHON: t"))
        (unwind-protect
            (with-current-buffer buffer
              (cl-letf (((symbol-function 'message) (lambda (&rest _) nil)))
                (moyu/org-ipython-setup))
              (should-not (local-variable-p 'ob-ipython-command))
              (should-not (local-variable-p 'org-babel-default-header-args:ipython)))
          (kill-buffer buffer)))))

  (ert-deftest python-ext/org-ipython-environment-falls-back-to-path ()
    "With no environment of its own, the file gets the one on PATH."
    (python-test--project
      (let ((default-directory root))
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (name) (and (member name '("jupyter" "python"))
                                       (concat "/usr/bin/" name)))))
          (should (equal (moyu/org-ipython-environment "t")
                         '("/usr/bin/jupyter" . "/usr/bin/python")))))))

  (ert-deftest python-ext/org-ipython-lift-carries-the-declared-python ()
    "What the file declared is what ob-ipython's client gets, even from a
temporary buffer -- which is where ob-ipython asks, and where the
buffer-local `python-shell-interpreter' cannot reach."
    (require 'python)
    (python-test--project
      (let ((python (python-test--tool (python-test--venv root) "python")))
        (with-temp-buffer
          (setq-local python-shell-interpreter python)
          (should (equal (moyu/org-ipython-lift
                          (lambda () (moyu/org-ipython-get-python (lambda () "fallback"))))
                         python))))))

  (ert-deftest python-ext/org-ipython-keeps-ob-ipythons-own-python-otherwise ()
    "A file that declared nothing leaves ob-ipython's own answer alone."
    (require 'python)
    (with-temp-buffer
      (should (equal (moyu/org-ipython-lift
                      (lambda () (moyu/org-ipython-get-python (lambda () "fallback"))))
                     "fallback"))))

  (ert-deftest python-ext/org-ipython-advise-installs-once ()
    "The bridge is installed on every function it has to cover, and twice is
still once."
    (moyu/org-ipython-advise)
    (moyu/org-ipython-advise)
    (dolist (function '(org-babel-execute:ipython ob-ipython-inspect))
      (should (advice-member-p #'moyu/org-ipython-lift function)))
    (should (advice-member-p #'moyu/org-ipython-get-python
                             'ob-ipython--get-python)))

  (ert-deftest python-ext/org-ipython-edit-buffer-gets-the-environment ()
    "Editing a block happens in a buffer that has no local variables of its
own, so the environment the Org file declared is carried over."
    (let ((source (generate-new-buffer "python-ext-source.org"))
          (edit (generate-new-buffer "python-ext-edit.py")))
      (unwind-protect
          (progn
            (with-current-buffer source
              (setq-local ob-ipython-command "/venv/bin/jupyter")
              (setq-local python-shell-interpreter "/venv/bin/python"))
            (with-current-buffer edit
              (cl-letf (((symbol-function 'org-src-edit-buffer-p)
                         (lambda (&optional _) t))
                        ((symbol-function 'org-src-source-buffer)
                         (lambda () source)))
                (moyu/org-ipython-edit-lift (lambda () nil)))
              (should (equal ob-ipython-command "/venv/bin/jupyter"))
              (should (equal python-shell-interpreter "/venv/bin/python"))))
        (kill-buffer source)
        (kill-buffer edit))))

  (ert-deftest python-ext/org-ipython-edit-buffer-stays-alone-without-a-declaration ()
    "An edit buffer of an Org file that declared nothing keeps the globals."
    (let ((source (generate-new-buffer "python-ext-source.org"))
          (edit (generate-new-buffer "python-ext-edit.py")))
      (unwind-protect
          (with-current-buffer edit
            (cl-letf (((symbol-function 'org-src-edit-buffer-p)
                       (lambda (&optional _) t))
                      ((symbol-function 'org-src-source-buffer)
                       (lambda () source)))
              (moyu/org-ipython-edit-lift (lambda () nil)))
            (should-not (local-variable-p 'ob-ipython-command))
            (should-not (local-variable-p 'python-shell-interpreter)))
        (kill-buffer source)
        (kill-buffer edit))))

  ;;; moyu/python-formatter

  (ert-deftest python-ext/formatter-prefers-ruff ()
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name) (and (equal name "ruff") "/usr/bin/ruff"))))
      (should (equal (moyu/python-formatter) '(ruff-isort ruff))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name) (and (equal name "black") "/usr/bin/black"))))
      (should (eq (moyu/python-formatter) 'black)))
    (cl-letf (((symbol-function 'executable-find) (lambda (_name) nil)))
      (should-not (moyu/python-formatter))))

  (ert-deftest python-ext/formatter-takes-the-environments-ruff ()
    "The project's own ruff is a formatter even when PATH has none."
    (python-test--project
      (let ((venv (python-test--venv root))
            (default-directory root))
        (python-test--tool venv "ruff")
        (cl-letf (((symbol-function 'executable-find) (lambda (_name) nil)))
          (should (equal (moyu/python-formatter) '(ruff-isort ruff)))))))

  ;;; ruff as a flymake backend

  (ert-deftest python-ext/ruff-prefers-the-environments-ruff ()
    (python-test--project
      (let ((venv (python-test--venv root))
            (default-directory root))
        (python-test--tool venv "ruff")
        (should (equal (moyu/python-ruff--program)
                       (moyu/python--venv-program venv "ruff"))))))

  (ert-deftest python-ext/ruff-falls-back-to-a-ruff-on-path ()
    (python-test--project
      (python-test--venv root)
      (let ((default-directory root))
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (name) (and (equal name "ruff") "/usr/bin/ruff"))))
          (should (equal (moyu/python-ruff--program) "/usr/bin/ruff"))))))

  (ert-deftest python-ext/ruff-severity-follows-the-rule ()
    (should (eq (moyu/python-ruff--severity "invalid-syntax") :error))
    (should (eq (moyu/python-ruff--severity "F401") :error))
    (should (eq (moyu/python-ruff--severity "E501") :error))
    (should (eq (moyu/python-ruff--severity "I001") :note))
    (should (eq (moyu/python-ruff--severity "C901") :warning))
    (should (eq (moyu/python-ruff--severity "PERF401") :warning))
    ;; A rule family is matched as a whole: `ERA001' is not `E'+`RA001', and
    ;; neither is `FURB1', `ICN001' or `NPY002' the family its first letter
    ;; would suggest.
    (should (eq (moyu/python-ruff--severity "ERA001") :warning))
    (should (eq (moyu/python-ruff--severity "FURB113") :warning))
    (should (eq (moyu/python-ruff--severity "ICN001") :warning))
    (should (eq (moyu/python-ruff--severity "NPY002") :warning))
    ;; A rule nobody classified is still a diagnostic.
    (should (eq (moyu/python-ruff--severity "XYZ123") :warning)))

  (ert-deftest python-ext/ruff-parses-a-syntax-error ()
    "A syntax error is `invalid-syntax: MESSAGE', with a colon after the rule."
    (require 'flymake)
    (with-temp-buffer
      (insert "def f(:\n    pass\n")
      (let ((diagnostics
             (moyu/python-ruff--diagnostics
              "bad.py:1:7: invalid-syntax: Expected a parameter or the end\n"
              (current-buffer))))
        (should (= (length diagnostics) 1))
        (should (eq (flymake-diagnostic-type (car diagnostics)) :error))
        (should (string-match-p "invalid-syntax"
                                (flymake-diagnostic-text (car diagnostics)))))))

  (ert-deftest python-ext/ruff-parses-concise-output ()
    "One line per finding, with the location resolved against the buffer."
    (require 'flymake)
    (with-temp-buffer
      (insert "import os\nx = 1\n")
      (let ((diagnostics
             (moyu/python-ruff--diagnostics
              (concat "app.py:1:8: F401 [*] `os` imported but unused\n"
                      "app.py:2:1: I001 [*] Import block is un-sorted\n"
                      "app.py:9:1: F821 Undefined name `nope`\n")
              (current-buffer))))
        ;; Three findings; the third names a line the buffer does not have,
        ;; which flymake clamps to the end of it rather than dropping.
        (should (= (length diagnostics) 3))
        (let ((first (car diagnostics)))
          (should (eq (flymake-diagnostic-type first) :error))
          (should (string-match-p "F401" (flymake-diagnostic-text first)))
          ;; Column 8 of line 1 is where `os` starts, and flymake extends
          ;; the region over the symbol at that position.
          (should (= (flymake-diagnostic-beg first) 8))
          (should (> (flymake-diagnostic-end first) 8)))
        (should (eq (flymake-diagnostic-type (cadr diagnostics)) :note))
        (should (eq (flymake-diagnostic-type (caddr diagnostics)) :error)))))

  (ert-deftest python-ext/ruff-location-wins-over-a-lookalike-in-the-message ()
    "A message that repeats `:LINE:COL:' is still the message."
    (require 'flymake)
    (with-temp-buffer
      (insert "import os\n")
      (let ((diagnostics
             (moyu/python-ruff--diagnostics
              "app.py:1:8: F401 `os` imported but unused, also see foo:2:1: X\n"
              (current-buffer))))
        (should (= (length diagnostics) 1))
        (should (= (flymake-diagnostic-beg (car diagnostics)) 8))
        (should (string-match-p "foo:2:1"
                                (flymake-diagnostic-text (car diagnostics)))))))

  (ert-deftest python-ext/ruff-backend-reports-what-ruff-finds ()
    (skip-unless (executable-find "ruff"))
    (python-test--project
      (let ((buffer (generate-new-buffer "python-ext-ruff")))
        (unwind-protect
            (with-current-buffer buffer
              (setq buffer-file-name (expand-file-name "app.py" root))
              (setq-local default-directory root)
              (insert "import os\n")
              (let ((reported 'pending))
                (moyu/python-ruff-backend
                 (lambda (diagnostics) (setq reported diagnostics)))
                (should (consp reported))
                (should (eq (flymake-diagnostic-type (car reported)) :error))
                (should (string-match-p "F401" (flymake-diagnostic-text (car reported))))))
          (kill-buffer buffer)))))

  (ert-deftest python-ext/ruff-backend-reports-a-syntax-error ()
    "A file that does not parse is not a file without findings."
    (skip-unless (executable-find "ruff"))
    (python-test--project
      (let ((buffer (generate-new-buffer "python-ext-ruff-syntax")))
        (unwind-protect
            (with-current-buffer buffer
              (setq buffer-file-name (expand-file-name "bad.py" root))
              (setq-local default-directory root)
              (insert "def f(:\n    pass\n")
              (let ((reported 'pending))
                (moyu/python-ruff-backend
                 (lambda (diagnostics) (setq reported diagnostics)))
                (should (consp reported))
                (should (eq (flymake-diagnostic-type (car reported)) :error))
                (should (string-match-p "invalid-syntax"
                                        (flymake-diagnostic-text (car reported))))))
          (kill-buffer buffer)))))

  (ert-deftest python-ext/ruff-check-reports-a-failing-ruff ()
    "Stderr is not a diagnostic, and a non-zero exit is not a clean file."
    (python-test--project
      (let ((script (expand-file-name "fake-ruff" root)))
        (with-temp-file script
          (insert "#!/bin/sh\necho 'boom' >&2\nexit 2\n"))
        (set-file-modes script #o755)
        (with-temp-buffer
          (insert "import os\n")
          (cl-letf (((symbol-function 'moyu/python-ruff--program)
                     (lambda () script))
                    ((symbol-function 'moyu/python-project-root)
                     (lambda (&optional _) root)))
            (let ((message
                   (condition-case err
                       (progn (moyu/python-ruff--check (current-buffer)) nil)
                     (error (error-message-string err)))))
              (should message)
              (should (string-match-p "boom" message))
              (should (string-match-p "exit 2" message))))))))

  (ert-deftest python-ext/ruff-backend-reports-a-panic-when-the-check-fails ()
    "A broken ruff is not the same thing as a clean file."
    (cl-letf (((symbol-function 'moyu/python-ruff--program)
               (lambda () "/usr/bin/ruff"))
              ((symbol-function 'moyu/python-ruff--check)
               (lambda (_source) (error "Ruff exploded"))))
      (with-temp-buffer
        (let (reported)
          (moyu/python-ruff-backend
           (lambda (&rest arguments) (setq reported arguments)))
          (should (eq (car reported) :panic))
          (should (string-match-p "Ruff exploded"
                                  (plist-get (cdr reported) :explanation)))))))

  (ert-deftest python-ext/flymake-ruff-needs-ty-beside-it ()
    "Ruff's linter is only added when it does not duplicate the server."
    (let ((added nil))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (name) (and (equal name "ruff") "/usr/bin/ruff")))
                ((symbol-function 'add-hook)
                 (lambda (&rest args) (setq added args))))
        (with-temp-buffer
          ;; ruff without ty: eglot falls back to `ruff server', which is
          ;; where these diagnostics come from already.
          (moyu/python-flymake-ruff-load)
          (should-not added)
          ;; ruff beside ty: the servers report different things.
          (cl-letf (((symbol-function 'executable-find) (lambda (_name) "/usr/bin/tool")))
            (moyu/python-flymake-ruff-load)
            (should (equal added
                           (list 'flymake-diagnostic-functions
                                 #'moyu/python-ruff-backend nil t))))))))

  (ert-deftest python-ext/flymake-ruff-is-off-in-remote-buffers ()
    "A local ruff cannot lint a remote workspace's files."
    (let ((added nil))
      (cl-letf (((symbol-function 'executable-find) (lambda (_name) "/usr/bin/tool"))
                ((symbol-function 'add-hook) (lambda (&rest args) (setq added args))))
        (with-temp-buffer
          (let ((default-directory "/ssh:host:/srv/app/"))
            (moyu/python-flymake-ruff-load))
          (should-not added)))))

  ;;; the uv commands

  (ert-deftest python-ext/uv-commands-run-in-the-project-root ()
    (python-test--project
      (let ((nested (expand-file-name "pkg" root))
            (seen nil))
        (make-directory nested t)
        (python-test--marker root "pyproject.toml")
        (cl-letf (((symbol-function 'compile)
                   (lambda (command &optional _comint)
                     (setq seen (cons command default-directory)))))
          (let ((default-directory nested))
            (moyu/python-uv--compile '("sync"))
            (should (equal (car seen) "uv sync"))
            (should (equal (cdr seen) root))
            (moyu/python-uv--compile '("add" "requests>=2"))
            (should (equal (car seen)
                           (format "uv add %s"
                                   (shell-quote-argument "requests>=2")))))))))

  (ert-deftest python-ext/uv-run-splits-the-command-into-arguments ()
    (python-test--project
      (let (seen)
        (cl-letf (((symbol-function 'compile)
                   (lambda (command &optional _comint) (setq seen command)))
                  ((symbol-function 'moyu/python-project-root)
                   (lambda (&optional _) root)))
          (moyu/python-uv-run "pytest -q tests")
          (should (equal seen "uv run pytest -q tests"))))))

  (ert-deftest python-ext/uv-commands-understand-quoted-arguments ()
    "Quoting survives the split, so `-c \"print(1)\"' stays one argument."
    (python-test--project
      (let (seen)
        (cl-letf (((symbol-function 'compile)
                   (lambda (command &optional _comint) (setq seen command)))
                  ((symbol-function 'moyu/python-project-root)
                   (lambda (&optional _) root)))
          (moyu/python-uv-run "python -c \"print(1)\"")
          (should (equal seen
                         (mapconcat #'shell-quote-argument
                                    '("uv" "run" "python" "-c" "print(1)")
                                    " ")))
          (moyu/python-uv-add "\"requests>=2\"")
          (should (equal seen
                         (mapconcat #'shell-quote-argument
                                    '("uv" "add" "requests>=2")
                                    " ")))))))

  (ert-deftest python-ext/uv-run-file-needs-a-file ()
    (with-temp-buffer
      (should-error (moyu/python-uv-run-file) :type 'user-error)))

  (ert-deftest python-ext/uv-map-binds-every-command ()
    (dolist (key '("a" "i" "l" "r" "R" "s"))
      (should (commandp (lookup-key moyu/python-uv-map (kbd key))))))

  ;;; the eglot preference

  (ert-deftest python-ext/eglot-prefers-ty-then-ruff ()
    (should (equal (car moyu/python-eglot-servers) '("ty" "server")))
    (should (equal (cadr moyu/python-eglot-servers) '("ruff" "server"))))

  (ert-deftest python-ext/eglot-contact-activates-before-probing ()
    "A ty that exists only in the project's environment is still found."
    (require 'eglot)
    (python-test--project
      (let ((venv (python-test--venv root))
            (moyu/python-eglot-servers '(("ty" "server") ("ruff" "server"))))
        (python-test--tool venv "ty")
        (with-temp-buffer
          (setq-local default-directory root)
          (should (equal (moyu/python-eglot-contact)
                         (list (moyu/python--venv-program venv "ty")
                               "server")))))))

  (ert-deftest python-ext/eglot-contact-falls-back-to-ruff ()
    "With no ty anywhere, the next server of the preference is used."
    (require 'eglot)
    (let ((moyu/python-eglot-servers '(("ty" "server") ("ruff" "server"))))
      (with-temp-buffer
        (cl-letf (((symbol-function 'eglot--executable-find)
                   (lambda (command &optional _remote)
                     (and (equal command "ruff") "/usr/bin/ruff"))))
          (should (equal (moyu/python-eglot-contact)
                         '("/usr/bin/ruff" "server")))))))

  (ert-deftest python-ext/eglot-contact-probes-the-remote-host ()
    "A remote buffer's server is looked for on the remote host."
    (require 'eglot)
    (let ((moyu/python-eglot-servers '(("ty" "server"))))
      (with-temp-buffer
        (let ((default-directory "/ssh:host:/srv/app/"))
          (cl-letf (((symbol-function 'eglot--executable-find)
                     (lambda (command &optional remote)
                       (and remote (equal command "ty")
                            "/ssh:host:/usr/bin/ty"))))
            (should (equal (moyu/python-eglot-contact)
                           '("/ssh:host:/usr/bin/ty" "server"))))))))

  (ert-deftest python-ext/eglot-registers-the-preferred-servers ()
    "Eglot's own Python entry is replaced by ours, once."
    (require 'eglot)
    (let ((eglot-server-programs eglot-server-programs))
      (moyu/python-register-eglot-servers)
      (moyu/python-register-eglot-servers)
      (should (= 1 (cl-count '(python-mode python-ts-mode)
                             eglot-server-programs
                             :key #'car :test #'equal)))
      (should (= 1 (cl-count #'moyu/python-eglot-contact
                             eglot-server-programs :key #'cdr)))
      (should (eq (cdr (eglot--lookup-mode 'python-ts-mode))
                  #'moyu/python-eglot-contact))))

  (ert-deftest python-ext/eglot-nil-preference-restores-eglot-entry ()
    "A nil preference puts eglot's own Python entry back, not nothing."
    (require 'eglot)
    (let ((builtin (cons '(python-mode python-ts-mode) '("pylsp")))
          (eglot-server-programs
           (list (cons '(python-mode python-ts-mode) '("pylsp"))))
          (moyu/python-eglot-servers '(("ty" "server")))
          (moyu/python--eglot-builtin-entry nil))
      (moyu/python-register-eglot-servers)
      (should (eq (cdr (assoc '(python-mode python-ts-mode)
                              eglot-server-programs))
                  #'moyu/python-eglot-contact))
      (let ((moyu/python-eglot-servers nil))
        (moyu/python-register-eglot-servers))
      (should (equal (assoc '(python-mode python-ts-mode) eglot-server-programs)
                     builtin))
      (should (= 1 (length eglot-server-programs)))))
  )

(provide 'python-ext)
;;; python-ext.el ends here
