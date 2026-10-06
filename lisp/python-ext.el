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
;;   ty    is the language server eglot starts (`ty server'), so it is what
;;         provides navigation, completion and type diagnostics.
;;   ruff  is the linter and the formatter: apheleia runs `ruff check
;;         --select I --fix' and then `ruff format', and flymake runs
;;         `ruff check' beside the type checker through the backend below.
;;
;; This library holds the parts that are more than a `use-package' form:
;;
;;   `moyu/python-project-root'    where the project starts
;;   `moyu/python-venv-root'       the `.venv' uv created there, if any
;;   `moyu/python-setup'           the `python-base-mode-hook' entry point
;;   `moyu/python-uv-map'          `uv' commands, bound to `C-c u' in
;;                                 `python-base-mode-map' by init.org
;;
;; The environment is activated buffer-locally rather than globally: two
;; Python buffers from two projects must not fight over `exec-path', and a
;; remote workspace must keep using the tools of the host that owns its
;; files, which is why every function here stops at `file-remote-p'.
;;
;;; Code:

(require 'cl-lib)
(require 'project)

;; `python.el' defines the first and apheleia the second; neither is loaded
;; when this file is, and the byte compiler should not have to guess.
(defvar apheleia-formatter)
;; `ob-ipython' is loaded with Org, long after this file.
(defvar ob-ipython-command)
(defvar python-shell-virtualenv-root)
(declare-function compile "compile" (command &optional comint))
(declare-function flymake-diag-region "flymake" (buffer line col))
(declare-function flymake-diagnostic-beg "flymake" (diagnostic))
(declare-function flymake-diagnostic-end "flymake" (diagnostic))
(declare-function flymake-diagnostic-text "flymake" (diagnostic))
(declare-function flymake-diagnostic-type "flymake" (diagnostic))
(declare-function flymake-make-diagnostic "flymake"
                  (buffer beg end type text &optional backend))
(declare-function ob-ipython--get-python "ob-ipython" ())
(declare-function ob-ipython-auto-configure-kernels "ob-ipython" (&optional replace))
(declare-function project-root "project" (project))

(defgroup moyu/python nil
  "Editing Python with the Astral toolchain."
  :group 'languages
  :prefix "moyu/python-")

(defcustom moyu/python-uv-program "uv"
  "Name of the uv executable, as found on `exec-path'."
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
Passed to `eglot-alternatives', so a string or a (PROGRAM ARGS...) list.
ty is first because it is the server this configuration installs and
because it is the one that both navigates and type checks; ruff's server
is the fallback for a machine that has ruff but no ty."
  :type '(repeat (choice string (repeat string)))
  :group 'moyu/python)

;;;; ── Where the project and its environment are ─────────────────────────────

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
DIRECTORY defaults to `default-directory'.  A project `project-current'
already knows wins -- uv puts the environment at the root of the checkout
at least as often as at the root of the package -- and otherwise the
nearest ancestor carrying a `.venv' or one of `moyu/python-project-markers'
is used.  The result is a directory name, with its trailing slash."
  (let* ((dir (file-name-as-directory
               (expand-file-name (or directory default-directory))))
         (known (project-current nil dir)))
    (or (and known (file-name-as-directory (project-root known)))
        (when-let* ((found (locate-dominating-file
                            dir #'moyu/python--project-marker-p)))
          (file-name-as-directory found)))))

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

(defun moyu/python-venv-root (&optional directory)
  "Return the virtualenv of the project containing DIRECTORY, or nil.
DIRECTORY defaults to `default-directory'.  The nearest `.venv' with an
interpreter wins, so a nested package that uv initialised on its own is
used rather than an outer environment."
  (let* ((dir (file-name-as-directory
               (expand-file-name (or directory default-directory))))
         (found (locate-dominating-file dir moyu/python-venv-directory)))
    (and found (moyu/python--venv-directory
                (expand-file-name moyu/python-venv-directory found)))))

;;;; ── Activating it ────────────────────────────────────────────────────────

(defun moyu/python-activate-venv (&optional directory)
  "Make the project's virtualenv the one the current buffer uses.
DIRECTORY defaults to `default-directory'.  `python-shell-virtualenv-root'
is set buffer-locally, the environment's bin directory goes to the front of
`exec-path' and PATH, and `VIRTUAL_ENV' is exported -- so that `run-python',
eglot, apheleia and flymake all reach the interpreter and the tools the
project pinned.  Returns the environment, or nil when there is none or
DIRECTORY is remote.

`pyvenv-activate' -- and the `pythonic' that `uv-mode' wraps -- would do this
too, but for the whole session: they set the globals and prepend to the global
PATH, so two projects open at once would take turns pointing `exec-path' at
each other's environment.  Doing it here is what keeps them apart."
  (let ((dir (or directory default-directory)))
    (when (moyu/python-local-directory-p dir)
      (when-let* ((venv (moyu/python-venv-root dir)))
        (let ((bin (moyu/python--venv-bin-directory venv)))
          (setq-local python-shell-virtualenv-root venv)
          ;; A mode hook can run twice for one buffer (a remap, a
          ;; `normal-mode' call); prepending again would grow PATH every time.
          (unless (member bin exec-path)
            (setq-local exec-path (cons bin exec-path))
            (setq-local process-environment
                        (cons (concat "VIRTUAL_ENV=" (directory-file-name venv))
                              (cons (concat "PATH=" bin path-separator
                                            (getenv "PATH"))
                                    process-environment))))
          venv)))))

(defun moyu/python-formatter ()
  "Return the apheleia formatter chain for the current Python buffer.
Ruff sorts the imports and then formats; black is the fallback for a
machine that has no ruff."
  (cond ((executable-find "ruff") '(ruff-isort ruff))
        ((executable-find "black") 'black)
        (t nil)))

(defun moyu/python-environment-program (name)
  "Return program NAME from the project's environment, or nil.
The environment uv made for the project is asked first, so a package the
project declared is the one that is found; callers fall back to
`executable-find' on PATH when this returns nil."
  (when-let* ((venv (moyu/python-venv-root))
              (program (moyu/python--venv-program venv name)))
    (and (file-executable-p program) program)))

;;;; ── ruff as a flymake backend ─────────────────────────────────────────────

(defcustom moyu/python-ruff-arguments
  '("check" "--output-format" "concise" "--exit-zero" "--quiet")
  "Arguments for `ruff', before the file name and the final `-'.
`--exit-zero' keeps a finding from looking like a failure, `--quiet' drops
the summary that would otherwise be parsed as a diagnostic, and concise
output is one `FILE:LINE:COL: CODE MESSAGE' per line."
  :type '(repeat string)
  :group 'moyu/python)

(defconst moyu/python-ruff--regexp
  "\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\([A-Za-z][A-Za-z0-9-]*\\)\\(?: \\[\\*\\]\\)? \\(.*\\)$"
  "Match one line of `ruff check --output-format concise'.
The groups are the file, the line, the column, the rule (which is
`invalid-syntax' for a syntax error) and the message.")

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
A rule that matches nothing is reported as a warning, which is what a
diagnostic nobody classified deserves.")

(defun moyu/python-ruff--program ()
  "Return the ruff to run, or nil.
The project's environment comes first, because that is where a version the
project pinned lives; PATH is the fallback."
  (or (moyu/python-environment-program "ruff")
      (executable-find "ruff")))

(defun moyu/python-ruff--severity (code)
  "Return the flymake severity of ruff rule CODE."
  (or (cdr (cl-find-if (lambda (entry) (string-prefix-p (car entry) code))
                       moyu/python-ruff-severity-alist))
      :warning))

(defun moyu/python-ruff--check (source)
  "Return ruff's concise output for the buffer SOURCE.
The check runs from the project root and is handed SOURCE's file name, so
that ruff finds the same configuration it would for a saved file."
  (let* ((default-directory (or (moyu/python-project-root) default-directory))
         (file (buffer-file-name source))
         (program (moyu/python-ruff--program))
         (arguments (append moyu/python-ruff-arguments
                            (when file (list "--stdin-filename" file))
                            '("-"))))
    (with-temp-buffer
      (insert-buffer-substring source)
      (apply #'call-process-region (point-min) (point-max) program t t nil
             arguments)
      (buffer-string))))

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
what this backend does."
  (let ((program (moyu/python-ruff--program)))
    (if (null program)
        (funcall report-fn :panic :explanation "ruff is not installed")
      (let ((buffer (current-buffer)))
        (funcall report-fn
                 (moyu/python-ruff--diagnostics
                  (moyu/python-ruff--check buffer) buffer))))))

(defun moyu/python-flymake-ruff-load ()
  "Let flymake report ruff's linter in this buffer.
Only when ty is installed as well: with `ty server' as the eglot server the
two report different things, while a fallback to `ruff server' already
carries every ruff diagnostic and would show each of them twice."
  (when (and (moyu/python-ruff--program)
             (or (moyu/python-environment-program "ty")
                 (executable-find "ty")))
    (require 'flymake)
    (add-hook 'flymake-diagnostic-functions #'moyu/python-ruff-backend nil t)))

(defun moyu/python-ob-ipython-setup ()
  "Point ob-ipython at the project's jupyter, or at one on PATH.
`ob-ipython' starts its kernel with `ob-ipython-command', and the environment
the project made comes first, because that is where a package it declared
belongs; a jupyter on PATH is the fallback.  This only chooses the command --
asking it for its kernels is ob-ipython's own `org-mode-hook' entry, which
runs after this one."
  (when-let* ((jupyter (or (moyu/python-environment-program "jupyter")
                           (executable-find "jupyter"))))
    (setq-local ob-ipython-command jupyter)))

(defun moyu/python-ob-ipython-configure-kernels (configure)
  "Run CONFIGURE -- ob-ipython's kernelspec query -- only if it can answer.
CONFIGURE runs `jupyter kernelspec list --json' and reads the answer as JSON;
with no jupyter at all that answer is the empty string, and the JSON error
aborts the rest of `org-mode-hook' in every Org buffer, Python or not.  A
jupyter that is there but broken reports itself when a block is evaluated."
  (when (executable-find (or (bound-and-true-p ob-ipython-command) "jupyter"))
    (funcall configure)))

(defun moyu/python-ob-ipython-get-python (get-python)
  "Return the interpreter ob-ipython should run its client with.
`ob-ipython' asks for `python-shell-interpreter' from inside a temporary
buffer, where a value set for one project's buffer cannot reach it -- so the
client would be run by whatever interpreter the whole session defaults to.
The environment is therefore resolved from `default-directory', which
`with-temp-buffer' does inherit.  Its Python is returned when the environment
is also where the jupyter came from, since the two were installed together
and that is the interpreter which can import `jupyter_client'; GET-PYTHON --
ob-ipython's own, PATH-based answer -- is the fallback for everything else,
including a jupyter that itself came from PATH."
  (or (when (moyu/python-environment-program "jupyter")
        (moyu/python-environment-program "python"))
      (funcall get-python)))

(defun moyu/python-ob-ipython-advise ()
  "Teach ob-ipython two things it cannot work out by itself.
It has no notion of a project environment -- `ob-ipython--get-python' is the
only place it looks, and it looks at `python-shell-interpreter' and
`exec-path' -- so the two adjustments are advices rather than settings:

  `ob-ipython--get-python'           resolve the client's interpreter from
                                     `default-directory', because ob-ipython
                                     asks for it from a temporary buffer,
                                     where a buffer-local value cannot reach
  `ob-ipython-auto-configure-kernels' do not ask for kernelspecs when there
                                     is no jupyter to answer, since the empty
                                     answer is read as JSON and the error
                                     would abort the rest of `org-mode-hook'

Both are idempotent, so a reloaded configuration does not stack them."
  (unless (advice-member-p #'moyu/python-ob-ipython-get-python
                           'ob-ipython--get-python)
    (advice-add 'ob-ipython--get-python :around
                #'moyu/python-ob-ipython-get-python))
  (unless (advice-member-p #'moyu/python-ob-ipython-configure-kernels
                           'ob-ipython-auto-configure-kernels)
    (advice-add 'ob-ipython-auto-configure-kernels :around
                #'moyu/python-ob-ipython-configure-kernels)))

;;;###autoload
(defun moyu/python-setup ()
  "Prepare the current Python buffer for the Astral toolchain.
Added to `python-base-mode-hook': activate the project's uv environment,
choose the formatter, and start ruff's linter when that does not duplicate
what eglot already reports.  The interactive shell is left alone: it is
`python-shell-interpreter' that decides it, and activating the environment is
already what makes that name resolve to the project's own Python."
  (moyu/python-activate-venv)
  (when (boundp 'apheleia-formatter)
    (setq-local apheleia-formatter (moyu/python-formatter)))
  (moyu/python-flymake-ruff-load))

;;;; ── Driving uv ───────────────────────────────────────────────────────────

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
  (moyu/python-uv--compile (cons "add" (split-string packages))))

;;;###autoload
(defun moyu/python-uv-run (command)
  "Run COMMAND in the project environment (`uv run')."
  (interactive "sCommand: ")
  (moyu/python-uv--compile (cons "run" (split-string command))))

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

(provide 'python-ext)

;;;; ── Tests ───────────────────────────────────────────────────────────────
;; Run:  emacs -q --batch -l lisp/python-ext.el --eval "(ert-run-tests-batch-and-exit)"

(with-eval-after-load 'ert

  (defmacro python-test--project (&rest body)
    "Run BODY with `root' bound to an empty temporary project directory."
    (declare (indent 0))
    `(let ((root (file-name-as-directory (make-temp-file "python-ext-test-" t))))
       (unwind-protect (progn ,@body)
         (ignore-errors (delete-directory root t)))))

  (defun python-test--venv (root)
    "Create a minimal .venv under ROOT and return its directory."
    (let* ((venv (expand-file-name ".venv" root))
           (bin (expand-file-name "bin" venv))
           (python (expand-file-name "python" bin)))
      (make-directory bin t)
      (with-temp-file python (insert "#!/bin/sh\n"))
      (set-file-modes python #o755)
      venv))

  (defun python-test--tool (venv name)
    "Create an executable NAME in VENV's bin directory and return its path."
    (let ((file (expand-file-name name (expand-file-name "bin" venv))))
      (with-temp-file file (insert "#!/bin/sh\n"))
      (set-file-modes file #o755)
      file))

  (defun python-test--marker (root name)
    "Create the project marker NAME under ROOT."
    (with-temp-file (expand-file-name name root) (insert "")))

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

  ;;; moyu/python-activate-venv

  (ert-deftest python-ext/activate-venv-sets-the-buffer-environment ()
    (python-test--project
      (let ((venv (python-test--venv root)))
        (with-temp-buffer
          (setq-local default-directory root)
          (should (equal (moyu/python-activate-venv) venv))
          (should (equal python-shell-virtualenv-root venv))
          (should (equal (getenv "VIRTUAL_ENV") venv))
          (should (equal (car exec-path) (expand-file-name "bin" venv)))))))

  (ert-deftest python-ext/activate-venv-does-not-grow-the-path ()
    "Running the hook twice must not list the environment's bin twice."
    (python-test--project
      (let ((venv (python-test--venv root)))
        (with-temp-buffer
          (setq-local default-directory root)
          (moyu/python-activate-venv)
          (moyu/python-activate-venv)
          (should (= (cl-count (expand-file-name "bin" venv) exec-path
                               :test #'equal)
                     1))))))

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

  ;;; moyu/python-ob-ipython-setup

  (ert-deftest python-ext/ob-ipython-uses-the-environments-jupyter ()
    "A project with jupyter in its .venv gets ob-ipython pointed at it."
    (python-test--project
      (let ((venv (python-test--venv root)))
        (python-test--tool venv "jupyter")
        (with-temp-buffer
          (setq-local default-directory root)
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (name) (and (equal name "jupyter")
                                         "/usr/local/bin/jupyter"))))
            (moyu/python-ob-ipython-setup)
            (should (equal ob-ipython-command
                           (expand-file-name "jupyter"
                                             (expand-file-name "bin" venv)))))))))

  (ert-deftest python-ext/ob-ipython-falls-back-to-a-jupyter-on-path ()
    "With no jupyter in the environment, the one on PATH is asked."
    (python-test--project
      (python-test--venv root)
      (with-temp-buffer
        (setq-local default-directory root)
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (name) (and (equal name "jupyter")
                                       "/usr/local/bin/jupyter"))))
          (moyu/python-ob-ipython-setup)
          (should (equal ob-ipython-command "/usr/local/bin/jupyter"))))))

  (ert-deftest python-ext/ob-ipython-asks-for-kernels-only-with-a-jupyter ()
    "The kernelspec query is skipped when there is nothing that can answer.
`jupyter kernelspec list --json' prints nothing without a jupyter, and
ob-ipython reads that empty answer as JSON, which aborts the rest of
`org-mode-hook' in every Org buffer."
    (let* ((called nil)
           (configure (lambda () (setq called t))))
      (cl-letf (((symbol-function 'executable-find) (lambda (_name) nil)))
        (moyu/python-ob-ipython-configure-kernels configure)
        (should-not called))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (name) (and (equal name "jupyter") "/usr/bin/jupyter"))))
        (moyu/python-ob-ipython-configure-kernels configure)
        (should called))))

  (ert-deftest python-ext/ob-ipython-client-uses-the-environments-python ()
    "The client is run by the environment's Python, not the session default.
ob-ipython resolves the interpreter inside a temporary buffer, where a
buffer-local value cannot reach it, so it is resolved from the directory."
    (python-test--project
      (let* ((venv (python-test--venv root))
             (python (expand-file-name "python" (expand-file-name "bin" venv)))
             (default-directory root))
        (python-test--tool venv "jupyter")
        (should (equal (moyu/python-ob-ipython-get-python
                        (lambda () "/usr/bin/python3"))
                       python)))))

  (ert-deftest python-ext/ob-ipython-client-falls-back-outside-a-project ()
    (python-test--project
      (let ((default-directory root))
        (should (equal (moyu/python-ob-ipython-get-python
                        (lambda () "/usr/bin/python3"))
                       "/usr/bin/python3")))))

  (ert-deftest python-ext/ob-ipython-client-falls-back-when-jupyter-is-not-local ()
    "A PATH jupyter needs a PATH client: the environment has no jupyter_client."
    (python-test--project
      (python-test--venv root)
      (let ((default-directory root))
        (should (equal (moyu/python-ob-ipython-get-python
                        (lambda () "/usr/bin/python3"))
                       "/usr/bin/python3")))))

  (ert-deftest python-ext/ob-ipython-leaves-a-project-without-jupyter-alone ()
    "No jupyter anywhere means the command is left as it was."
    (python-test--project
      (python-test--venv root)
      (with-temp-buffer
        (setq-local default-directory root)
        (cl-letf (((symbol-function 'executable-find) (lambda (_name) nil)))
          (moyu/python-ob-ipython-setup)
          (should-not (local-variable-p 'ob-ipython-command))))))

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

  ;;; ruff as a flymake backend

  (ert-deftest python-ext/ruff-prefers-the-environments-ruff ()
    (python-test--project
      (let ((venv (python-test--venv root))
            (default-directory root))
        (python-test--tool venv "ruff")
        (should (equal (moyu/python-ruff--program)
                       (expand-file-name "ruff"
                                         (expand-file-name "bin" venv)))))))

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
    (should (eq (moyu/python-ruff--severity "PERF401") :warning))
    ;; A rule nobody classified is still a diagnostic.
    (should (eq (moyu/python-ruff--severity "XYZ123") :warning)))

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

  (ert-deftest python-ext/uv-run-file-needs-a-file ()
    (with-temp-buffer
      (should-error (moyu/python-uv-run-file) :type 'user-error)))

  (ert-deftest python-ext/uv-map-binds-every-command ()
    (dolist (key '("a" "i" "l" "r" "R" "s"))
      (should (commandp (lookup-key moyu/python-uv-map (kbd key))))))

  ;;; the eglot preference

  (ert-deftest python-ext/eglot-prefers-ty-then-ruff ()
    (should (equal (car moyu/python-eglot-servers) '("ty" "server")))
    (should (equal (cadr moyu/python-eglot-servers) '("ruff" "server")))))

;;; python-ext.el ends here
