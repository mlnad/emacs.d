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
;;         --select I --fix' and then `ruff format', and flymake can run
;;         `ruff check' beside the type checker.
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

;; `python.el' defines the first, apheleia and flymake-ruff the others; none
;; of them is loaded when this file is, and the byte compiler should not have
;; to guess.
(defvar apheleia-formatter)
(defvar flymake-ruff-program)
(defvar python-shell-virtualenv-root)
(declare-function compile "compile" (command &optional comint))
(declare-function flymake-ruff-load "flymake-ruff" ())
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

(defun moyu/python--venv-directory (directory)
  "Return DIRECTORY when it carries a Python interpreter, else nil.
A `.venv' that stopped halfway through being created has no `bin/python',
and pointing `python-shell-virtualenv-root' at one breaks every Python
command with no visible reason."
  (let ((python (expand-file-name
                 (if (eq system-type 'windows-nt)
                     "Scripts/python.exe"
                   "bin/python")
                 directory)))
    (and (file-executable-p python) directory)))

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
DIRECTORY is remote."
  (let ((dir (or directory default-directory)))
    (when (moyu/python-local-directory-p dir)
      (when-let* ((venv (moyu/python-venv-root dir)))
        (let ((bin (expand-file-name
                    (if (eq system-type 'windows-nt) "Scripts" "bin")
                    venv)))
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

(defun moyu/python-flymake-ruff-load ()
  "Let flymake report ruff's linter in this buffer.
Only when ty is installed as well: with `ty server' as the eglot server the
two report different things, while the fallback to `ruff server' already
carries every ruff diagnostic and would show each of them twice.  Does
nothing when flymake-ruff is not installed."
  (when (and (executable-find "ruff")
             (executable-find "ty")
             (fboundp 'flymake-ruff-load))
    ;; The check runs from a temporary buffer, where a buffer-local
    ;; `exec-path' is not in effect; hand it the path this buffer resolved.
    (setq-local flymake-ruff-program (executable-find "ruff"))
    (flymake-ruff-load)))

;;;###autoload
(defun moyu/python-setup ()
  "Prepare the current Python buffer for the Astral toolchain.
Added to `python-base-mode-hook': activate the project's uv environment,
choose the formatter, and start ruff's linter when that does not duplicate
what eglot already reports."
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

  ;;; moyu/python-flymake-ruff-load

  (ert-deftest python-ext/flymake-ruff-needs-ty-beside-it ()
    "Ruff's linter is only added when it does not duplicate the server."
    (let ((loaded nil))
      (cl-letf (((symbol-function 'flymake-ruff-load) (lambda () (setq loaded t)))
                ((symbol-function 'executable-find)
                 (lambda (name) (and (equal name "ruff") "/usr/bin/ruff"))))
        (with-temp-buffer
          ;; ruff without ty: eglot falls back to `ruff server', which is
          ;; where these diagnostics come from already.
          (moyu/python-flymake-ruff-load)
          (should-not loaded)
          ;; ruff beside ty: the servers report different things.
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (_name) "/usr/bin/tool")))
            (moyu/python-flymake-ruff-load)
            (should loaded)
            (should (equal flymake-ruff-program "/usr/bin/tool")))))))

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
