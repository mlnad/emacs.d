;;; moyue.el --- Moyue CLI Framework -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Liu
;;
;; Author: Liu <liumiaogemini@foxmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (cl-lib "1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; A command-line dispatch framework that turns Emacs into a scriptable CLI.
;; Run with:  moyue COMMAND [ARGS...]
;;
;; Adding a new command is one call to `moyue-defcommand':
;;
;;   (moyue-defcommand "greet" "[NAME]" "Print a greeting."
;;     (message "Hello, %s!" (or (car args) "world")))
;;
;; The variable `args' is automatically bound to the list of remaining
;; arguments passed after the command name.
;;
;;; Code:

(require 'cl-lib)
(require 'package)

;; ERT is loaded lazily, by `moyue test' and by the `doctor' checks, so the
;; compiler cannot see these definitions.  Declaring them keeps the runtime
;; calls in the tool checks from looking like undefined functions.
(declare-function ert-fail "ert" (data))
(declare-function ert-set-test "ert" (symbol definition))
(declare-function ert-skip "ert" (data))
(declare-function make-ert-test "ert" (&rest args))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Core framework
;;;; ──────────────────────────────────────────────────────────────────────────

;; Capture bin/ directory at load time, before load-file-name becomes nil.
(defvar moyue--bin-dir
  (file-name-directory (or load-file-name buffer-file-name ""))
  "Directory containing moyue.el and its companion files.")

(defvar moyue--commands (make-hash-table :test #'equal)
  "Registry mapping command-name strings to `moyue-command' structs.")

(cl-defstruct moyue-command
  "Metadata and handler for a single CLI command."
  (name  "" :type string)   ; command name used on the command line
  (usage "" :type string)   ; short argument synopsis, e.g. "[FILE]"
  (doc   "" :type string)   ; one-line description shown in `help'
  (fn    nil))              ; (lambda (args) ...) where args is a list of strings

(defmacro moyue-defcommand (name usage doc &rest body)
  "Register a CLI command named NAME.

NAME   - string, the subcommand (e.g. \"tangle\")
USAGE  - string, argument synopsis shown in help (e.g. \"[FILE]\")
DOC    - string, one-line description shown in help output
BODY   - Lisp forms that implement the command.
         The variable `args' is bound to the list of remaining CLI arguments.

Example:
  (moyue-defcommand \"greet\" \"[NAME]\" \"Print a greeting.\"
    (message \"Hello, %s!\" (or (car args) \"world\")))"
  (declare (indent 3) (doc-string 3))
  `(puthash ,name
            (make-moyue-command
             :name  ,name
             :usage ,usage
             :doc   ,doc
             :fn    (lambda (args) (ignore args) ,@body))
            moyue--commands))

(defun moyue--sorted-commands ()
  "Return all registered commands sorted alphabetically by name."
  (let (cmds)
    (maphash (lambda (_k v) (push v cmds)) moyue--commands)
    (sort cmds (lambda (a b) (string< (moyue-command-name a)
                                      (moyue-command-name b))))))

(defun moyue-dispatch (argv)
  "Dispatch to a registered command based on ARGV.
ARGV is a list of strings; the first element is the command name
and the rest are its arguments."
  (let* ((raw  (car argv))
         ;; treat --help / -h as the help command
         (name (cond ((member raw '("--help" "-h")) "help")
                     ((null raw)                    "help")
                     (t                              raw)))
         (args (cdr argv))
         (cmd  (gethash name moyue--commands)))
    (if cmd
        (condition-case err
            (funcall (moyue-command-fn cmd) args)
          (error
           (message "moyue %s: error: %s" name (error-message-string err))
           (kill-emacs 1)))
      (message "moyue: unknown command '%s'\n\nRun 'moyue help' to list available commands." name)
      (kill-emacs 1))))

(defun moyue--process-argv (raw-argv)
  "Strip the literal \"--\" separator that Emacs batch mode prepends to RAW-ARGV."
  (if (equal (car raw-argv) "--") (cdr raw-argv) raw-argv))

(defun moyue-main ()
  "Entry point called by the shell wrapper after loading this file.
Reads arguments from `argv' (populated by Emacs batch mode)."
  (moyue-dispatch (moyue--process-argv argv)))

(defun moyue--config-root ()
  "Return the configuration root, i.e. the parent of the bin/ directory."
  (file-name-as-directory (expand-file-name ".." moyue--bin-dir)))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Built-in commands
;;;; ──────────────────────────────────────────────────────────────────────────

(moyue-defcommand "help" "[COMMAND]"
                  "Show help for all commands or for a specific COMMAND."
                  (if args
                      ;; detailed help for one command
                      (let* ((cmd-name (car args))
                             (cmd (gethash cmd-name moyue--commands)))
                        (if cmd
                            (message "Usage: moyue %s %s\n\n%s"
                                     (moyue-command-name cmd)
                                     (moyue-command-usage cmd)
                                     (moyue-command-doc   cmd))
                          (message "moyue help: unknown command '%s'" cmd-name)
                          (kill-emacs 1)))
                    ;; list all commands
                    (let ((lines (list "Usage: moyue COMMAND [ARGS...]\n\nCommands:")))
                      (dolist (cmd (moyue--sorted-commands))
                        ;; Only the first line: a command's docstring may go
                        ;; on to explain itself, and that belongs in
                        ;; `moyue help COMMAND', not in the list.
                        (push (format "  %-16s %s" (moyue-command-name cmd)
                                      (car (split-string (moyue-command-doc cmd) "\n")))
                              lines))
                      (push "\nRun 'moyue help COMMAND' for details on a specific command." lines)
                      (message "%s" (string-join (nreverse lines) "\n")))))

(defun moyue--run-streaming (command)
  "Run COMMAND (a list of strings), forwarding its output; return exit status.
`call-process' with DESTINATION t does not reach stdout in batch mode, so the
child is run with a filter that `princ'es every chunk as it arrives."
  (let ((proc (make-process :name "moyue-child"
                            :command command
                            :buffer nil
                            :noquery t
                            :filter (lambda (_proc chunk) (princ chunk)))))
    (while (accept-process-output proc 1))
    (process-exit-status proc)))

(moyue-defcommand "emacs-src" "[VERSION]"
                  "Download Emacs VERSION sources into .cache/emacs-src (for source-directory)."
                  (let* ((config-root (moyue--config-root))
                         (script      (expand-file-name "bin/install-emacs" config-root))
                         (version     (or (car args) emacs-version))
                         (dest        (file-name-as-directory
                                       (expand-file-name ".cache/emacs-src" config-root))))
                    (unless (file-executable-p script)
                      (message "moyue emacs-src: %s not found or not executable" script)
                      (kill-emacs 1))
                    (message "moyue emacs-src: fetching Emacs %s sources into %s" version dest)
                    (let ((status (moyue--run-streaming
                                   (list script
                                         "--fetch-source-only"
                                         "--version" version
                                         "--source-dir" dest))))
                      (unless (and (integerp status) (zerop status))
                        (message "moyue emacs-src: install-emacs failed (exit %S)" status)
                        (kill-emacs 1)))
                    (message "moyue emacs-src: sources are in %s" dest)
                    (message "Add this to your configuration so M-. finds the sources:")
                    (message "  (setq source-directory %S)" dest)))

(moyue-defcommand "tangle" "[FILE]"
                  "Tangle an Org file (defaults to init.org in user-emacs-directory)."
                  (require 'org)
                  (let ((file (or (car args)
                                  (expand-file-name "init.org" user-emacs-directory))))
                    (if (file-exists-p file)
                        (progn
                          (message "Tangling %s ..." file)
                          (org-babel-tangle-file file)
                          (message "Done."))
                      (message "moyue tangle: file not found: %s" file)
                      (kill-emacs 1))))

(defun moyue--use-package-install-target (form)
  "Return package symbol to install from use-package FORM.
When :ensure is t, install the feature name.
When :ensure is pkg label, install that label."
  (when (and (consp form)
             (eq (car form) 'use-package)
             (symbolp (cadr form)))
    (let* ((feature (cadr form))
           (args (cddr form))
           (ensure nil)
           (cursor args))
      (while (consp cursor)
        (when (eq (car cursor) :ensure)
          (setq ensure (cadr cursor)
                cursor nil))
        (when (consp cursor)
          (setq cursor (cdr cursor))))
      (when ensure
        (let ((ensure ensure))
          (cond
           ((eq ensure t) feature)
           ((or (null ensure) (eq ensure nil)) nil)
           ((symbolp ensure) ensure)
           ((stringp ensure) (intern ensure))
           ((and (consp ensure) (symbolp (car ensure))) (car ensure))
           ((and (consp ensure) (stringp (car ensure))) (intern (car ensure)))
           (t nil)))))))

(defun moyue--walk-form-for-packages (form acc)
  "Collect package symbols from FORM into ACC."
  (let ((target (moyue--use-package-install-target form)))
    (when target (push target acc)))
  (cond
   ((consp form)
    (setq acc (moyue--walk-form-for-packages (car form) acc))
    (setq acc (moyue--walk-form-for-packages (cdr form) acc)))
   ((vectorp form)
    (dotimes (i (length form))
      (setq acc (moyue--walk-form-for-packages (aref form i) acc)))))
  acc)

(defun moyue--collect-install-packages (lisp-file)
  "Parse LISP-FILE and collect packages declared via use-package :ensure."
  (let ((collected '()))
    (with-temp-buffer
      (insert-file-contents lisp-file)
      (goto-char (point-min))
      (let ((done nil))
        (while (not done)
          (condition-case err
              (let ((form (read (current-buffer))))
                (setq collected (moyue--walk-form-for-packages form collected)))
            (end-of-file
             (setq done t))
            (error
             (error "Failed to parse %s near char %d: %s"
                    lisp-file (point) (error-message-string err)))))))
    (nreverse (delete-dups (nreverse collected)))))

(defun moyue--installed-packages-manifest-write (installed-packages-manifest-file packages)
  "Write PACKAGES list to INSTALLED-PACKAGES-MANIFEST-FILE."
  (make-directory (file-name-directory installed-packages-manifest-file) t)
  (with-temp-file installed-packages-manifest-file
    (let ((print-length nil)
          (print-level nil))
      (insert ";; Auto-generated by `moyue install'.\n")
      (prin1 packages (current-buffer))
      (insert "\n"))))

(defun moyue--installed-packages-manifest-read (installed-packages-manifest-file)
  "Read package list from INSTALLED-PACKAGES-MANIFEST-FILE."
  (with-temp-buffer
    (insert-file-contents installed-packages-manifest-file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun moyue--configure-package-archives-from-init (init-tangle-dst)
  "Configure package archives by reusing variable definitions in INIT-TANGLE-DST."
  (unless (boundp 'moyu/package-mirror)
    (setq moyu/package-mirror 'default))
  (with-temp-buffer
    (insert-file-contents init-tangle-dst)
    (goto-char (point-min))
    (let ((done nil))
      (while (not done)
        (let ((form (read (current-buffer))))
          (cond
           ((and (consp form)
                 (eq (car form) 'defvar)
                 (memq (cadr form) '(moyu/available-package-mirrors
                                     moyu/used-package-mirror)))
            (eval form))
           ((and (consp form)
                 (eq (car form) 'setq)
                 (memq 'package-archives (cdr form)))
            (eval form)
            (setq done t))))))))

(defun moyue--configure-package-checks ()
  "Point package.el at the package directory used by the configuration.
`init.el' installs into elpa/<major>.<minor>/ (see `package-user-dir'),
so on a clean checkout `moyue doctor' would otherwise look in elpa/ and
report every package as missing."
  (let ((init-el (expand-file-name "init.el" (moyue--config-root))))
    (when (file-exists-p init-el)
      (moyue--configure-package-archives-from-init init-el))))

(declare-function moyu/env-file "env-ext")
(declare-function moyu/env-generate-file "env-ext")

(defun moyue--ensure-env-file (config-root &optional force)
  "Prepare CONFIG-ROOT/.cache/env.el from the login shell environment.
The file is written only when it does not exist yet, so hand edits made
after the first install survive later installs.  With FORCE non-nil it is
regenerated unconditionally.  Return a cons (FILE . CREATED)."
  (let* ((user-emacs-directory (file-name-as-directory
                                (expand-file-name config-root)))
         (lisp-dir (expand-file-name "lisp" user-emacs-directory)))
    (add-to-list 'load-path lisp-dir)
    (require 'env-ext)
    (let* ((file (moyu/env-file))
           (created (if (or force (not (file-exists-p file)))
                        (progn (moyu/env-generate-file file) t)
                      nil)))
      (cons file created))))

(defun moyue--install-package-list (packages)
  "Install PACKAGES with package.el and return a result plist."
  (unless (bound-and-true-p package--initialized)
    (package-initialize))
  (let ((refreshed nil)
        (installed 0)
        (skipped 0)
        (failed '()))
    (dolist (pkg packages)
      (cond
       ((or (package-installed-p pkg)
            (package-built-in-p pkg))
        (setq skipped (1+ skipped)))
       (t
        (unless refreshed
          (message "Refreshing package archives ...")
          (package-refresh-contents)
          (setq refreshed t))
        (condition-case err
            (progn
              (message "Installing %s ..." pkg)
              (package-install pkg)
              (setq installed (1+ installed)))
          (error
           (push (format "%s: %s" pkg (error-message-string err)) failed))))))
    (list :installed installed
          :skipped skipped
          :failed (nreverse failed))))

(defun moyue--update-package-list (packages)
  "Upgrade PACKAGES with package.el and return a result plist."
  (unless (bound-and-true-p package--initialized)
    (package-initialize))
  (message "Refreshing package archives ...")
  (package-refresh-contents)
  (let ((installed 0)
        (upgraded 0)
        (skipped 0)
        (failed '()))
    (dolist (pkg packages)
      (condition-case err
          (let* ((installed-desc (cadr (assq pkg package-alist)))
                 (archive-desc (cadr (assq pkg package-archive-contents))))
            (cond
             ((package-built-in-p pkg)
              (setq skipped (1+ skipped)))
             ((null installed-desc)
              (message "Installing missing %s ..." pkg)
              (package-install pkg)
              (setq installed (1+ installed)))
             ((and archive-desc
                   (version-list-< (package-desc-version installed-desc)
                                   (package-desc-version archive-desc)))
              (message "Upgrading %s ..." pkg)
              (package-upgrade pkg)
              (setq upgraded (1+ upgraded)))
             (t
              (setq skipped (1+ skipped)))))
        (error
         (push (format "%s: %s" pkg (error-message-string err)) failed))))
    (list :installed installed
          :upgraded upgraded
          :skipped skipped
          :failed (nreverse failed))))

(defun moyue--install-grammars-step (grammars)
  "Report or perform the tree-sitter grammar step of `moyue install'.
With GRAMMARS non-nil every pinned grammar is materialised and compiled; a
checkout that already carries local edits is left untouched.  Without it the
step is only announced, so the default install stays offline and quick."
  (let ((pinned (length (moyue-treesit-languages))))
    (if grammars
        (progn
          (message "Building %d tree-sitter grammars ..." pinned)
          (let ((failed (moyue-treesit-install-grammars)))
            (if failed
                (progn
                  (message "moyue install: %d grammar(s) failed:" (length failed))
                  (dolist (f failed) (message "  %s: %s" (car f) (cdr f)))
                  (kill-emacs 1))
              (message "Tree-sitter grammars built: %d" pinned))))
      (message "Tree-sitter grammars skipped (%d pinned); add --grammars to build them"
               pinned))))

;;;; ── `moyue install' and the tools it can also install ─────────────────────
;; `moyue install TOOL' and `moyue install --tools' are handled by
;; bin/commands/tools.el, which owns the recipe table and the download
;; machinery.  They are reached through the `install' command rather than
;; through a command of their own so that one word installs both the setup
;; and the programs it uses.

(defun moyue--ensure-bin-on-exec-path ()
  "Put <config>/bin on `exec-path' for this run.
`init.el' gives the directory the same place at the front of PATH, so a
check that looks for a tool there matches what the editor itself sees."
  (let ((dir (directory-file-name
              (file-name-as-directory (expand-file-name "bin" (moyue--config-root))))))
    (unless (member dir exec-path)
      (setq exec-path (cons dir exec-path)))))

(defun moyue--install-tool-arguments (args)
  "Return the tool names ARGS asks `moyue install' to install, or nil.
Names are recognised against the recipe table in bin/commands/tools.el, so
`install' keeps installing the configuration itself when no argument names
a tool -- and `moyue install --list' is the only way to ask for that table
without naming a tool.  A word that is neither a flag nor a known tool is
an error, so a typo cannot quietly turn into a full configuration install."
  (cond
   ((null args) nil)
   ((member "--tools" args) t)
   ((member "--list" args) t)
   ((member "--all" args) t)
   (t
    ;; The recipe table is loaded by the time this runs: every `moyue' run
    ;; loads bin/commands/tools.el, and `moyue install TOOL' is the point of
    ;; that table.
    (when (boundp 'moyue-tool-recipes)
      (let ((names nil))
        (dolist (arg args)
          (cond
           ((moyue--tool-known-p arg) (push arg names))
           ((string-prefix-p "-" arg) nil)   ; a flag this command ignores
           (t (error "moyue install: no tool named `%s'.  Known tools: %s"
                     arg (string-join (mapcar #'symbol-name (moyue-tool-ids)) ", ")))))
        (delete-dups (nreverse names)))))))

(defun moyue--tool-known-p (name)
  "Return non-nil when NAME names a recipe.
Recipes are looked up by name rather than by symbol identity, because
interning the same characters twice need not give `eq' symbols."
  (and (boundp 'moyue-tool-recipes)
       (cl-find name moyue-tool-recipes
                :key (lambda (entry) (symbol-name (car entry)))
                :test #'equal)))

(defun moyue--install-tools-run (args tools)
  "Run the tool part of `moyue install ARGS'.
TOOLS is what `moyue--install-tool-arguments' decided: `t' for every
recipe, a list of names for those recipes."
  (require 'tools)
  (cond
   ((member "--list" args)
    (moyue-tools-command '("list")))
   ((eq tools t)
    (moyue-tools--install-named nil (car (moyue-tools--parse-args args))))
   (t
    ;; The names are filtered out of ARGS, which leaves only the flags.
    (moyue-tools--install-named tools (car (moyue-tools--parse-args args))))))

(defun moyue--install-configuration (config-root force-env grammars)
  "Install the configuration rooted at CONFIG-ROOT.
FORCE-ENV regenerates `.cache/env.el' from the login shell and GRAMMARS
also fetches and compiles the pinned tree-sitter grammars."
  (let* ((init-tangle-src (expand-file-name "init.org" config-root))
         (init-tangle-dst (expand-file-name "init.el" config-root))
         (early-init-tangle-dst (expand-file-name "early-init.el" config-root))
         (installed-packages-manifest-file
          (expand-file-name ".cache/installed-packages.el" config-root))
         (package-list nil))
    (if (file-exists-p init-tangle-src)
        (progn
          (message "Step 1/4  Tangling %s ..." init-tangle-src)
          (org-babel-tangle-file init-tangle-src)
          (message "Tangle done."))
      (message "moyue install: init.org not found: %s" init-tangle-src)
      (kill-emacs 1))
    (let ((env-result (moyue--ensure-env-file config-root force-env)))
      (if (cdr env-result)
          (message "Step 2/4  Environment file written: %s" (car env-result))
        (message "Step 2/4  Environment file kept: %s (edit it by hand)"
                 (car env-result))))
    (if (file-exists-p init-tangle-dst)
        (progn
          (when (file-exists-p early-init-tangle-dst)
            (message "Step 3/4  Loading %s ..." early-init-tangle-dst)
            (load-file early-init-tangle-dst))
          (moyue--configure-package-archives-from-init init-tangle-dst)
          (setq package-list (moyue--collect-install-packages init-tangle-dst))
          (when (< emacs-major-version 29)
            (push 'use-package package-list)
            (setq package-list (nreverse (delete-dups (nreverse package-list)))))
          (moyue--installed-packages-manifest-write
           installed-packages-manifest-file package-list)
          (message "Collected %d packages -> %s"
                   (length package-list) installed-packages-manifest-file)
          (message "Step 4/4  Installing packages with package.el ...")
          (let* ((result (moyue--install-package-list package-list))
                 (failed (plist-get result :failed)))
            (if failed
                (progn
                  (message "moyue install: %d packages failed:\n%s"
                           (length failed)
                           (string-join failed "\n"))
                  (kill-emacs 1))
              (message "Packages installed successfully. installed=%d skipped=%d"
                       (plist-get result :installed)
                       (plist-get result :skipped))
              (moyue--install-grammars-step grammars))))
      (message "moyue install: init.el not found after tangle: %s" init-tangle-dst)
      (kill-emacs 1))))

(moyue-defcommand "install" "[TOOL...] [--force-env] [--grammars] [--tools|--list]"
                  "Tangle init.org, prepare the environment file, then install from manifest.
With TOOL arguments, install those external programs instead: each name is
looked up in the recipe table of bin/commands/tools.el and downloaded and
installed for this machine (see `moyue install --list').  `moyue install
--tools' installs every recipe, `--version VERSION' pins a release and
`--dry-run' only prints what would be downloaded.

The environment file `.cache/env.el' is created from the login shell on
the first run only; use --force-env to regenerate it from the shell.
With --grammars, also check out and compile the pinned tree-sitter grammars
into `.cache/tree-sitter/'; a checkout that carries local edits is left
alone rather than reset."
                  (require 'org)
                  (let* ((config-root (moyue--config-root))
                         (user-emacs-directory config-root)
                         (default-directory config-root)
                         (force-env (and (member "--force-env" args) t))
                         (grammars (and (member "--grammars" args) t))
                         (tools (moyue--install-tool-arguments args)))
                    ;; Installing external programs is a different job from
                    ;; setting up the configuration, and the two never mix:
                    ;; naming a tool leaves the elpa state alone, and the
                    ;; default run installs no tools.
                    (if tools
                        (moyue--install-tools-run args tools)
                      (moyue--install-configuration config-root force-env grammars))))
(moyue-defcommand "update" ""
                  "Update packages from `.cache/installed-packages.el`."
                  (let* ((config-root (moyue--config-root))
                         (user-emacs-directory config-root)
                         (default-directory config-root)
                         (init-tangle-dst (expand-file-name "init.el" config-root))
                         (installed-packages-manifest-file
                          (expand-file-name ".cache/installed-packages.el" config-root)))
                    (unless (file-exists-p init-tangle-dst)
                      (message "moyue update: init.el not found: %s" init-tangle-dst)
                      (message "Run `moyue install` first to tangle it.")
                      (kill-emacs 1))
                    (unless (file-exists-p installed-packages-manifest-file)
                      (message "moyue update: package manifest not found: %s"
                               installed-packages-manifest-file)
                      (message "Run `moyue install` first to generate it.")
                      (kill-emacs 1))
                    (moyue--configure-package-archives-from-init init-tangle-dst)
                    (let* ((package-list
                            (moyue--installed-packages-manifest-read
                             installed-packages-manifest-file))
                           (result (moyue--update-package-list package-list))
                           (failed (plist-get result :failed)))
                      (if failed
                          (progn
                            (message "moyue update: %d packages failed:\n%s"
                                     (length failed)
                                     (string-join failed "\n"))
                            (kill-emacs 1))
                        (message "Update finished. installed=%d upgraded=%d skipped=%d"
                                 (plist-get result :installed)
                                 (plist-get result :upgraded)
                                 (plist-get result :skipped))))))

(moyue-defcommand "test" "[PATTERN]"
                  "Run the unit tests (everything but the config/ doctor checks).
PATTERN is an ERT selector and is read as Lisp, so a regexp needs quotes of
its own: moyue test '\"^treesit-\"'.  Without PATTERN, every test outside the
`config/' namespace runs -- the lisp/ helpers, bin/moyue.el and the
bin/commands/ extensions; one test is selected by its name as a symbol."
                  (require 'ert)
                  (let* ((arg        (car args))
                         (lisp-dir   (expand-file-name "../lisp/" moyue--bin-dir))
                         (load-lisp  (lambda ()
                                       (add-to-list 'load-path lisp-dir)
                                       (require 'ert)
                                       ;; Load each lisp source so that its
                                       ;; `with-eval-after-load' test section
                                       ;; fires.  A file that is already in
                                       ;; `load-history' is skipped: its hook
                                       ;; has fired already, and ERT treats a
                                       ;; second definition in batch mode as an
                                       ;; error rather than as a redefinition.
                                       (dolist (src (directory-files lisp-dir t "\\.el$"))
                                         (unless (or (string-match-p "-test\\.el$" src)
                                                     (assoc (expand-file-name src)
                                                            load-history))
                                           (load src nil 'nomessage))))))
                    (funcall load-lisp)
                    (ert-run-tests-batch-and-exit
                     (if (or (null arg) (equal arg "lisp"))
                         '(not "^config/")
                       (read arg)))))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; doctor: configuration integrity checks
;;;; ──────────────────────────────────────────────────────────────────────────
;; Formerly bin/config-test.el.  The checks are registered by a function rather
;; than at load time, so every other subcommand stays cheap and neither pulls
;; in package.el nor reads init.el.

;; Both are defined at run time by the bootstrap inside
;; `moyue--doctor-define-checks', which evals them out of the tangled init.el.
;; Declaring them keeps the byte compiler quiet; binding either one here would
;; make the `boundp'/`fboundp' checks below pass vacuously.
(defvar moyu/face-fonts)
(declare-function moyu/apply-face-fonts "init")

(defmacro moyue--config-defcheck (name doc &rest body)
  "Define a configuration check named NAME with DOC and BODY.
The check is registered under the `config/' namespace for easy filtering:
  moyue doctor config/NAME"
  (declare (indent 2) (doc-string 2))
  `(ert-deftest ,(intern (format "config/%s" name)) ()
     ,doc
     ,@body))

(defmacro moyue--config-check-package (pkg)
  "Assert that PKG is available: installed in elpa OR provided as a built-in."
  `(moyue--config-defcheck ,(intern (format "package-%s" pkg))
     ,(format "Package `%s' should be available (elpa or built-in)." pkg)
     (should (or (package-installed-p ',pkg)
                 (locate-library ,(symbol-name pkg))))))

;;; ── External tool checks ─────────────────────────────────────────────────
;; Emacs runs without any of these, but the configuration delegates real work
;; to outside programs: Magit and `moyue treesit' shell out to git, search
;; falls back to grep, the tree-sitter grammars are compiled with a C
;; toolchain, and eglot starts one language server per major mode.  `moyue
;; doctor' cannot install them, but it can name the one that is missing, say
;; what it is for, and fail when a required one is absent.
;;
;; A requirement is a plist:
;;   :id       symbol, unique, and the tail of the check name
;;   :kind     `tool' for a command-line program, `lsp' for a language server
;;   :commands executable names; the first one found satisfies the check
;;   :required non-nil fails the check when missing, nil skips it instead, so
;;             a minimal machine can still report a clean doctor
;;   :hint     one line, shown when the program is missing
;;   :modes    (lsp only) the major modes that would start this server
;;   :install  (optional) a `moyue install' argument that would provide it;
;;             when nil the tool id is tried, so the entries that
;;             bin/commands/tools.el can install suggest themselves
;;
;; A program that bin/commands/tools.el has a recipe for does not need an
;; entry here at all: `moyue--doctor-merge-tool-recipes' adds one per recipe
;; once that library is loaded, so what can be installed and what doctor
;; checks are always the same list.
;;
;; The names are looked up with `executable-find', i.e. on the `exec-path' of
;; the shell that runs `moyue doctor'; see the `config/env-file-*' checks for
;; the file that gives a desktop-launched Emacs the same PATH.

(defconst moyue-tool-requirements
  '(;; ── Command-line programs ─────────────────────────────────────────────
    (:id git :kind tool :required t
     :commands ("git")
     :hint "Magit, the package archives and `moyue treesit' all shell out to git.")
    (:id grep :kind tool :required t
     :commands ("grep")
     :hint "Search falls back to grep when ripgrep is absent.")
    (:id find :kind tool :required t
     :commands ("find")
     :hint "`consult-find' and `find-dired' run find.")
    (:id ripgrep :kind tool
     :commands ("rg")
     :hint "Preferred search backend; Debian/Ubuntu package: ripgrep.")
    (:id fd :kind tool
     :commands ("fd" "fdfind")
     :hint "Fast file finder; Debian/Ubuntu package: fd-find (binary fdfind).")
    (:id make :kind tool
     :commands ("make" "gmake")
     :hint "Builds the pinned tree-sitter grammars (`moyue install --grammars').")
    (:id cc :kind tool
     :commands ("cc" "gcc" "clang")
     :hint "C compiler for tree-sitter grammars and native compilation.")
    (:id python3 :kind tool
     :commands ("python3" "python")
     :hint "Python shell and babel blocks.")
    (:id poetry :kind tool
     :commands ("poetry")
     :hint "Python virtualenv management for the `poetry' package.")
    (:id latexmk :kind tool
     :commands ("latexmk")
     :hint "AUCTeX latexmk integration.")
    (:id gnuplot :kind tool
     :commands ("gnuplot")
     :hint "Org gnuplot source blocks.")
    (:id docker :kind tool
     :commands ("docker")
     :hint "The `docker' package and `bin/dockemacs'.")
    (:id scheme :kind tool
     :commands ("guile" "racket" "chez" "mit-scheme")
     :hint "A Scheme implementation for geiser.")

    ;; ── Language servers (eglot) ──────────────────────────────────────────
    ;; Only the languages actually edited need a server, so these are all
    ;; optional: a missing one is reported as skipped, never as a failure.
    ;;
    ;; clangd is not listed here: bin/commands/tools.el carries its recipe, so
    ;; `moyue install clangd' provides it and `moyue--doctor-merge-tool-recipes'
    ;; derives the `config/lsp-cpp' check from that recipe.
    (:id python :kind lsp
     :commands ("pylsp" "pyls" "pyright-langserver" "basedpyright-langserver"
                "jedi-language-server")
     :modes (python-ts-mode python-mode)
     :hint "A Python server, e.g. `pip install python-lsp-server' or pyright.")
    (:id go :kind lsp :commands ("gopls")
     :modes (go-ts-mode go-mode)
     :hint "gopls for Go (`go install golang.org/x/tools/gopls@latest').")
    (:id bash :kind lsp :commands ("bash-language-server")
     :modes (bash-ts-mode sh-mode)
     :hint "bash-language-server for shell scripts (`npm i -g bash-language-server').")
    (:id typescript :kind lsp :commands ("typescript-language-server")
     :modes (typescript-ts-mode tsx-ts-mode js-ts-mode typescript-mode)
     :hint "typescript-language-server (`npm i -g typescript-language-server').")
    (:id cmake :kind lsp :commands ("cmake-language-server" "neocmakelsp")
     :modes (cmake-ts-mode)
     :hint "cmake-language-server or neocmakelsp for CMake files.")
    (:id dockerfile :kind lsp
     :commands ("docker-langserver" "docker-language-server")
     :modes (dockerfile-ts-mode dockerfile-mode)
     :hint "docker-langserver for Dockerfiles (`npm i -g dockerfile-language-server-nodejs').")
    (:id java :kind lsp :commands ("jdtls")
     :modes (java-ts-mode java-mode)
     :hint "Eclipse JDT language server (jdtls) for Java.")
    (:id lua :kind lsp :commands ("lua-language-server")
     :modes (lua-ts-mode lua-mode)
     :hint "lua-language-server for Lua.")
    (:id ruby :kind lsp :commands ("ruby-lsp" "solargraph")
     :modes (ruby-ts-mode ruby-mode)
     :hint "ruby-lsp or solargraph for Ruby.")
    (:id php :kind lsp :commands ("intelephense" "phpactor")
     :modes (php-ts-mode php-mode)
     :hint "intelephense or phpactor for PHP.")
    (:id yaml :kind lsp :commands ("yaml-language-server")
     :modes (yaml-ts-mode yaml-mode)
     :hint "yaml-language-server for YAML.")
    (:id toml :kind lsp :commands ("tombi" "taplo")
     :modes (toml-ts-mode conf-toml-mode)
     :hint "tombi or taplo for TOML.")
    (:id json :kind lsp
     :commands ("vscode-json-language-server" "json-languageserver")
     :modes (json-ts-mode json-mode jsonc-mode)
     :hint "vscode-json-language-server for JSON.")
    (:id css :kind lsp :commands ("vscode-css-language-server")
     :modes (css-ts-mode css-mode)
     :hint "vscode-css-language-server for CSS.")
    (:id html :kind lsp :commands ("vscode-html-language-server")
     :modes (html-mode mhtml-mode)
     :hint "vscode-html-language-server for HTML.")
    (:id latex :kind lsp :commands ("texlab" "digestif")
     :modes (latex-mode LaTeX-mode tex-mode)
     :hint "texlab or digestif for LaTeX.")
    (:id markdown :kind lsp :commands ("marksman" "markdown-oe")
     :modes (markdown-mode)
     :hint "marksman for Markdown."))
  "External programs this configuration expects to find on PATH.
Required entries are the few the rest of the configuration assumes; optional
ones skip instead of failing when absent, so a machine that edits only some
languages still gets a clean doctor.  Every entry is checked by
`moyue--tool-check' and named `config/tool-<id>' or `config/lsp-<id>'.")

(defun moyue--tool-executable (requirement)
  "Return the executable name that satisfies REQUIREMENT, or nil.
REQUIREMENT is one entry of `moyue-tool-requirements'; its :commands are
tried in order and the first one on `exec-path' wins."
  (cl-find-if #'executable-find (plist-get requirement :commands)))

(defun moyue--install-hint (requirement)
  "Return how `moyue install' would provide REQUIREMENT, or nil.
The recipe table in bin/commands/tools.el is consulted under the tool's id,
so a program this very command can install is never described as somebody
else's package."
  (or (plist-get requirement :install)
      (and (fboundp 'moyue-tool-recipes)
           (let ((id (plist-get requirement :id)))
             (and (assq id moyue-tool-recipes) (symbol-name id))))))

(defun moyue--doctor-merge-tool-recipes ()
  "Add a `moyue-tool-requirements' entry per recipe in bin/commands/tools.el.
The recipe table lives in bin/commands/ and is therefore loaded after this
file, so the merge happens when `moyue doctor' runs and only then.  An id
that is already in the table is left alone, so the checks are defined once
and a second doctor run in the same Emacs changes nothing."
  (when (fboundp 'moyue-tool-recipes->requirements)
    (dolist (requirement (moyue-tool-recipes->requirements))
      (unless (cl-find (plist-get requirement :id) moyue-tool-requirements
                       :key (lambda (entry) (plist-get entry :id)))
        (setq moyue-tool-requirements
              (append moyue-tool-requirements (list requirement)))))))

(defun moyue--tool-check (requirement)
  "Assert that REQUIREMENT is satisfied, as the body of an ERT check.
A missing program fails the check when :required is non-nil and skips it
otherwise, so an optional tool that is not installed does not turn a minimal
machine's doctor run red.  Either way the :hint says what to install, and
`moyue install' is named when it has a recipe for the program."
  (let* ((id       (plist-get requirement :id))
         (commands (plist-get requirement :commands))
         (hint     (or (plist-get requirement :hint) ""))
         (install  (moyue--install-hint requirement))
         (found    (moyue--tool-executable requirement)))
    (if found
        found
      (let ((detail (format "%s (tried %s).  %s%s"
                            id (string-join commands ", ") hint
                            (if install
                                (format "  Install with `moyue install %s'." install)
                              ""))))
        (if (plist-get requirement :required)
            (ert-fail (concat "required program not found: " detail))
          (ert-skip (concat "optional program not installed: " detail)))))))

(defun moyue--doctor-define-tool-checks ()
  "Register one `config/tool-*' or `config/lsp-*' check per requirement.
The checks are built from `moyue-tool-requirements' at run time, so adding a
program to that table is all it takes to add its check."
  (dolist (requirement moyue-tool-requirements)
    (let* ((kind (plist-get requirement :kind))
           (id   (plist-get requirement :id))
           (name (intern (format "config/%s-%s" kind id))))
      (ert-set-test
       name
       (make-ert-test
        :name name
        :documentation
        (format "%s `%s' should be available on PATH."
                (if (eq kind 'lsp) "The language server for" "The program")
                id)
        :body (lambda () (moyue--tool-check requirement)))))))

(defun moyue--doctor-define-checks ()
  "Register the `config/...' ERT checks used by `moyue doctor'."
  (require 'ert)
  ;; Every program `moyue install' has a recipe for becomes a check too, so
  ;; the doctor run names exactly the tools this configuration can set up.
  (moyue--doctor-merge-tool-recipes)
  ;; package.el is already required by this file; initialize it so that
  ;; `package-installed-p' works.
  (package-initialize)

  ;; Environment checks
  (moyue--config-defcheck emacs-version
    "Emacs version must be at least 28.1."
    (should (version<= "28.1" emacs-version)))

  (moyue--config-defcheck init-org-exists
    "init.org must exist in user-emacs-directory."
    (should (file-exists-p
             (expand-file-name "init.org" user-emacs-directory))))

  (moyue--config-defcheck init-el-exists
    "init.el must exist in user-emacs-directory (tangled from init.org)."
    (should (file-exists-p
             (expand-file-name "init.el" user-emacs-directory))))

  (moyue--config-defcheck elpa-dir-exists
    "The elpa/ package directory must exist."
    (should (file-directory-p
             (expand-file-name "elpa" user-emacs-directory))))

  (moyue--config-defcheck env-file-exists
    "The environment file .cache/env.el must exist (created by `moyue install')."
    (should (file-exists-p
             (expand-file-name ".cache/env.el" user-emacs-directory))))

  (moyue--config-defcheck env-file-is-elisp
    "The environment file must contain readable Emacs Lisp."
    (let ((file (expand-file-name ".cache/env.el" user-emacs-directory)))
      (should (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((done nil))
          (while (not done)
            (condition-case nil
                (read (current-buffer))
              (end-of-file (setq done t))))))))

  ;; External tools: the command-line programs and language servers this
  ;; configuration delegates work to.  They come right after the environment
  ;; checks because a fresh machine is most likely to be missing one of them.
  (moyue--doctor-define-tool-checks)

  ;; Tree-sitter grammars
  ;; The pinned manifest is the single source of truth for which upstream
  ;; revision every grammar is built from, so a malformed pin would silently
  ;; turn the next build into an unreproducible one.  It lives in
  ;; bin/commands/treesit.el, which this file loads along with the other
  ;; commands, so nothing has to be re-read here.
  (moyue--config-defcheck treesit-manifest
    "The pinned tree-sitter manifest must be readable and well formed."
    (should (bound-and-true-p moyue-treesit-manifest))
    (should (consp moyue-treesit-manifest))
    (dolist (entry moyue-treesit-manifest)
      (should (symbolp (car entry)))
      (should (string-match-p "\\`https://" (or (nth 1 entry) "")))
      (should (string-match-p "\\`[0-9a-f]\\{40\\}\\'"
                              (or (plist-get (cddr entry) :commit) "")))
      (should (stringp (plist-get (cddr entry) :source-dir))))
    ;; Every pinned language must be usable by the build path.
    (dolist (lang (moyue-treesit-languages))
      (should (equal (car (moyue-treesit-entry lang)) lang))
      (should (file-name-absolute-p (nth 1 (moyue-treesit-recipe lang))))))

  (moyue--config-defcheck treesit-modes-wired
    "`treesit-enabled-modes' must be set with `setopt', in the tangled init.el.
The variable installs its value through a :set function that fills
`major-mode-remap-alist'.  `setq' never calls a :set function, so a `setq'
leaves that alist untouched while the variable still reads back as t -- the
setting looks applied and no tree-sitter mode is ever enabled.  Nothing
signals this at run time, which is why it is asserted on the source."
    (let ((init-el (expand-file-name "init.el" user-emacs-directory)))
      (should (file-readable-p init-el))
      (with-temp-buffer
        (insert-file-contents init-el)
        (goto-char (point-min))
        (should-not (search-forward "(setq treesit-enabled-modes" nil t))
        (goto-char (point-min))
        (should (search-forward "(setopt treesit-enabled-modes" nil t)))))

  ;; Package installation checks
  ;; Bootstrap / completion framework
  (moyue--config-check-package use-package)
  (moyue--config-check-package vertico)
  (moyue--config-check-package orderless)
  (moyue--config-check-package consult)
  (moyue--config-check-package corfu)
  (moyue--config-check-package cape)
  (moyue--config-check-package marginalia)
  (moyue--config-check-package embark)
  (moyue--config-check-package embark-consult)
  (moyue--config-check-package tempel)

  ;; LSP / development
  (moyue--config-check-package eglot)
  (moyue--config-check-package consult-eglot)
  (moyue--config-check-package apheleia)
  (moyue--config-check-package dape)
  (moyue--config-check-package projection)
  (moyue--config-check-package projection-multi)
  (moyue--config-check-package projection-multi-embark)

  ;; Language support
  (moyue--config-check-package rust-mode)
  (moyue--config-check-package rustic)
  (moyue--config-check-package python)
  (moyue--config-check-package pyimport)
  (moyue--config-check-package poetry)
  (moyue--config-check-package geiser)
  (moyue--config-check-package buttercup)
  (moyue--config-check-package dockerfile-ts-mode)

  ;; Org-mode ecosystem
  (moyue--config-check-package org)
  (moyue--config-check-package org-roam)
  (moyue--config-check-package org-modern)
  (moyue--config-check-package valign)
  (moyue--config-check-package org-fragtog)
  (moyue--config-check-package gnuplot)

  ;; Markdown / TeX
  (moyue--config-check-package markdown-mode)
  (moyue--config-check-package auctex-latexmk)
  (moyue--config-check-package cdlatex)

  ;; Version control
  (moyue--config-check-package magit)
  (moyue--config-check-package magit-todos)
  (moyue--config-check-package diff-hl)

  ;; UI / theme
  (moyue--config-check-package doom-themes)
  (moyue--config-check-package doom-modeline)
  (moyue--config-check-package nerd-icons-corfu)
  (moyue--config-check-package all-the-icons)
  (moyue--config-check-package writeroom-mode)
  (moyue--config-check-package popper)
  (moyue--config-check-package svg-tag-mode)

  ;; Editing
  (moyue--config-check-package evil)
  (moyue--config-check-package which-key)

  ;; Misc
  (moyue--config-check-package rime)
  (moyue--config-check-package docker)
  (moyue--config-check-package aidermacs)

  ;; Font configuration tests
  ;; Bootstrap: load the face-fonts definitions from the tangled init.el so that
  ;; `moyu/face-fonts' and `moyu/apply-face-fonts' are available without loading
  ;; all of init.el (which needs a live display).
  (let ((init-el (expand-file-name "init.el" user-emacs-directory)))
    (when (file-exists-p init-el)
      (with-temp-buffer
        (insert-file-contents init-el)
        (goto-char (point-min))
        ;; Org-babel tangles `:var face-fonts=face-fonts' as a let block; find it.
        (when (search-forward "(let ((face-fonts" nil t)
          (goto-char (match-beginning 0))
          (ignore-errors (eval (read (current-buffer))))))))

  (moyue--config-defcheck face-fonts-structure
    "Every row in `moyu/face-fonts' is a (string string positive-number) triple."
    (should (boundp 'moyu/face-fonts))
    (should (listp moyu/face-fonts))
    (should (> (length moyu/face-fonts) 0))
    (dolist (row moyu/face-fonts)
      (should (= (length row) 3))
      (cl-destructuring-bind (face family size) row
        (should (stringp face))
        (should (and (stringp family) (not (string-empty-p family))))
        (should (and (numberp size) (> size 0))))))

  (moyue--config-defcheck face-fonts-default-row
    "The face-fonts table must contain a `default' row."
    (should (boundp 'moyu/face-fonts))
    (should (cl-find "default" moyu/face-fonts :key #'car :test #'string=)))

  (moyue--config-defcheck face-fonts-cjk-rows
    "face-fonts must contain a single CJK font row."
    (should (boundp 'moyu/face-fonts))
    (should (cl-find "cjk" moyu/face-fonts :key #'car :test #'string=))
    (should (= (length (cl-remove-if-not
                        (lambda (row) (string-prefix-p "cjk" (car row)))
                        moyu/face-fonts))
               1)))

  (moyue--config-defcheck face-fonts-latin-unified
    "The Latin face rows (default, fixed-pitch, fixed-pitch-serif) share one font family."
    (should (boundp 'moyu/face-fonts))
    (let* ((latin-rows (cl-remove-if (lambda (r) (string-prefix-p "cjk" (car r)))
                                     moyu/face-fonts))
           (families (mapcar #'cadr latin-rows)))
      (should (cl-every (lambda (f) (string= f (car families))) families))))

  (moyue--config-defcheck face-fonts-apply-dispatches-correctly
    "`moyu/apply-face-fonts' routes each row to the right handler."
    (should (fboundp 'moyu/apply-face-fonts))
    (let ((latin-calls 0) (cjk-calls 0))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda ()       t))
                ((symbol-function 'set-face-attribute) (lambda (&rest _) (cl-incf latin-calls)))
                ((symbol-function 'set-fontset-font)   (lambda (&rest _) (cl-incf cjk-calls))))
        (moyu/apply-face-fonts))
      ;; default + fixed-pitch + fixed-pitch-serif → set-face-attribute ×3
      (should (= latin-calls 3))
      ;; cjk → set-fontset-font ×4 (han cjk-misc bopomofo kana)
      (should (= cjk-calls 4))))

  (moyue--config-defcheck face-fonts-skipped-without-display
    "`moyu/apply-face-fonts' does nothing when there is no graphical display."
    (should (fboundp 'moyu/apply-face-fonts))
    (let ((called nil))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil))
                ((symbol-function 'set-face-attribute) (lambda (&rest _) (setq called t))))
        (moyu/apply-face-fonts))
      (should-not called))))

(defun moyue--doctor-selector (raw)
  "Turn the RAW argument of `moyue doctor' into an ERT selector.
A word that names a defined test selects that test; any other word is used
as a regexp over the `config/' checks, so `moyue doctor tool' runs the
`config/tool-*' checks and `moyue doctor lsp' the `config/lsp-*' ones.  A
full Lisp selector such as \"(tag slow)\" is read as Lisp.  RAWest nil means
every `config/' check.

Words are anchored at the `config/' namespace on purpose: the unit tests in
`moyue test' are free to use the same words, and a bare word must not drag
them into a configuration check run."
  (if (null raw)
      "^config/"
    (let ((form (condition-case nil (read raw) (error raw))))
      (cond
       ((eq form t) t)
       ((and (symbolp form) (ert-test-boundp form)) form)
       ((symbolp form) (format "^config/.*%s" (symbol-name form)))
       (t form)))))

(moyue-defcommand "doctor" "[SELECTOR]"
                  "Check that the configuration files and the tools they use are complete.
Without SELECTOR every `config/' check runs: files, packages, fonts, the
command-line programs and the eglot language servers.  SELECTOR is an ERT
selector; a word that is not a test name is a regexp, so `moyue doctor tool'
or `moyue doctor lsp' runs one family.  Any check that fails exits non-zero."
                  (moyue--configure-package-checks)
                  ;; Every `moyue' run sees <config>/bin on PATH, because that
                  ;; is where the configuration's own commands live; a tool
                  ;; this run installed must not look missing because the
                  ;; shell that launched Emacs had not learned about it yet.
                  (moyue--ensure-bin-on-exec-path)
                  (moyue--doctor-define-checks)
                  (ert-run-tests-batch-and-exit
                   (moyue--doctor-selector (car args))))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Optional extension: auto-load commands from bin/commands/*.el
;;;; ──────────────────────────────────────────────────────────────────────────


;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Tests
;;;; ──────────────────────────────────────────────────────────────────────────
;; Run with `moyue test'; the `config/...' checks themselves are `moyue
;; doctor's and are not exercised here beyond their construction.

(with-eval-after-load 'ert

  (ert-deftest moyue/tool-requirements-are-well-formed ()
    "Every requirement carries the keys and types the checks read."
    (should (consp moyue-tool-requirements))
    (let ((ids '()))
      (dolist (requirement moyue-tool-requirements)
        (let ((id (plist-get requirement :id)))
          (should (symbolp id))
          (should-not (member id ids))
          (push id ids))
        (should (memq (plist-get requirement :kind) '(tool lsp)))
        (should (memq (plist-get requirement :required) '(t nil)))
        (should (consp (plist-get requirement :commands)))
        (should (cl-every #'stringp (plist-get requirement :commands)))
        (should (stringp (plist-get requirement :hint)))
        (when (eq (plist-get requirement :kind) 'lsp)
          (should (consp (plist-get requirement :modes)))))))

  (ert-deftest moyue/doctor-selector-keeps-bare-words-in-config ()
    "A bare word selects `config/' checks; a test name stays a name."
    (should (equal (moyue--doctor-selector nil) "^config/"))
    (should (eq (moyue--doctor-selector "t") t))
    (should (equal (moyue--doctor-selector "tool") "^config/.*tool"))
    (should (equal (moyue--doctor-selector "\"^config/tool\"")
                   "^config/tool"))
    (should (eq (moyue--doctor-selector
                 "moyue/tool-requirements-are-well-formed")
                'moyue/tool-requirements-are-well-formed)))

  (ert-deftest moyue/tool-executable-takes-the-first-candidate ()
    "The first :commands entry that resolves wins over later ones."
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name) (and (equal name "second") "/bin/second"))))
      (should (equal (moyue--tool-executable
                      '(:id x :commands ("first" "second" "third")))
                     "second"))))

  (ert-deftest moyue/tool-executable-is-nil-when-nothing-resolves ()
    (cl-letf (((symbol-function 'executable-find) (lambda (_name) nil)))
      (should-not (moyue--tool-executable '(:id x :commands ("nope"))))))

  (ert-deftest moyue/tool-check-passes-when-the-program-is-found ()
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name) (and (equal name "git") "/bin/git"))))
      (should (moyue--tool-check
               '(:id git :kind tool :required t
                 :commands ("git") :hint "needed")))))

  (ert-deftest moyue/tool-check-fails-a-missing-required-program ()
    (cl-letf (((symbol-function 'executable-find) (lambda (_name) nil)))
      (should-error (moyue--tool-check
                     '(:id git :kind tool :required t
                       :commands ("git") :hint "needed"))
                    :type 'ert-test-failed)))

  (ert-deftest moyue/tool-check-skips-a-missing-optional-program ()
    (cl-letf (((symbol-function 'executable-find) (lambda (_name) nil)))
      (should-error (moyue--tool-check
                     '(:id rg :kind tool :required nil
                       :commands ("rg") :hint "nice to have"))
                    :type 'ert-test-skipped)))

  (ert-deftest moyue/doctor-defines-a-check-per-requirement ()
    "The table and the registered `config/...' tests stay in step."
    (moyue--doctor-define-tool-checks)
    (dolist (requirement moyue-tool-requirements)
      (let ((name (intern (format "config/%s-%s"
                                  (plist-get requirement :kind)
                                  (plist-get requirement :id)))))
        (should (ert-get-test name))))))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Optional extension: load the commands from bin/commands/*.el
;;;; ──────────────────────────────────────────────────────────────────────────
;; This has to come last: those files register their commands with
;; `moyue-defcommand', which is defined above, and they are loaded on every
;; `moyue' run so that `moyue install TOOL' can consult the recipe table.

(let ((commands-dir (expand-file-name "commands" moyue--bin-dir)))
  (when (file-directory-p commands-dir)
    (dolist (f (directory-files commands-dir t "\\.el\\'"))
      (load f nil 'nomessage))))

(provide 'moyue)
;;; moyue.el ends here
