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
        (push (format "  %-16s %s" (moyue-command-name cmd)
                      (moyue-command-doc cmd))
              lines))
      (push "\nRun 'moyue help COMMAND' for details on a specific command." lines)
      (message "%s" (string-join (nreverse lines) "\n")))))

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

(moyue-defcommand "install" ""
  "Install packages declared in init.el via use-package."
  (let ((init-el (expand-file-name "init.el" user-emacs-directory)))
    (if (file-exists-p init-el)
        (progn
          (message "Loading %s to install packages ..." init-el)
          (load-file init-el)
          (message "Packages installed successfully."))
      (message "moyue install: init.el not found: %s" init-el)
      (kill-emacs 1))))

(moyue-defcommand "test" "[SUITE|PATTERN]"
  "Run test suites. SUITE: all (default), framework, config. Or pass a regexp PATTERN."
  (require 'ert)
  (let* ((suite      (or (car args) "all"))
         (fw-test    (expand-file-name "moyue-test.el"  moyue--bin-dir))
         (cfg-test   (expand-file-name "config-test.el" moyue--bin-dir))
         (load-fw    (lambda () (if (file-exists-p fw-test)
                               (load fw-test nil 'nomessage)
                             (message "moyue test: not found: %s" fw-test)
                             (kill-emacs 1))))
         (load-cfg   (lambda () (if (file-exists-p cfg-test)
                                (load cfg-test nil 'nomessage)
                              (message "moyue test: not found: %s" cfg-test)
                              (kill-emacs 1)))))
    (pcase suite
      ("framework"
       (funcall load-fw)
       (ert-run-tests-batch-and-exit t))
      ("config"
       (funcall load-cfg)
       (ert-run-tests-batch-and-exit t))
      ("all"
       (funcall load-fw)
       (funcall load-cfg)
       (ert-run-tests-batch-and-exit t))
      (_
       ;; Treat as an ERT selector: load all test files then filter by pattern.
       (funcall load-fw)
       (when (file-exists-p cfg-test) (load cfg-test nil 'nomessage))
       (ert-run-tests-batch-and-exit (read suite))))))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Optional extension: auto-load commands from bin/commands/*.el
;;;; ──────────────────────────────────────────────────────────────────────────

(let ((commands-dir (expand-file-name "commands" moyue--bin-dir)))
  (when (file-directory-p commands-dir)
    (dolist (f (directory-files commands-dir t "\\.el\\'"))
      (load f nil 'nomessage))))

(provide 'moyue)
;;; moyue.el ends here
