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

(defun moyue--test-configure-packages ()
  "Point package.el at the package directory used by the configuration.
`init.el' installs into elpa/<major>.<minor>/ (see `package-user-dir'),
so on a clean checkout `config-test' would otherwise look in elpa/ and
report every package as missing."
  (let ((init-el (expand-file-name "init.el" (moyue--config-root))))
    (when (file-exists-p init-el)
      (moyue--configure-package-archives-from-init init-el))))

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

(moyue-defcommand "install" ""
                  "Tangle init.org, collect ensured packages, then install from manifest."
                  (require 'org)
                  (let* ((config-root (moyue--config-root))
                         (user-emacs-directory config-root)
                         (default-directory config-root)
                         (init-tangle-src (expand-file-name "init.org" config-root))
                         (init-tangle-dst (expand-file-name "init.el" config-root))
                         (early-init-tangle-dst (expand-file-name "early-init.el" config-root))
                         (installed-packages-manifest-file
                          (expand-file-name ".cache/installed-packages.el" config-root))
                         (package-list nil))
                    (if (file-exists-p init-tangle-src)
                        (progn
                          (message "Step 1/3  Tangling %s ..." init-tangle-src)
                          (org-babel-tangle-file init-tangle-src)
                          (message "Tangle done."))
                      (message "moyue install: init.org not found: %s" init-tangle-src)
                      (kill-emacs 1))
                    (if (file-exists-p init-tangle-dst)
                        (progn
                          (when (file-exists-p early-init-tangle-dst)
                            (message "Step 2/3  Loading %s ..." early-init-tangle-dst)
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
                          (message "Step 3/3  Installing packages with package.el ...")
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
                                       (plist-get result :skipped)))))
                      (message "moyue install: init.el not found after tangle: %s" init-tangle-dst)
                      (kill-emacs 1))))

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

(moyue-defcommand "test" "[SUITE|PATTERN]"
                  "Run test suites. SUITE: all (default), lisp, framework, config. Or pass a regexp PATTERN."
                  (require 'ert)
                  (let* ((suite      (or (car args) "all"))
                         (lisp-dir   (expand-file-name "../lisp/" moyue--bin-dir))
                         (fw-test    (expand-file-name "moyue-test.el"  moyue--bin-dir))
                         (cfg-test   (expand-file-name "config-test.el" moyue--bin-dir))
                         (load-lisp  (lambda ()
                                       (add-to-list 'load-path lisp-dir)
                                       (require 'ert)
                                       ;; Load each lisp source so with-eval-after-load 'ert fires.
                                       (dolist (src (directory-files lisp-dir t "\\.el$"))
                                         (unless (string-match-p "-test\\.el$" src)
                                           (load src nil 'nomessage)))))
                         (load-fw    (lambda () (if (file-exists-p fw-test)
                                                    (load fw-test nil 'nomessage)
                                                  (message "moyue test: not found: %s" fw-test)
                                                  (kill-emacs 1))))
                         (load-cfg   (lambda ()
                                       (moyue--test-configure-packages)
                                       (if (file-exists-p cfg-test)
                                           (load cfg-test nil 'nomessage)
                                         (message "moyue test: not found: %s" cfg-test)
                                         (kill-emacs 1)))))
                    (pcase suite
                      ("lisp"
                       (funcall load-lisp)
                       (ert-run-tests-batch-and-exit "^env-ext/\\|^core/"))
                      ("framework"
                       (funcall load-fw)
                       (ert-run-tests-batch-and-exit t))
                      ("config"
                       (funcall load-cfg)
                       (ert-run-tests-batch-and-exit t))
                      ("all"
                       (funcall load-lisp)
                       (funcall load-fw)
                       (funcall load-cfg)
                       (ert-run-tests-batch-and-exit t))
                      (_
                       ;; Treat as an ERT selector: load all test files then filter by pattern.
                       (funcall load-lisp)
                       (funcall load-fw)
                       (moyue--test-configure-packages)
                       (when (file-exists-p cfg-test) (load cfg-test nil 'nomessage))
                       (ert-run-tests-batch-and-exit (read suite))))))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; itest: Docker-based integration test
;;;; ──────────────────────────────────────────────────────────────────────────

(defconst moyue--itest-distros
  '(("archlinux" . "archlinux:latest")
    ("ubuntu"    . "ubuntu:latest")
    ("alpine"    . "alpine:latest"))
  "Alist of distribution name -> base image used by `moyue itest'.")

(defconst moyue--itest-default-distro "archlinux"
  "Distribution used by `moyue itest' when none is requested.")

(defun moyue--itest-docker-p ()
  "Return non-nil if the `docker' binary is available."
  (executable-find "docker"))

(defun moyue--itest-distro-names ()
  "Return the list of supported distribution names."
  (mapcar #'car moyue--itest-distros))

(defun moyue--itest-resolve-distro (name)
  "Expand NAME into a list of distributions.
NAME may be a supported distribution or \"all\"."
  (cond
   ((equal name "all") (moyue--itest-distro-names))
   ((assoc name moyue--itest-distros) (list name))
   (t (error "unsupported distribution '%s' (supported: %s, all)"
             name (string-join (moyue--itest-distro-names) ", ")))))

(defun moyue--itest-base-image (distro)
  "Return the Docker Hub base image backing DISTRO."
  (or (cdr (assoc distro moyue--itest-distros))
      (error "unsupported distribution '%s' (supported: %s)"
             distro (string-join (moyue--itest-distro-names) ", "))))

(defun moyue--itest-image (distro)
  "Return the integration-test image tag built for DISTRO."
  (format "moyue-base:%s" distro))

(defun moyue--itest-parse-args (args)
  "Parse `moyue itest' ARGS into a property list.
Returns (:distros DISTROS :rebuild BOOL).  A distribution may be given
positionally or via --distro=NAME / --distro NAME (--image is accepted
as an alias); \"all\" selects every supported distribution.  With no
distribution the value of `moyue--itest-default-distro' is used."
  (let ((distros '())
        (rebuild nil)
        (rest args))
    (while rest
      (let ((arg (pop rest)))
        (cond
         ((member arg '("--rebuild" "-r"))
          (setq rebuild t))
         ((member arg '("--all" "-a"))
          (setq distros (append distros (moyue--itest-distro-names))))
         ((string-prefix-p "--distro=" arg)
          (setq distros (append distros
                                (moyue--itest-resolve-distro
                                 (substring arg (length "--distro="))))))
         ((string-prefix-p "--image=" arg)
          (setq distros (append distros
                                (moyue--itest-resolve-distro
                                 (substring arg (length "--image="))))))
         ((member arg '("--distro" "--image"))
          (unless rest (error "%s requires a value" arg))
          (setq distros (append distros (moyue--itest-resolve-distro (pop rest)))))
         ((string-prefix-p "-" arg)
          (error "unknown option '%s'" arg))
         (t
          (setq distros (append distros (moyue--itest-resolve-distro arg)))))))
    (list :distros (delete-dups
                    (or distros (list moyue--itest-default-distro)))
          :rebuild rebuild)))

(defun moyue--itest-env-args (variable)
  "Return the extra Docker arguments stored in environment VARIABLE.
The value is split on whitespace, so callers can inject e.g.
--network=host or proxy settings without editing this file."
  (let ((value (getenv variable)))
    (when value
      (split-string value "[ \t\n]+" t))))

(defun moyue--itest-image-exists-p (image)
  "Return non-nil if Docker image IMAGE (name[:tag]) is present locally."
  (= 0 (call-process "docker" nil nil nil
                     "image" "inspect" "--format={{.Id}}" image)))

(defun moyue--itest-build-args (distro rebuild)
  "Return the `docker build' argument list for DISTRO.
When REBUILD is non-nil add --no-cache."
  (append
   (list "build")
   (when rebuild '("--no-cache"))
   (moyue--itest-env-args "MOYUE_ITEST_BUILD_ARGS")
   (list "--build-arg" (format "BASE_IMAGE=%s" (moyue--itest-base-image distro))
         "-t" (moyue--itest-image distro)
         (moyue--config-root))))

(defun moyue--itest-build (distro rebuild)
  "Build the integration-test image for DISTRO.
When REBUILD is non-nil pass --no-cache to `docker build'."
  (message "itest: building %s (BASE_IMAGE=%s) ..."
           (moyue--itest-image distro)
           (moyue--itest-base-image distro))
  (apply #'call-process "docker" nil t nil
         (moyue--itest-build-args distro rebuild)))

(defun moyue--itest-container-command ()
  "Return the shell command executed inside the integration-test container.
The checkout is mounted read-only at /mnt/emacs.d and copied into the
writable tmpfs at /root/.emacs.d, because `moyue install' has to tangle
init.el and populate .cache/ while keeping the host tree untouched."
  (concat
   ;; Skip the heavy host-only directories while copying.
   "tar -C /mnt/emacs.d --exclude=./elpa --exclude=./.cache "
   "--exclude=./.git -cf - . | tar -C /root/.emacs.d -xf - && "
   ;; The tmpfs is mounted with exec, but go through sh as well so the
   ;; script never depends on its executable bit surviving the copy.
   "sh /root/.emacs.d/bin/moyue install && "
   "sh /root/.emacs.d/bin/moyue test config"))

(defun moyue--itest-run-args (distro)
  "Return the `docker run' argument list for DISTRO."
  (append
   (list "run" "--rm"
         "-v" (concat (moyue--config-root) ":/mnt/emacs.d:ro")
         "--tmpfs" "/root/.emacs.d:exec"
         "--env" "HOME=/root"
         "--env" "EMACS_DIR=/root/.emacs.d")
   (moyue--itest-env-args "MOYUE_ITEST_RUN_ARGS")
   (list (moyue--itest-image distro)
         "sh" "-c" (moyue--itest-container-command))))

(defun moyue--itest-run (distro)
  "Run the integration-test container for DISTRO and return its exit code."
  (apply #'call-process "docker" nil t nil
         (moyue--itest-run-args distro)))

(defun moyue--itest-usage ()
  "Print the `moyue itest' help text."
  (message "Usage: moyue itest [DISTRO|all] [--distro=DISTRO] [--rebuild]\n\n%s\n\n%s"
           (string-join
            (mapcar (lambda (d) (format "  %-10s %s" (car d) (cdr d)))
                    moyue--itest-distros)
            "\n")
           (concat
            "Environment:\n"
            "  MOYUE_ITEST_BUILD_ARGS  extra `docker build' arguments\n"
            "  MOYUE_ITEST_RUN_ARGS    extra `docker run' arguments")))

(moyue-defcommand "itest" "[DISTRO|all] [--distro=DISTRO] [--rebuild]"
                  "Run integration tests inside Docker for one or more distributions."
                  (when (member "--help" args)
                    (moyue--itest-usage)
                    (kill-emacs 0))
                  (unless (moyue--itest-docker-p)
                    (message "moyue itest: `docker' not found in PATH")
                    (kill-emacs 1))
                  (let* ((opts    (moyue--itest-parse-args args))
                         (distros (plist-get opts :distros))
                         (rebuild (plist-get opts :rebuild))
                         (failed  '()))
                    (dolist (distro distros)
                      (let ((image (moyue--itest-image distro))
                            (ok t))
                        (message "\n=== itest: %s (%s) ==="
                                 distro (moyue--itest-base-image distro))
                        (when (or rebuild (not (moyue--itest-image-exists-p image)))
                          (unless (= 0 (moyue--itest-build distro rebuild))
                            (setq ok nil)
                            (message "itest: docker build failed for %s" distro)))
                        (when ok
                          (unless (= 0 (moyue--itest-run distro))
                            (setq ok nil)
                            (message "itest: TESTS FAILED for %s" distro)))
                        (unless ok (push distro failed))))
                    (if failed
                        (progn
                          (message "\nitest: FAILED: %s"
                                   (string-join (nreverse failed) ", "))
                          (kill-emacs 1))
                      (message "\nitest: ALL TESTS PASSED (%s)"
                               (string-join distros ", ")))))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Optional extension: auto-load commands from bin/commands/*.el
;;;; ──────────────────────────────────────────────────────────────────────────

(let ((commands-dir (expand-file-name "commands" moyue--bin-dir)))
  (when (file-directory-p commands-dir)
    (dolist (f (directory-files commands-dir t "\\.el\\'"))
      (load f nil 'nomessage))))

(provide 'moyue)
;;; moyue.el ends here
