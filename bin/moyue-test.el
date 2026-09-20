;;; moyue-test.el --- ERT test suite for moyue CLI framework -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Run with:  moyue test
;; Or directly:
;;   emacs -q --batch --load bin/moyue.el --load bin/moyue-test.el \
;;         --eval "(ert-run-tests-batch-and-exit)"
;;
;;; Code:

(require 'ert)
(require 'cl-lib)

;; Load the framework under test (path relative to this file)
(let ((moyue-el (expand-file-name "moyue.el" (file-name-directory
                                               (or load-file-name
                                                   buffer-file-name "")))))
  (when (file-exists-p moyue-el)
    (load moyue-el nil 'nomessage)))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Test helpers
;;;; ──────────────────────────────────────────────────────────────────────────

(defmacro moyue-test--with-fresh-registry (&rest body)
  "Execute BODY with a clean, empty command registry, then restore the original."
  `(let ((moyue--commands (make-hash-table :test #'equal)))
     ,@body))

(define-error 'moyue-exit "Moyue process exit (test stub)")

(defmacro moyue-test--capture-messages (&rest body)
  "Execute BODY and return all `message' output as a single string."
  `(let ((msgs '()))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) msgs))))
       ,@body)
     (string-join (nreverse msgs) "\n")))

(defmacro moyue-test--no-kill (&rest body)
  "Execute BODY with `kill-emacs' replaced by a signal of type `moyue-exit'."
  `(cl-letf (((symbol-function 'kill-emacs)
              (lambda (&optional _code) (signal 'moyue-exit nil))))
     ,@body))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Tests: moyue-defcommand (command registration)
;;;; ──────────────────────────────────────────────────────────────────────────

(ert-deftest moyue-test/defcommand-registers-command ()
  "moyue-defcommand stores a command struct in the registry."
  (moyue-test--with-fresh-registry
   (moyue-defcommand "ping" "" "Test command." (ignore args))
   (let ((cmd (gethash "ping" moyue--commands)))
     (should (moyue-command-p cmd))
     (should (equal (moyue-command-name  cmd) "ping"))
     (should (equal (moyue-command-doc   cmd) "Test command."))
     (should (equal (moyue-command-usage cmd) "")))))

(ert-deftest moyue-test/defcommand-records-usage ()
  "moyue-defcommand stores the usage string."
  (moyue-test--with-fresh-registry
   (moyue-defcommand "foo" "[FILE] [OPTIONS]" "Foo command." (ignore args))
   (should (equal (moyue-command-usage (gethash "foo" moyue--commands))
                  "[FILE] [OPTIONS]"))))

(ert-deftest moyue-test/defcommand-fn-receives-args ()
  "The :fn closure receives the remaining args list."
  (moyue-test--with-fresh-registry
   (let (captured)
     (moyue-defcommand "echo-args" "[ARGS...]" "Echo args."
       (setq captured args))
     (funcall (moyue-command-fn (gethash "echo-args" moyue--commands))
              '("a" "b" "c"))
     (should (equal captured '("a" "b" "c"))))))

(ert-deftest moyue-test/defcommand-overwrites-existing ()
  "Re-registering a command name replaces the previous entry."
  (moyue-test--with-fresh-registry
   (moyue-defcommand "dup" "" "First."  (ignore args))
   (moyue-defcommand "dup" "" "Second." (ignore args))
   (should (equal (moyue-command-doc (gethash "dup" moyue--commands)) "Second."))))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Tests: moyue--sorted-commands
;;;; ──────────────────────────────────────────────────────────────────────────

(ert-deftest moyue-test/sorted-commands-alphabetical ()
  "moyue--sorted-commands returns commands in alphabetical order."
  (moyue-test--with-fresh-registry
   (moyue-defcommand "zebra" "" "Z." (ignore args))
   (moyue-defcommand "alpha" "" "A." (ignore args))
   (moyue-defcommand "mango" "" "M." (ignore args))
   (let ((names (mapcar #'moyue-command-name (moyue--sorted-commands))))
     (should (equal names '("alpha" "mango" "zebra"))))))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Tests: moyue-dispatch (command routing)
;;;; ──────────────────────────────────────────────────────────────────────────

(ert-deftest moyue-test/dispatch-calls-registered-command ()
  "Dispatching a known command invokes its handler."
  (moyue-test--with-fresh-registry
   (let (was-called)
     (moyue-defcommand "run" "" "Run it." (setq was-called t))
     (moyue-dispatch '("run"))
     (should was-called))))

(ert-deftest moyue-test/dispatch-passes-args-to-handler ()
  "Extra tokens after the command name are forwarded as `args'."
  (moyue-test--with-fresh-registry
   (let (got-args)
     (moyue-defcommand "capture" "[ARGS]" "Capture." (setq got-args args))
     (moyue-dispatch '("capture" "x" "y"))
     (should (equal got-args '("x" "y"))))))

(ert-deftest moyue-test/dispatch-unknown-command-exits ()
  "Dispatching an unknown command calls kill-emacs."
  (moyue-test--with-fresh-registry
   (moyue-test--no-kill
    (should-error (moyue-dispatch '("no-such-cmd"))
                  :type 'moyue-exit))))

(ert-deftest moyue-test/dispatch-unknown-command-prints-hint ()
  "Dispatching an unknown command prints an error message mentioning the name."
  (moyue-test--with-fresh-registry
   (let ((out ""))
     (moyue-test--no-kill
      (setq out (moyue-test--capture-messages
                 (ignore-errors (moyue-dispatch '("no-such-cmd"))))))
     (should (string-match-p "no-such-cmd" out)))))

(ert-deftest moyue-test/dispatch-nil-argv-defaults-to-help ()
  "Dispatching nil (no arguments) calls the help command."
  (moyue-test--with-fresh-registry
   (let (help-called)
     (moyue-defcommand "help" "" "Help." (setq help-called t))
     (moyue-dispatch nil)
     (should help-called))))

(ert-deftest moyue-test/dispatch-dash-dash-help-routes-to-help ()
  "\"--help\" is treated as the \"help\" command."
  (moyue-test--with-fresh-registry
   (let (help-called)
     (moyue-defcommand "help" "" "Help." (setq help-called t))
     (moyue-dispatch '("--help"))
     (should help-called))))

(ert-deftest moyue-test/dispatch-dash-h-routes-to-help ()
  "\"-h\" is treated as the \"help\" command."
  (moyue-test--with-fresh-registry
   (let (help-called)
     (moyue-defcommand "help" "" "Help." (setq help-called t))
     (moyue-dispatch '("-h"))
     (should help-called))))

(ert-deftest moyue-test/dispatch-catches-handler-errors ()
  "Errors thrown by a command handler are caught, reported, and exit with code 1."
  (moyue-test--with-fresh-registry
   (moyue-defcommand "boom" "" "Explode." (error "intentional error"))
   (moyue-test--no-kill
    (should-error (moyue-dispatch '("boom")) :type 'moyue-exit))))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Tests: moyue--process-argv  (argv stripping)
;;;; ──────────────────────────────────────────────────────────────────────────

(ert-deftest moyue-test/process-argv-strips-dashes ()
  "moyue--process-argv removes the leading \"--\" inserted by Emacs batch mode."
  (should (equal (moyue--process-argv '("--" "cmd" "foo")) '("cmd" "foo"))))

(ert-deftest moyue-test/process-argv-no-op-without-dashes ()
  "moyue--process-argv is a no-op when there is no leading \"--\"."
  (should (equal (moyue--process-argv '("cmd" "bar")) '("cmd" "bar"))))

(ert-deftest moyue-test/process-argv-handles-nil ()
  "moyue--process-argv returns nil when given nil."
  (should (null (moyue--process-argv nil))))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Tests: built-in help command
;;;; ──────────────────────────────────────────────────────────────────────────

(ert-deftest moyue-test/help-lists-all-commands ()
  "The help command output includes names of all registered commands."
  ;; Use the real registry which always has help/tangle/install/test.
  (let ((out (moyue-test--capture-messages (moyue-dispatch '("help")))))
    (should (string-match-p "help"    out))
    (should (string-match-p "tangle"  out))
    (should (string-match-p "install" out))))

(ert-deftest moyue-test/help-shows-command-detail ()
  "\"help COMMAND\" prints the usage and doc for that command."
  (moyue-test--with-fresh-registry
   (moyue-defcommand "help" "[CMD]" "Show help."
     (let* ((cmd-name (car args))
            (cmd (gethash cmd-name moyue--commands)))
       (if cmd
           (message "Usage: moyue %s %s\n\n%s"
                    (moyue-command-name cmd)
                    (moyue-command-usage cmd)
                    (moyue-command-doc   cmd))
         (kill-emacs 1))))
   (moyue-defcommand "greet" "[NAME]" "Print a greeting." (ignore args))
   (let ((out (moyue-test--capture-messages
               (moyue-dispatch '("help" "greet")))))
     (should (string-match-p "greet"   out))
     (should (string-match-p "\\[NAME\\]" out))
     (should (string-match-p "greeting" out)))))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Tests: configuration root + package directory discovery
;;;; ──────────────────────────────────────────────────────────────────────────

(ert-deftest moyue-test/config-root-is-parent-of-bin ()
  "moyue--config-root points at the directory that holds bin/."
  (let ((root (moyue--config-root)))
    (should (file-name-absolute-p root))
    (should (file-directory-p (expand-file-name "bin" root)))
    (should (file-exists-p (expand-file-name "bin/moyue.el" root)))))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Tests: itest distribution handling
;;;; ──────────────────────────────────────────────────────────────────────────

(ert-deftest moyue-test/itest-defaults-to-archlinux ()
  "With no arguments `itest' targets the default distribution."
  (let ((opts (moyue--itest-parse-args nil)))
    (should (equal (plist-get opts :distros) (list moyue--itest-default-distro)))
    (should-not (plist-get opts :rebuild))))

(ert-deftest moyue-test/itest-positional-distro ()
  "A bare positional argument selects that distribution."
  (should (equal (plist-get (moyue--itest-parse-args '("ubuntu")) :distros)
                 '("ubuntu"))))

(ert-deftest moyue-test/itest-distro-equals-form ()
  "\"--distro=NAME\" selects NAME."
  (should (equal (plist-get (moyue--itest-parse-args '("--distro=alpine")) :distros)
                 '("alpine"))))

(ert-deftest moyue-test/itest-distro-separate-value ()
  "\"--distro NAME\" selects NAME."
  (should (equal (plist-get (moyue--itest-parse-args '("--distro" "ubuntu")) :distros)
                 '("ubuntu"))))

(ert-deftest moyue-test/itest-image-alias ()
  "\"--image=NAME\" is accepted as an alias for \"--distro\"."
  (should (equal (plist-get (moyue--itest-parse-args '("--image=alpine")) :distros)
                 '("alpine")))
  (should (equal (plist-get (moyue--itest-parse-args '("--image" "ubuntu")) :distros)
                 '("ubuntu"))))

(ert-deftest moyue-test/itest-rebuild-flag ()
  "\"--rebuild\" and \"-r\" set :rebuild."
  (should (plist-get (moyue--itest-parse-args '("--rebuild")) :rebuild))
  (should (plist-get (moyue--itest-parse-args '("-r" "ubuntu")) :rebuild)))

(ert-deftest moyue-test/itest-all-expands-every-distro ()
  "\"all\" and \"--all\" expand to every supported distribution."
  (should (equal (plist-get (moyue--itest-parse-args '("all")) :distros)
                 (moyue--itest-distro-names)))
  (should (equal (plist-get (moyue--itest-parse-args '("--all")) :distros)
                 (moyue--itest-distro-names))))

(ert-deftest moyue-test/itest-dedupes-distros ()
  "Repeating a distribution keeps a single entry."
  (should (equal (plist-get (moyue--itest-parse-args
                             '("ubuntu" "--distro=ubuntu")) :distros)
                 '("ubuntu"))))

(ert-deftest moyue-test/itest-unknown-distro-signals ()
  "An unsupported distribution is rejected."
  (should-error (moyue--itest-parse-args '("gentoo")) :type 'error))

(ert-deftest moyue-test/itest-unknown-option-signals ()
  "An unknown option is rejected."
  (should-error (moyue--itest-parse-args '("--bogus")) :type 'error))

(ert-deftest moyue-test/itest-missing-value-signals ()
  "An option that requires a value rejects a missing one."
  (should-error (moyue--itest-parse-args '("--distro")) :type 'error))

(ert-deftest moyue-test/itest-image-tags-are-distro-scoped ()
  "Each distribution gets its own image tag."
  (should (equal (moyue--itest-image "archlinux") "moyue-base:archlinux"))
  (should (equal (moyue--itest-image "ubuntu")    "moyue-base:ubuntu"))
  (should (equal (moyue--itest-image "alpine")    "moyue-base:alpine")))

(ert-deftest moyue-test/itest-base-image-mapping ()
  "Each distribution maps to the expected Docker Hub image."
  (should (equal (moyue--itest-base-image "archlinux") "archlinux:latest"))
  (should (equal (moyue--itest-base-image "ubuntu")    "ubuntu:latest"))
  (should (equal (moyue--itest-base-image "alpine")    "alpine:latest"))
  (should-error (moyue--itest-base-image "gentoo") :type 'error))

(ert-deftest moyue-test/itest-distro-names ()
  "The supported distribution list covers the three supported images."
  (should (equal (moyue--itest-distro-names)
                 '("archlinux" "ubuntu" "alpine"))))

(ert-deftest moyue-test/itest-env-args-splits-whitespace ()
  "Extra Docker arguments are split on whitespace."
  (let ((process-environment
         (cons "MOYUE_TEST_ARGS=--network=host  --pull" process-environment)))
    (should (equal (moyue--itest-env-args "MOYUE_TEST_ARGS")
                   '("--network=host" "--pull")))))

(ert-deftest moyue-test/itest-env-args-nil-when-unset-or-empty ()
  "Extra Docker arguments are nil when the variable is unset or empty."
  (let ((process-environment
         (cons "MOYUE_TEST_ARGS_EMPTY=" process-environment)))
    (should-not (moyue--itest-env-args "MOYUE_TEST_ARGS_EMPTY"))
    (should-not (moyue--itest-env-args "MOYUE_TEST_ARGS_UNSET"))))

(ert-deftest moyue-test/itest-build-args ()
  "`docker build' arguments carry the base image, tag and context."
  (let ((args (moyue--itest-build-args "ubuntu" nil)))
    (should (equal (car args) "build"))
    (should (member "--build-arg" args))
    (should (member "BASE_IMAGE=ubuntu:latest" args))
    (should (member "moyue-base:ubuntu" args))
    (should (equal (car (last args)) (moyue--config-root)))
    (should-not (member "--no-cache" args)))
  (should (member "--no-cache" (moyue--itest-build-args "alpine" t))))

(ert-deftest moyue-test/itest-build-args-honors-env ()
  "Extra `docker build' arguments come from MOYUE_ITEST_BUILD_ARGS."
  (let ((process-environment
         (cons "MOYUE_ITEST_BUILD_ARGS=--network=host" process-environment)))
    (should (member "--network=host" (moyue--itest-build-args "alpine" nil)))))

(ert-deftest moyue-test/itest-run-args-mount-source-read-only ()
  "The checkout is mounted read-only at /mnt/emacs.d."
  (should (member (concat (moyue--config-root) ":/mnt/emacs.d:ro")
                  (moyue--itest-run-args "alpine"))))

(ert-deftest moyue-test/itest-run-args-tmpfs-is-executable ()
  "The writable tmpfs is mounted with exec, else bin/moyue cannot run."
  (let ((args (moyue--itest-run-args "alpine")))
    (should (member "--tmpfs" args))
    (should (member "/root/.emacs.d:exec" args))))

(ert-deftest moyue-test/itest-run-args-ends-with-image-and-command ()
  "`docker run' ends with the distro image plus the shell command."
  (let* ((args (moyue--itest-run-args "ubuntu"))
         (tail (last args 4)))
    (should (equal (car args) "run"))
    (should (member "--rm" args))
    (should (equal (nth 0 tail) "moyue-base:ubuntu"))
    (should (equal (nth 1 tail) "sh"))
    (should (equal (nth 2 tail) "-c"))
    (should (equal (nth 3 tail) (moyue--itest-container-command)))))

(ert-deftest moyue-test/itest-run-args-honors-env ()
  "Extra `docker run' arguments come from MOYUE_ITEST_RUN_ARGS."
  (let ((process-environment
         (cons "MOYUE_ITEST_RUN_ARGS=--network=host" process-environment)))
    (should (member "--network=host" (moyue--itest-run-args "alpine")))))

(ert-deftest moyue-test/itest-container-command-copies-and-tests ()
  "The container command copies the read-only tree then installs and tests."
  (let ((cmd (moyue--itest-container-command)))
    (should (string-match-p "tar -C /mnt/emacs.d" cmd))
    (should (string-match-p "--exclude=./elpa" cmd))
    (should (string-match-p "--exclude=./\\.cache" cmd))
    (should (string-match-p "--exclude=./\\.git" cmd))
    ;; Invoked through sh so a noexec tmpfs cannot break the run.
    (should (string-match-p "sh /root/.emacs.d/bin/moyue install" cmd))
    (should (string-match-p "sh /root/.emacs.d/bin/moyue test config" cmd))))

(provide 'moyue-test)
;;; moyue-test.el ends here
