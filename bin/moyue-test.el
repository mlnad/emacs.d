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

(provide 'moyue-test)
;;; moyue-test.el ends here
