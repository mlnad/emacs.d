;;; templates.el --- Tempel template validation for `moyue check-templates' -*- lexical-binding: t; -*-

;;; Commentary:

;; Tempel reads its templates from `tempel-path' -- by default the single file
;; ~/.emacs.d/templates -- as plain Lisp data: the whole file, wrapped in one
;; pair of parentheses, must read as a single list whose elements are mode
;; symbols, plist keywords with a value, and template lists, and every template
;; group must name at least one mode.
;;
;; A file that breaks that format is never diagnosed by Tempel itself.  A group
;; that names no mode is dropped in silence, and an element the reader cannot
;; consume -- a stray atom between two groups, say -- makes `tempel--file-read'
;; spin forever.  This command reads a file exactly the way Tempel does and
;; names the file and the offending form instead.
;;
;;   moyue check-templates                    # the configured templates file
;;   moyue check-templates FILE [FILE...]     # explicit files
;;   moyue check-templates DIR                # every *.eld file directly inside
;;
;; The last form is the layout of Crandel/tempel-collection, so a checkout of it
;; below .cache/ can be validated whole.  Two checks go past the collection's
;; scripts/check-templates.el: data left after the first Lisp form (which is
;; what unbalanced parentheses look like) is an error instead of being ignored,
;; and a template name defined twice for one mode is reported, because
;; `tempel-expand' can only ever reach the first of the two definitions.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun moyue-templates-default-file ()
  "Return the template file `moyue check-templates' validates by default.
It is the file next to the configuration this command was run from, which is
also what a stock `tempel-path' points at."
  (expand-file-name "templates" (moyue--config-root)))

(define-error 'moyue-templates-trailing-data
  "Data after the first Lisp form (unbalanced parentheses?)")

(defun moyue-templates--read-data (file)
  "Read FILE the way `tempel--file-read' does and return its top-level list.
Signal an error when FILE cannot be read or its contents are not one balanced
Lisp list, and `moyue-templates-trailing-data' when data follows that list."
  (with-temp-buffer
    (insert "(\n")
    (insert-file-contents file)
    (goto-char (point-max))
    (insert "\n)")
    (goto-char (point-min))
    (prog1 (read (current-buffer))
      ;; `read' stops at the end of the first form, so anything left over means
      ;; the file closed the list early.  Tempel ignores that tail; a checker
      ;; should not, since the rest of the file would then never be loaded.
      (forward-comment (point-max))
      (unless (eobp)
        (signal 'moyue-templates-trailing-data nil)))))

(defun moyue-templates--validate-data (data)
  "Return the format errors of the top-level template list DATA."
  (let ((errors nil)
        (seen (make-hash-table :test #'equal)))
    (while data
      (let ((modes nil) (templates nil) (before data))
        (while (and (car data) (symbolp (car data)) (not (keywordp (car data))))
          (push (pop data) modes))
        (while (keywordp (car data))
          (let ((keyword (pop data)))
            (if (consp data)
                (pop data)
              (push (format "dangling keyword %S (missing value)" keyword)
                    errors))))
        (while (consp (car data))
          (push (pop data) templates))
        (setq modes (nreverse modes)
              templates (nreverse templates))
        ;; One name, one mode, one definition: a second one is unreachable.
        (dolist (template templates)
          (dolist (mode modes)
            (let ((key (cons mode (car template))))
              (if (gethash key seen)
                  (push (format "template %S is defined twice for mode %S (only the first definition is reachable)"
                                (car template) mode)
                        errors)
                (puthash key t seen)))))
        (cond
         ;; Nothing above could consume the element, so `tempel--file-read'
         ;; would spin on it forever.  Always advance, whatever it is.
         ((eq before data)
          (push (format "unexpected element %S (must be a mode symbol, a plist keyword or a template list)"
                        (car data))
                errors)
          (pop data))
         ;; A group that names no mode loses the templates it defines.  A
         ;; group of keywords alone defines nothing and is harmless there.
         ((and (null modes) templates)
          (push "template group without a mode (its templates are silently dropped)"
                errors)))))
    (nreverse errors)))

(defun moyue-templates-validate-file (file)
  "Return the format errors that keep FILE from being valid template data.
An empty list means FILE is readable and satisfies the format that
`tempel--file-read' and `tempel-path-templates' expect."
  (cond
   ((not (file-exists-p file)) (list "no such file or directory"))
   ((file-directory-p file)    (list "is a directory"))
   (t
    (let ((result (condition-case err
                      (cons 'ok (moyue-templates--read-data file))
                    (error (cons (car err) (error-message-string err))))))
      (pcase (car result)
        ('ok
         (let ((data (cdr result)))
           (cond
            ((null data)        (list "file contains no templates"))
            ((not (listp data)) (list "top-level form is not a list"))
            (t (moyue-templates--validate-data data)))))
        ('moyue-templates-trailing-data (list (cdr result)))
        (_ (list (format "unreadable Lisp data: %s" (cdr result)))))))))

(defun moyue-templates--files (args)
  "Return the files named by the command-line ARGS.
Without ARGS the default template file is returned.  A directory ARG
contributes every *.eld file directly inside it, and is an error when it
holds none."
  (if (null args)
      (list (moyue-templates-default-file))
    (cl-loop for arg in args
             for path = (expand-file-name arg)
             append (cond
                     ((not (file-directory-p path)) (list path))
                     ((directory-files path t "\\.eld\\'"))
                     (t (error "No *.eld template files in %s" path))))))

(defun moyue-templates-check (files)
  "Validate FILES, print one line per file plus a summary.
Return the number of files that failed."
  (let ((failures 0))
    (dolist (file files)
      (let ((errors (moyue-templates-validate-file file)))
        (if (null errors)
            (message "ok   %s" file)
          (setq failures (1+ failures))
          (dolist (error errors)
            (message "FAIL %s: %s" file error)))))
    (message "Checked %d file(s), %d failure(s)" (length files) failures)
    failures))

(defun moyue-templates--help ()
  "Print the usage and documentation of the `check-templates' command."
  (let ((cmd (gethash "check-templates" moyue--commands)))
    (message "Usage: moyue check-templates %s\n\n%s"
             (moyue-command-usage cmd) (moyue-command-doc cmd))))

(moyue-defcommand "check-templates" "[FILE|DIR...]"
  "Validate Tempel template data files; exits non-zero on a broken one.

Tempel reads a template file as plain Lisp data: the whole file, wrapped in
one pair of parentheses, must read as a single list whose elements are mode
symbols, plist keywords with a value, and template lists, and no template
group may leave out its mode.  Tempel does not diagnose a file that breaks
that format -- a group without a mode is dropped in silence and a stray atom
hangs its reader -- so this command reads the file the same way and names
the offending form instead.

FILE is validated as a template file.  DIR contributes every *.eld file
directly inside it, which is the layout of Crandel/tempel-collection.
Without an argument the configured template file is checked, i.e. the
`templates' file next to this configuration."
  (if (member (car args) '("help" "--help" "-h"))
      (moyue-templates--help)
    (let ((failures (moyue-templates-check (moyue-templates--files args))))
      (when (> failures 0)
        (kill-emacs 1)))))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Tests

(defun moyue-templates--test-errors (content)
  "Validate CONTENT as a template file and return the errors it produces."
  (let ((file (make-temp-file "moyue-templates-test" nil ".eld")))
    (unwind-protect
        (progn
          (with-temp-file file (insert content))
          (moyue-templates-validate-file file))
      (delete-file file))))

(with-eval-after-load 'ert

  (ert-deftest templates/accepts-modes-templates-and-plists ()
    "A well-formed file passes, including group plists and comments."
    (should-not (moyue-templates--test-errors
                 (mapconcat #'identity
                            '(";; a comment"
                              "fundamental-mode ;; everywhere"
                              ""
                              "(tdy (format-time-string \"%Y-%m-%d\"))"
                              ""
                              "org-mode"
                              ""
                              "(src & \"#+begin_src \" p n r n \"#+end_src\""
                              " :post (org-edit-src-code))"
                              ""
                              "text-mode"
                              ""
                              ":when (derived-mode-p 'text-mode)"
                              ""
                              "(box \"+\" p \"+\")")
                            "\n"))))

  (ert-deftest templates/accepts-a-multi-mode-group ()
    "Consecutive mode symbols are one group, as in the collection."
    (should-not (moyue-templates--test-errors
                 "rust-mode rust-ts-mode\n\n(fn \"fn \" p \"()\")\n")))

  (ert-deftest templates/rejects-a-group-without-a-mode ()
    "Templates that name no mode would be dropped silently by Tempel."
    (let ((errors (moyue-templates--test-errors "(oops \"x\")\n")))
      (should (= 1 (length errors)))
      (should (string-match-p "without a mode" (car errors)))))

  (ert-deftest templates/rejects-a-dangling-keyword ()
    "A plist keyword without a value disables its whole group."
    (let ((errors (moyue-templates--test-errors
                   "foo-mode\n\n(t)\n\n:when\n")))
      (should (= 1 (length errors)))
      (should (string-match-p "dangling keyword :when" (car errors)))))

  (ert-deftest templates/rejects-a-stray-atom-without-hanging ()
    "An atom Tempel's reader cannot consume is reported, not looped on."
    (let ((errors (moyue-templates--test-errors
                   "foo-mode\n\n42\n\n(t)\n")))
      (should (cl-some (lambda (e) (string-match-p "unexpected element 42" e))
                       errors))))

  (ert-deftest templates/rejects-a-nil-element-without-hanging ()
    "A literal nil element stalls Tempel's mode loop and must be reported."
    (should (cl-some (lambda (e) (string-match-p "unexpected element nil" e))
                     (moyue-templates--test-errors "foo-mode\n\n(t)\n\nnil\n"))))

  (ert-deftest templates/rejects-a-duplicate-template-name ()
    "Only the first of two same-named templates for a mode is reachable."
    (let ((errors (moyue-templates--test-errors
                   "foo-mode\n\n(t \"one\")\n\nbar-mode\n\n(t \"two\")\n\nfoo-mode\n\n(t \"three\")\n")))
      (should (= 1 (length errors)))
      (should (string-match-p "defined twice for mode foo-mode" (car errors)))))

  (ert-deftest templates/allows-one-name-in-two-modes ()
    "The same name in different modes is not a duplicate."
    (should-not (moyue-templates--test-errors
                 "foo-mode\n\n(t \"one\")\n\nbar-mode\n\n(t \"two\")\n")))

  (ert-deftest templates/rejects-unreadable-lisp-data ()
    "An unbalanced file is reported with the reader's own message."
    (let ((errors (moyue-templates--test-errors "foo-mode\n\n(t\n")))
      (should (= 1 (length errors)))
      (should (string-match-p "unreadable Lisp data" (car errors)))))

  (ert-deftest templates/rejects-trailing-data ()
    "A tail Tempel would never read means the parentheses do not balance."
    (should (equal (moyue-templates--test-errors "foo-mode\n\n(t))\n(ignored)\n")
                   '("Data after the first Lisp form (unbalanced parentheses?)"))))

  (ert-deftest templates/rejects-an-empty-file ()
    (should (equal (moyue-templates--test-errors "; only a comment\n")
                   '("file contains no templates"))))

  (ert-deftest templates/rejects-a-missing-file ()
    (should (equal (moyue-templates-validate-file
                    "/nonexistent/moyue-templates")
                   '("no such file or directory")))
    (should (equal (moyue-templates-validate-file "/tmp")
                   '("is a directory"))))

  (ert-deftest templates/directory-argument-contributes-eld-files ()
    (let ((dir (make-temp-file "moyue-templates-dir" 'directory)))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "a.eld" dir)
              (insert "foo-mode\n\n(t)\n"))
            (with-temp-file (expand-file-name "b.eld" dir)
              (insert "(t)\n"))
            (with-temp-file (expand-file-name "note.txt" dir)
              (insert "not a template\n"))
            (let ((files (moyue-templates--files (list dir))))
              (should (= 2 (length files)))
              (should (cl-every (lambda (f) (string-suffix-p ".eld" f)) files))))
        (delete-directory dir t))))

  (ert-deftest templates/empty-directory-argument-is-an-error ()
    "A directory with no *.eld file is a typo, not a clean run."
    (let ((dir (make-temp-file "moyue-templates-empty" 'directory)))
      (unwind-protect
          (should-error (moyue-templates--files (list dir)))
        (delete-directory dir t))))

  (ert-deftest templates/default-file-sits-next-to-the-configuration ()
    (cl-letf (((symbol-function 'moyue--config-root) (lambda () "/tmp/moyu-config/")))
      (should (equal (moyue-templates-default-file) "/tmp/moyu-config/templates"))
      (should (equal (moyue-templates--files nil) '("/tmp/moyu-config/templates")))))

  (ert-deftest templates/help-arguments-print-the-usage ()
    "`--help' asks for the usage; it is not probed as a path."
    (let ((cmd (gethash "check-templates" moyue--commands))
          (messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args)
                   (push (apply #'format format args) messages)))
                ((symbol-function 'kill-emacs) (lambda (&optional _code) nil)))
        (funcall (moyue-command-fn cmd) '("--help")))
      (should (cl-some (lambda (m) (string-prefix-p "Usage: moyue check-templates" m))
                       messages))))

  (ert-deftest templates/check-counts-the-failing-files ()
    "The summary counts files, not errors, and the return value drives the exit."
    (let ((good (make-temp-file "moyue-templates-good" nil ".eld"))
          (bad (make-temp-file "moyue-templates-bad" nil ".eld"))
          (messages nil))
      (unwind-protect
          (progn
            (with-temp-file good (insert "foo-mode\n\n(t)\n"))
            (with-temp-file bad (insert "(t)\n"))
            (cl-letf (((symbol-function 'message)
                       (lambda (format &rest args)
                         (push (apply #'format format args) messages))))
              (should (= 1 (moyue-templates-check (list good bad)))))
            (should (member "Checked 2 file(s), 1 failure(s)" messages))
            (should (cl-some (lambda (m) (string-prefix-p "ok   " m)) messages))
            (should (cl-some (lambda (m) (string-prefix-p "FAIL " m)) messages)))
        (delete-file good)
        (delete-file bad)))))

;;; templates.el ends here

