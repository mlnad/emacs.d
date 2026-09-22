;;; env-ext.el --- Environment file for Emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; Emacs started from a desktop launcher, a systemd unit or `emacsclient'
;; does not inherit the environment of the login shell, so tools such as
;; `git', `ripgrep' or language servers may be missing from `exec-path'.
;;
;; This library keeps the environment in an ordinary Emacs Lisp file,
;; `.cache/env.el' below `user-emacs-directory', which is created once from
;; the login shell and may be edited by hand afterwards:
;;
;;   (moyu/env-ensure-file)   ; create it when missing (`moyue install')
;;   (moyu/env-load 'noerror) ; apply it at startup (init.el)
;;
;; The generated file only contains `setenv' calls, one per variable:
;;
;;   (setenv "PATH" "/usr/local/bin:/usr/bin:/bin")
;;   (setenv "GOPATH" "/home/user/go")
;;
;; so it can be customized like any other piece of Emacs Lisp.
;; `moyu/env-ensure-file' never overwrites an existing file; refresh it
;; explicitly from the shell with M-x moyu/reload-env.
;;
;;; Code:

(defvar moyu/env-filename "env.el"
  "Name of the environment file inside the `.cache' directory.")

(defun moyu/env-file ()
  "Return the absolute path of the environment file.
The path is derived from `user-emacs-directory' at call time, so batch
commands such as `moyue install' can bind that variable and still resolve
the same file."
  (expand-file-name moyu/env-filename
                    (expand-file-name ".cache/" user-emacs-directory)))

(defvar moyu/denied-env-patterns
  '(;; Unix/shell state that shouldn't be persisted
    "^HOME$" "^\\(OLD\\)?PWD$" "^SHLVL$" "^PS1$" "^R?PROMPT$" "^TERM\\(CAP\\)?$"
    "^USER$" "^INSIDE_EMACS$"
    ;; X server, Wayland, or session services
    "^DISPLAY$" "^WAYLAND_DISPLAY" "^DBUS_SESSION_BUS_ADDRESS$" "^XAUTHORITY$"
    ;; Windows / WSL
    "^WSL_INTEROP$"
    ;; XDG runtime-only variables
    "^XDG_CURRENT_DESKTOP$" "^XDG_RUNTIME_DIR$"
    "^XDG_\\(VTNR\\|SEAT\\|SESSION_\\(TYPE\\|CLASS\\)\\)"
    ;; Sockets: I3SOCK, GREETD_SOCK, SEATD_SOCK, SWAYSOCK, …
    "SOCK$"
    ;; SSH / GPG (become stale quickly)
    "^SSH_\\(AUTH_SOCK\\|AGENT_PID\\)$" "^\\(SSH\\|GPG\\)_TTY$" "^GPG_AGENT_INFO$"
    ;; Internal flags
    "^DEBUG$" "^INSECURE$" "^__")
  "Regexps matching env var names to exclude from the env cache file.")

(defun moyu/env--denied-p (var)
  "Return non-nil if VAR matches any pattern in `moyu/denied-env-patterns'."
  (seq-some (lambda (pat) (string-match-p pat var))
            moyu/denied-env-patterns))

(defun moyu/env--entry-split (entry)
  "Split \"KEY=VALUE\" ENTRY into a (KEY . VALUE) pair, or nil when malformed."
  (let ((pos (string-match "=" entry)))
    (when pos
      (cons (substring entry 0 pos)
            (substring entry (1+ pos))))))

(defun moyu/env--shell-environ ()
  "Return a list of \"KEY=VALUE\" strings sourced from the login shell.
Uses NUL-separated output (`env -0') when available so that values
containing newlines are handled correctly.  Falls back to the current
`process-environment' on Windows or when the shell cannot be invoked."
  (if (eq system-type 'windows-nt)
      (default-toplevel-value 'process-environment)
    (let* ((shell (or (getenv "SHELL") "/bin/sh"))
           (quoted (shell-quote-argument shell))
           ;; Try env -0 first (GNU coreutils; safe with embedded newlines)
           (raw0 (condition-case nil
                     (shell-command-to-string
                      (format "%s -l -c 'env -0' 2>/dev/null" quoted))
                   (error nil))))
      (cond
       ((and raw0 (string-match-p "\0" raw0))
        (seq-filter (lambda (s) (string-match-p "=" s))
                    (split-string raw0 "\0" t)))
       ;; Fallback: plain env, newlines in values are unlikely in practice
       (t
        (let ((raw (condition-case nil
                       (shell-command-to-string
                        (format "%s -l -c env 2>/dev/null" quoted))
                     (error ""))))
          (if (string-empty-p raw)
              (progn
                (message "env-ext: could not invoke login shell, \
falling back to process-environment")
                (default-toplevel-value 'process-environment))
            (seq-filter (lambda (s) (string-match-p "=" s))
                        (split-string raw "\n" t)))))))))

(defun moyu/env--captured-entries ()
  "Return the capturable login-shell environment as sorted (KEY . VALUE) pairs.
Denied names (see `moyu/denied-env-patterns') are dropped and duplicate
names are collapsed, last value first, so the generated file is stable."
  (let ((seen (make-hash-table :test #'equal))
        pairs)
    (dolist (entry (moyu/env--shell-environ))
      (let ((kv (moyu/env--entry-split entry)))
        (when (and kv
                   (not (string-empty-p (car kv)))
                   (not (moyu/env--denied-p (car kv))))
          (puthash (car kv) (cdr kv) seen))))
    (maphash (lambda (key value) (push (cons key value) pairs)) seen)
    (sort pairs (lambda (a b) (string< (car a) (car b))))))

;;;###autoload
(defun moyu/env-generate-file (&optional file)
  "Write the login-shell environment to FILE as Emacs Lisp.
FILE defaults to the value of `moyu/env-file'.  Return the file name."
  (let* ((file (or file (moyu/env-file)))
         (entries (moyu/env--captured-entries))
         (name (file-name-nondirectory file)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (setq-local coding-system-for-write 'utf-8-unix)
      (insert ";;; " name " --- Environment variables \
-*- mode: emacs-lisp; lexical-binding: t; coding: utf-8-unix; -*-\n"
              ";;\n"
              ";; Generated from the login shell by `moyue install'.\n"
              ";; This is ordinary Emacs Lisp: add, change or remove variables\n"
              ";; as needed.  It is created only once and never overwritten by\n"
              ";; `moyu/env-ensure-file'; M-x moyu/reload-env regenerates it\n"
              ";; from the shell.\n"
              ";;\n")
      (dolist (kv entries)
        (insert (format "(setenv %s %s)\n"
                        (prin1-to-string (car kv))
                        (prin1-to-string (cdr kv)))))
      (insert "\n;;; " name " ends here\n"))
    file))

;;;###autoload
(defun moyu/env-ensure-file (&optional file)
  "Create the environment file from the login shell when it is missing.
FILE defaults to `moyu/env-file'.  Return FILE when it was created, and
nil when it already existed — existing files are left untouched so that
manual edits survive."
  (let ((file (or file (moyu/env-file))))
    (unless (file-exists-p file)
      (moyu/env-generate-file file)
      file)))

(defun moyu/env--refresh-derived ()
  "Refresh `exec-path' and `shell-file-name' from `process-environment'."
  (let ((path (getenv "PATH")))
    (when (and path (not (string-empty-p path)))
      (setq-default exec-path
                    (append (split-string path path-separator t)
                            (list exec-directory)))))
  (let ((shell (getenv "SHELL")))
    (when (and shell (not (string-empty-p shell)))
      (setq-default shell-file-name shell))))

(defun moyu/env-load-file (&optional file noerror)
  "Load the environment variables stored in FILE into this Emacs session.
FILE defaults to `moyu/env-file'.  The file is ordinary Emacs Lisp and is
evaluated, so the variables it sets are merged into `process-environment'
(existing values for the same names are replaced).  `exec-path' and
`shell-file-name' are refreshed from the resulting environment.

Return non-nil when FILE was loaded.  Signal `file-error' when FILE does
not exist, unless NOERROR is non-nil, in which case nil is returned."
  (let ((file (or file (moyu/env-file))))
    (cond
     ((not (file-exists-p file))
      (unless noerror
        (signal 'file-error (list "No env file" file)))
      nil)
     (t
      (condition-case err
          (progn
            (load file nil 'nomessage)
            (moyu/env--refresh-derived)
            t)
        (error
         (if noerror
             nil
           (signal (car err) (cdr err)))))))))

;;;###autoload
(defun moyu/env-load (&optional noerror)
  "Load the default environment file into this Emacs session.
With NOERROR non-nil, do nothing when the file does not exist."
  (moyu/env-load-file (moyu/env-file) noerror))

;;;###autoload
(defun moyu/reload-env ()
  "Regenerate the environment file from the login shell and reload it."
  (interactive)
  (moyu/env-generate-file)
  (moyu/env-load-file)
  (message "env-ext: environment reloaded from %s (%d vars)"
           (moyu/env-file) (length process-environment)))

;;;###autoload
(defun moyu/env-visit-file ()
  "Open the environment file so it can be edited by hand."
  (interactive)
  (find-file (moyu/env-file)))

;;; Backward-compatible aliases for the original API.
(defalias 'generate-env-file #'moyu/env-generate-file)
(defalias 'load-env-file #'moyu/env-load-file)

(provide 'env-ext)

;;;; ── Tests ───────────────────────────────────────────────────────────────
;; Run:  emacs -q --batch -l lisp/env-ext.el --eval "(ert-run-tests-batch-and-exit)"

(with-eval-after-load 'ert

  ;; Helper: save and restore globals mutated by the env functions.
  (defmacro env-test--save-globals (&rest body)
    (declare (indent 0))
    `(let ((s-env   (default-value 'process-environment))
           (s-exec  (default-value 'exec-path))
           (s-shell (default-value 'shell-file-name)))
       (unwind-protect (progn ,@body)
         (setq-default process-environment s-env
                       exec-path           s-exec
                       shell-file-name     s-shell))))

  ;; Helper: write CONTENT to a temp .el file, bind FILE, run BODY.
  (defmacro env-test--with-file (content &rest body)
    (declare (indent 1))
    `(let ((file (make-temp-file "env-ext-test-" nil ".el")))
       (unwind-protect
           (progn
             (with-temp-file file
               (insert ";;; -*- lexical-binding: t; -*-\n" ,content))
             ,@body)
         (ignore-errors (delete-file file)))))

  ;; Helper: stub the login-shell capture with ENTRIES.
  (defmacro env-test--with-captured (entries &rest body)
    (declare (indent 1))
    `(cl-letf (((symbol-function 'moyu/env--shell-environ)
                (lambda () ,entries)))
       ,@body))

  ;;; moyu/env--denied-p

  (ert-deftest env-ext/denied-p-shell-state ()
    (dolist (v '("HOME" "PWD" "OLDPWD" "SHLVL" "PS1" "TERM" "TERMCAP"
                 "USER" "INSIDE_EMACS"))
      (should (moyu/env--denied-p v))))

  (ert-deftest env-ext/denied-p-display-and-session ()
    (dolist (v '("DISPLAY" "WAYLAND_DISPLAY" "DBUS_SESSION_BUS_ADDRESS"
                 "XAUTHORITY" "WSL_INTEROP"
                 "XDG_CURRENT_DESKTOP" "XDG_RUNTIME_DIR"
                 "XDG_VTNR" "XDG_SEAT" "XDG_SESSION_TYPE" "XDG_SESSION_CLASS"))
      (should (moyu/env--denied-p v))))

  (ert-deftest env-ext/denied-p-sockets ()
    (dolist (v '("I3SOCK" "SWAYSOCK" "GREETD_SOCK" "SEATD_SOCK"))
      (should (moyu/env--denied-p v))))

  (ert-deftest env-ext/denied-p-ssh-gpg ()
    (dolist (v '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "SSH_TTY"
                 "GPG_TTY" "GPG_AGENT_INFO"))
      (should (moyu/env--denied-p v))))

  (ert-deftest env-ext/denied-p-internal-flags ()
    (dolist (v '("DEBUG" "INSECURE" "__CF_USER_TEXT_ENCODING" "__SOME_VAR"))
      (should (moyu/env--denied-p v))))

  (ert-deftest env-ext/denied-p-allows-dev-vars ()
    (dolist (v '("PATH" "GOPATH" "GOROOT" "CARGO_HOME" "RUSTUP_HOME"
                 "NVM_DIR" "JAVA_HOME" "LANG" "EDITOR"
                 "DEEPSEEK_API_KEY" "OPENAI_API_KEY"))
      (should-not (moyu/env--denied-p v))))

  ;;; moyu/env--entry-split

  (ert-deftest env-ext/entry-split-keeps-equals-in-value ()
    (should (equal (moyu/env--entry-split "A=b=c") '("A" . "b=c")))
    (should (equal (moyu/env--entry-split "A=") '("A" . "")))
    (should-not (moyu/env--entry-split "NOEQUALS")))

  ;;; moyu/env-file

  (ert-deftest env-ext/env-file-lives-in-cache ()
    (let ((user-emacs-directory "/tmp/env-ext-home/"))
      (should (equal (moyu/env-file) "/tmp/env-ext-home/.cache/env.el"))))

  ;;; moyu/env-generate-file

  (ert-deftest env-ext/generate-returns-file ()
    (env-test--with-captured nil
      (let ((file (make-temp-file "env-ext-gen-" nil ".el")))
        (unwind-protect (should (equal file (moyu/env-generate-file file)))
          (ignore-errors (delete-file file))))))

  (ert-deftest env-ext/generate-writes-loadable-elisp ()
    (env-test--save-globals
      (env-test--with-captured '("GOPATH=/home/user/go" "PATH=/usr/bin")
        (let ((file (make-temp-file "env-ext-gen-" nil ".el")))
          (unwind-protect
              (progn
                (moyu/env-generate-file file)
                (load file nil 'nomessage)
                (should (equal (getenv "GOPATH") "/home/user/go")))
            (ignore-errors (delete-file file)))))))

  (ert-deftest env-ext/generate-excludes-denied-includes-allowed ()
    (env-test--with-captured '("HOME=/root" "SSH_AUTH_SOCK=/tmp/s.sock"
                               "GOPATH=/home/user/go" "PATH=/usr/bin")
      (let ((file (make-temp-file "env-ext-gen-" nil ".el")))
        (unwind-protect
            (progn
              (moyu/env-generate-file file)
              (let ((c (with-temp-buffer (insert-file-contents file) (buffer-string))))
                (should-not (string-match-p "\"HOME\""         c))
                (should-not (string-match-p "SSH_AUTH_SOCK"    c))
                (should     (string-match-p "\"GOPATH\""       c))
                (should     (string-match-p "\"PATH\""         c))))
          (ignore-errors (delete-file file))))))

  (ert-deftest env-ext/generate-sorts-and-dedupes ()
    (env-test--with-captured '("B=2" "A=1" "A=3")
      (let ((file (make-temp-file "env-ext-gen-" nil ".el")))
        (unwind-protect
            (progn
              (moyu/env-generate-file file)
              (let ((c (with-temp-buffer (insert-file-contents file) (buffer-string))))
                (should (string-match-p "(setenv \"A\" \"3\")" c))
                (should (= 1 (length (seq-filter
                                      (lambda (l) (string-match-p "(setenv \"A\"" l))
                                      (split-string c "\n")))))
                (should (< (string-match-p "(setenv \"A\"" c)
                           (string-match-p "(setenv \"B\"" c)))))
          (ignore-errors (delete-file file))))))

  (ert-deftest env-ext/generate-creates-parent-directory ()
    (env-test--with-captured '("PATH=/usr/bin")
      (let* ((dir (make-temp-file "env-ext-dir-" t))
             (file (expand-file-name "nested/env.el" dir)))
        (unwind-protect
            (progn
              (moyu/env-generate-file file)
              (should (file-exists-p file)))
          (ignore-errors (delete-directory dir t))))))

  (ert-deftest env-ext/generate-round-trips-awkward-values ()
    (env-test--save-globals
      (env-test--with-captured '("ENVT_WEIRD=a b\tc" "ENVT_NL=line1\nline2"
                                 "ENVT_QUOTE=say \"hi\"" "ENVT_EQ=a=b")
        (let ((file (make-temp-file "env-ext-gen-" nil ".el")))
          (unwind-protect
              (progn
                (moyu/env-generate-file file)
                (load file nil 'nomessage)
                (should (equal (getenv "ENVT_WEIRD") "a b\tc"))
                (should (equal (getenv "ENVT_NL") "line1\nline2"))
                (should (equal (getenv "ENVT_QUOTE") "say \"hi\""))
                (should (equal (getenv "ENVT_EQ") "a=b")))
            (ignore-errors (delete-file file)))))))

  ;;; moyu/env-ensure-file

  (ert-deftest env-ext/ensure-creates-missing-file ()
    (env-test--with-captured '("PATH=/usr/bin")
      (let* ((dir (make-temp-file "env-ext-ensure-" t))
             (file (expand-file-name ".cache/env.el" dir)))
        (unwind-protect
            (progn
              (should (equal file (moyu/env-ensure-file file)))
              (should (file-exists-p file)))
          (ignore-errors (delete-directory dir t))))))

  (ert-deftest env-ext/ensure-keeps-existing-file ()
    (env-test--with-captured '("PATH=/usr/bin")
      (env-test--with-file ";; hand written\n(setenv \"ENVT_KEEP\" \"yes\")\n"
        (should-not (moyu/env-ensure-file file))
        (with-temp-buffer
          (insert-file-contents file)
          (should (string-match-p "hand written" (buffer-string)))
          (should-not (string-match-p "\"PATH\"" (buffer-string)))))))

  ;;; moyu/env-load-file

  (ert-deftest env-ext/load-missing-signals-file-error ()
    (should-error (moyu/env-load-file "/no/such/path/env-xyz.el")
                  :type 'file-error))

  (ert-deftest env-ext/load-missing-noerror-returns-nil ()
    (should-not (moyu/env-load-file "/no/such/path/env-xyz.el" 'noerror)))

  (ert-deftest env-ext/load-merges-vars ()
    (env-test--save-globals
      (env-test--with-file "(setenv \"ENVT_A\" \"aaa\")\n(setenv \"ENVT_B\" \"bbb\")\n"
        (should (moyu/env-load-file file))
        (should (equal (getenv "ENVT_A") "aaa"))
        (should (equal (getenv "ENVT_B") "bbb")))))

  (ert-deftest env-ext/load-overrides-existing-key ()
    (env-test--save-globals
      (setenv "ENVT_OVER" "old")
      (env-test--with-file "(setenv \"ENVT_OVER\" \"new\")\n"
        (moyu/env-load-file file)
        (should (equal (getenv "ENVT_OVER") "new"))
        (let ((hits (seq-filter (lambda (e) (string-prefix-p "ENVT_OVER=" e))
                                process-environment)))
          (should (= 1 (length hits)))))))

  (ert-deftest env-ext/load-no-duplicates-on-reload ()
    (env-test--save-globals
      (env-test--with-file "(setenv \"ENVT_DUP\" \"v\")\n"
        (moyu/env-load-file file)
        (moyu/env-load-file file)
        (should (= 1 (length (seq-filter (lambda (e) (string-prefix-p "ENVT_DUP=" e))
                                         process-environment)))))))

  (ert-deftest env-ext/load-updates-exec-path ()
    (env-test--save-globals
      (env-test--with-file "(setenv \"PATH\" \"/envtest/bin:/usr/bin\")\n"
        (moyu/env-load-file file)
        (should (member "/envtest/bin" exec-path)))))

  (ert-deftest env-ext/load-updates-shell-file-name ()
    (env-test--save-globals
      (env-test--with-file "(setenv \"PATH\" \"/usr/bin\")\n(setenv \"SHELL\" \"/envtest/sh\")\n"
        (moyu/env-load-file file)
        (should (equal shell-file-name "/envtest/sh")))))

  (ert-deftest env-ext/load-malformed-noerror ()
    (env-test--with-file "((not valid lisp"
      (should-not (moyu/env-load-file file 'noerror))))

  (ert-deftest env-ext/load-malformed-signals-without-noerror ()
    (env-test--with-file "((not valid lisp"
      (should-error (moyu/env-load-file file))))

  ;;; moyu/env-load

  (ert-deftest env-ext/env-load-uses-default-file ()
    (env-test--save-globals
      (let* ((dir (make-temp-file "env-ext-home-" t))
             (user-emacs-directory (file-name-as-directory dir)))
        (unwind-protect
            (progn
              (make-directory (expand-file-name ".cache" dir) t)
              (with-temp-file (moyu/env-file)
                (insert "(setenv \"ENVT_DEFAULT\" \"loaded\")\n"))
              (should (moyu/env-load))
              (should (equal (getenv "ENVT_DEFAULT") "loaded")))
          (ignore-errors (delete-directory dir t))))))

  (ert-deftest env-ext/env-load-noerror-when-missing ()
    (let* ((dir (make-temp-file "env-ext-home-" t))
           (user-emacs-directory (file-name-as-directory dir)))
      (unwind-protect (should-not (moyu/env-load 'noerror))
        (ignore-errors (delete-directory dir t)))))

  ;;; moyu/reload-env

  (ert-deftest env-ext/reload-regenerates-and-loads ()
    (env-test--save-globals
      (let* ((dir (make-temp-file "env-ext-home-" t))
             (user-emacs-directory (file-name-as-directory dir)))
        (unwind-protect
            (env-test--with-captured '("ENVT_RELOAD=fresh" "PATH=/usr/bin")
              (moyu/reload-env)
              (should (equal (getenv "ENVT_RELOAD") "fresh"))
              (should (file-exists-p (moyu/env-file))))
          (ignore-errors (delete-directory dir t)))))))

;;; env-ext.el ends here
