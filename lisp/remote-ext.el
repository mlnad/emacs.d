;;; remote-ext.el --- run a remote workspace's processes on the remote host -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;
;; Local Emacs, remote processes.  When a buffer's `default-directory' is a
;; TRAMP file name, the processes Emacs starts for that buffer belong on the
;; remote host: clangd, gcc, git, grep, formatters, and `M-!' shell commands.
;;
;; Emacs already routes part of this by itself.  These honour TRAMP:
;;
;;   `process-file', `start-file-process'
;;   `process-file-shell-command', `start-file-process-shell-command'
;;   `make-process' when the caller passes `:file-handler t'
;;
;; eglot and dape do pass it, which is why remote clangd mostly works already.
;; What does not honour TRAMP, and therefore runs on the *local* machine even
;; when `default-directory' is remote:
;;
;;   `call-process'                 (the Emacs manual: use `process-file')
;;   `call-process-shell-command'   (so `M-!' == `shell-command' is local)
;;   `start-process', `start-process-shell-command'
;;   `make-process' without `:file-handler t'
;;   `executable-find' with its default REMOTE argument
;;
;; This file closes those gaps and pins down the per-host search path, so a
;; remote workspace really does use the remote server's tools.
;;
;; Two mechanisms are worth knowing about, because they surprise people:
;;
;; 1. `exec-path' is only used for *finding* programs (`executable-find').
;;    Execution uses the PATH that TRAMP computes from `tramp-remote-path'
;;    and installs in the remote process environment.  Both are set here:
;;    `exec-path' buffer-locally (so discovery matches execution), and
;;    `tramp-remote-path' is left to the user's init.
;;
;; 2. TRAMP forwards to a remote process only those variables of the buffer's
;;    `process-environment' that are *not* in the top-level value (see
;;    `tramp-local-environment-variable-p').  So `moyu/remote-env' is applied
;;    by prepending to the top-level value; replacing it outright would
;;    invert the filter and leak every local variable to the remote host.
;;
;; Known gap, deliberately not papered over: `call-process-region' (used by
;; `M-|', `shell-command-on-region') has no TRAMP-aware counterpart --
;; `process-file-region' does not exist -- and TRAMP's `make-process' does not
;; embed a local INFILE.  Region filters therefore stay local.  For pure text
;; filters that is harmless; for `M-|' commands that take file arguments it is
;; not.  Use `M-!' or a remote shell instead.
;;
;; Entry points:
;;   `moyu/remote-mode'        route remote-workspace processes remotely (t)
;;   `moyu/remote-status'      report what is in effect for this buffer
;;   `moyu/remote-self-test'   verify against a real host, batch-friendly
;;   `moyu/remote-cleanup'     drop the cached path and the TRAMP connection
;;   `moyu/remote-eglot-ensure', `moyu/remote-eglot-reconnect'
;;                             start eglot on the remote clangd, and recover
;;                             when its first handshake stalls (see below)
;;
;; One known rough edge of remote LSP, measured rather than guessed: the first
;; `initialize' of a session carries several kilobytes of capabilities and can
;; stall over a freshly opened TRAMP pipe until `eglot-connect-timeout' fires
;; and eglot kills the server.  The same handshake succeeds immediately on a
;; new ssh channel, which is what `moyu/remote-eglot-reconnect' arranges.
;;
;;; Code:

(require 'cl-lib)

;; TRAMP itself is deliberately not required here: this file is loaded at
;; startup, and the user's init keeps TRAMP lazy.  Everything TRAMP-specific
;; below is called only from inside a remote buffer, i.e. after TRAMP's own
;; file name handlers are in place.

(declare-function tramp-get-remote-path "tramp-sh" (vec))
(declare-function tramp-cleanup-this-connection "tramp" ())
(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
(declare-function eglot-ensure "eglot" (&optional interactive project))
(declare-function eglot-current-server "eglot" (&optional managed-buffers))
(declare-function eglot-shutdown "eglot" (server &optional timeout quit-p))
(defvar eglot-server-programs)

(defgroup moyu-remote nil
  "Run a remote workspace's processes on the remote host."
  :group 'tramp
  :prefix "moyu/remote-")

(defcustom moyu/remote-mode t
  "How to route process calls made from a remote workspace buffer.

nil   leave Emacs alone; every process API keeps its stock behaviour.
warn  run the stock behaviour, but record the calls that *would* have been
      rewritten in `moyu/remote-log-buffer'.  Use this to audit which
      packages in your configuration are remote-blind.
t     rewrite the call so the process runs on the remote host.

Only buffers whose `default-directory' is a TRAMP file name are affected;
local buffers are never touched."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Warn only" warn)
                 (const :tag "Route remotely" t)))

(defcustom moyu/remote-exec-path nil
  "Remote directories to use as `exec-path' in remote buffers.
When nil, the list is derived per connection from the PATH that TRAMP
computes for the remote host (`tramp-get-remote-path'), falling back to
asking the remote login shell.  Set this to skip that lookup and pin a
fixed list."
  :type '(repeat string))

(defcustom moyu/remote-env nil
  "Extra environment variables for remote processes, as \"NAME=VALUE\".
Useful on hosts without internet access, for example
\(\"GOPROXY=off\" \"GOTELEMETRY=off\").  Applied by prepending to the
top-level `process-environment' in remote buffers, which is what makes
TRAMP forward exactly these variables and nothing else."
  :type '(repeat string))

(defcustom moyu/remote-log-buffer " *remote-ext log*"
  "Buffer recording process calls routed, or not routed, to the remote host."
  :type 'string)

(defcustom moyu/remote-log-all nil
  "When non-nil, log every rewrite, not only those seen in `warn' mode."
  :type 'boolean)

(defcustom moyu/remote-clangd-program '("clangd")
  "Command starting clangd in a remote C or C++ workspace.
A list: the program followed by its arguments.  Used by
`moyu/remote-eglot-setup' to set a buffer-local `eglot-server-program'."
  :type '(repeat string))

(defvar moyu/remote--inhibit nil
  "Dynamically bound to t while this file's advice is already dispatching.
Prevents a rewrite from re-entering itself through TRAMP's own internal
process calls.")

(defvar moyu/remote--path-cache (make-hash-table :test #'equal)
  "Cache of connection id -> remote `exec-path' list.")

(defvar-local moyu/remote--prepared nil
  "Connection id this buffer was prepared for, or nil when not prepared.")

;;; Workspace predicate and connection identity

(defun moyu/remote-workspace-p (&optional dir)
  "Return the TRAMP prefix when DIR (default `default-directory') is remote."
  (file-remote-p (or dir default-directory)))

(defun moyu/remote--connection-id (&optional dir)
  "Return a string identifying the connection of DIR.
Matches the granularity of TRAMP's own connection-local criteria, so a
host reached on two ports yields two different ids."
  (file-remote-p (or dir default-directory)))

(defun moyu/remote--active-p ()
  "Non-nil when this buffer's processes belong on the remote host."
  (and (not moyu/remote--inhibit)
       (not (null moyu/remote-mode))
       (moyu/remote-workspace-p)))

(defun moyu/remote--rewrite-p ()
  "Non-nil when this buffer's process calls should actually be rewritten."
  (and (moyu/remote--active-p) (eq moyu/remote-mode t)))

(defmacro moyu/remote--dispatch (&rest body)
  "Evaluate BODY with this file's advice disabled.
Use around anything that hands control back to TRAMP, whose internal
process calls must not be rewritten a second time."
  (declare (indent 0) (debug t))
  `(let ((moyu/remote--inhibit t)) ,@body))

;;; Logging

(defun moyu/remote--log (kind detail)
  "Record KIND with DETAIL when logging is enabled for the current mode."
  (when (or (eq moyu/remote-mode 'warn) moyu/remote-log-all)
    (with-current-buffer (get-buffer-create moyu/remote-log-buffer)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "%s  %-30s %s%s\n"
                        (format-time-string "%H:%M:%S")
                        kind
                        detail
                        (if (eq moyu/remote-mode 'warn) "   [warn only]" "")))))))

;;; The remote search path

(defun moyu/remote--query-path (dir)
  "Ask the remote host in DIR for its PATH, as a list of directories."
  (with-temp-buffer
    (let ((default-directory dir))
      (moyu/remote--dispatch
        (ignore-errors
          (process-file "sh" nil t nil "-c" "printf '%s' \"$PATH\""))))
    (split-string (buffer-string) ":" t)))

(defun moyu/remote--tramp-path (dir)
  "Return the PATH TRAMP computed for DIR, or nil.
Using TRAMP's own list guarantees that what `executable-find' searches is
exactly what remote commands will resolve."
  (when (and (fboundp 'tramp-get-remote-path)
             (ignore-errors (require 'tramp-sh nil t)))
    (ignore-errors
      (tramp-get-remote-path (tramp-dissect-file-name dir)))))

(defun moyu/remote--path-for (dir)
  "Return the list of directories to search for programs when in DIR."
  (or moyu/remote-exec-path
      (let ((id (moyu/remote--connection-id dir)))
        (or (gethash id moyu/remote--path-cache)
            (puthash id
                     (or (moyu/remote--tramp-path dir)
                         (moyu/remote--query-path dir)
                         ;; Last resort: the usual Unix layout.
                         '("/usr/local/bin" "/usr/bin" "/bin"))
                     moyu/remote--path-cache)))))

;;; Preparing a remote buffer

(defun moyu/remote--ensure ()
  "Prepare the current remote buffer to use the remote host's tools.
Idempotent per connection: sets buffer-local `exec-path' and prepends
`moyu/remote-env' to the buffer's `process-environment'."
  (when (and (moyu/remote-workspace-p)
             (not (equal moyu/remote--prepared
                         (moyu/remote--connection-id))))
    (setq moyu/remote--prepared (moyu/remote--connection-id))
    (let ((path (moyu/remote--path-for default-directory)))
      (when path
        (setq-local exec-path path)))
    (when moyu/remote-env
      ;; TRAMP forwards a variable only when it is absent from the top-level
      ;; value.  Prepending keeps exactly `moyu/remote-env' in the diff.
      (setq-local process-environment
                  (append moyu/remote-env
                          (default-toplevel-value 'process-environment))))
    t))

;;;###autoload
(defun moyu/remote-prepare ()
  "Prepare the current buffer for remote processes.
Call this from a hook when a package reads `exec-path' directly instead of
going through `executable-find'."
  (interactive)
  (moyu/remote--ensure))

;;; Process routing

(defun moyu/remote--note (kind detail)
  "Log a rewrite or a would-be rewrite of KIND."
  (when (moyu/remote--active-p)
    (moyu/remote--log kind detail)))

(defun moyu/remote--call-process (fn program &optional infile destination display &rest args)
  "Route `call-process' to `process-file' for remote PROGRAM.
The argument lists of the two functions line up positionally, so the
substitution is exact."
  (if (moyu/remote--rewrite-p)
      (progn
        (moyu/remote--ensure)
        (moyu/remote--note "call-process" (format "%s -> process-file" program))
        (moyu/remote--dispatch
          (apply #'process-file program infile destination display args)))
    (progn (moyu/remote--note "call-process" (format "%s [local]" program))
           (apply fn program infile destination display args))))

(defun moyu/remote--call-process-shell-command (fn command &optional infile buffer display &rest args)
  "Route `call-process-shell-command' to its TRAMP-aware sibling.
This is what makes `shell-command' (\\[shell-command]) use the remote shell."
  (if (moyu/remote--rewrite-p)
      (progn
        (moyu/remote--ensure)
        (moyu/remote--note "call-process-shell-command"
                           (format "%S -> process-file-shell-command" command))
        (moyu/remote--dispatch
          (apply #'process-file-shell-command command infile buffer display args)))
    (progn (moyu/remote--note "call-process-shell-command" (format "%S [local]" command))
           (apply fn command infile buffer display args))))

(defun moyu/remote--start-process (fn name buffer program &rest args)
  "Route `start-process' to `start-file-process' for remote PROGRAM."
  (if (moyu/remote--rewrite-p)
      (progn
        (moyu/remote--ensure)
        (moyu/remote--note "start-process" (format "%s -> start-file-process" program))
        (moyu/remote--dispatch
          (apply #'start-file-process name buffer program args)))
    (progn (moyu/remote--note "start-process" (format "%s [local]" program))
           (apply fn name buffer program args))))

(defun moyu/remote--start-process-shell-command (fn name buffer command)
  "Route `start-process-shell-command' to its TRAMP-aware sibling.
This is what makes asynchronous `shell-command' (\\[async-shell-command])
use the remote shell."
  (if (moyu/remote--rewrite-p)
      (progn
        (moyu/remote--ensure)
        (moyu/remote--note "start-process-shell-command"
                           (format "%S -> start-file-process-shell-command" command))
        (moyu/remote--dispatch
          (start-file-process-shell-command name buffer command)))
    (progn (moyu/remote--note "start-process-shell-command" (format "%S [local]" command))
           (funcall fn name buffer command))))

(defun moyu/remote--make-process-args (args)
  "Return a remote-safe copy of the `make-process' argument plist ARGS.
Three things have to be fixed together; adding only `:file-handler' would
trade one failure for another."
  (let ((plist (copy-sequence args)))
    ;; TRAMP handles the call only when it is asked to.
    (unless (eq t (plist-get plist :file-handler))
      (setq plist (plist-put plist :file-handler t)))
    ;; `make-process' defaults `:connection-type' to `process-connection-type',
    ;; which is a pty.  A remote pty translates CR/LF and cannot be used for
    ;; LSP or DAP framing, so force a plain pipe.
    (when (memq (plist-get plist :connection-type) '(nil pty))
      (setq plist (plist-put plist :connection-type 'pipe)))
    ;; TRAMP rejects a pipe *process* as `:stderr'; a buffer is supported
    ;; (TRAMP creates a FIFO on the remote host for it).
    (when (processp (plist-get plist :stderr))
      (setq plist (plist-put plist :stderr
                             (get-buffer-create
                              (format " *%s stderr*"
                                      (or (plist-get plist :name) "process"))))))
    plist))

(defun moyu/remote--make-process (fn &rest args)
  "Add TRAMP routing to `make-process' calls made from a remote buffer."
  (if (moyu/remote--rewrite-p)
      (let ((fixed (moyu/remote--make-process-args args)))
        (moyu/remote--ensure)
        (moyu/remote--note "make-process"
                           (format "%S" (plist-get fixed :command)))
        (moyu/remote--dispatch (apply fn fixed)))
    (progn (moyu/remote--note "make-process"
                              (format "%S [local]"
                                      (plist-get args :command)))
           (apply fn args))))

;;;###autoload
(defun moyu/remote--executable-find (fn command &optional remote)
  "Make `executable-find' look on the remote host from a remote buffer.
Emacs defaults REMOTE to nil, which searches the *local* filesystem using
`exec-path' -- and after `moyu/remote--ensure' that list holds remote
directories, so the stock behaviour would look for remote paths locally."
  (if (and (moyu/remote--active-p) (not remote))
      (progn
        (moyu/remote--ensure)
        (funcall fn command t))
    (progn (moyu/remote--ensure)
           (funcall fn command remote))))

;;; Installing and removing the advice

(defconst moyu/remote--advice
  '((call-process . moyu/remote--call-process)
    (call-process-shell-command . moyu/remote--call-process-shell-command)
    (start-process . moyu/remote--start-process)
    (start-process-shell-command . moyu/remote--start-process-shell-command)
    (make-process . moyu/remote--make-process)
    (executable-find . moyu/remote--executable-find))
  "Alist of function -> advice installed by this file.")

;;;###autoload
(defun moyu/remote-install ()
  "Install the remote process routing advice."
  (interactive)
  (dolist (cell moyu/remote--advice)
    (advice-add (car cell) :around (cdr cell))))

;;;###autoload
(defun moyu/remote-uninstall ()
  "Remove the remote process routing advice."
  (interactive)
  (dolist (cell moyu/remote--advice)
    (advice-remove (car cell) (cdr cell))))

(moyu/remote-install)

;;; clangd and eglot in a remote workspace
;;
;; eglot needs no help for the common case: its C/C++ entry is
;; `(eglot-alternatives \\='("clangd" "ccls"))', and that calls
;; `executable-find' with REMOTE non-nil, which searches the buffer's
;; `exec-path' on the remote host.  Once `moyu/remote--ensure' has made that
;; list the remote one, eglot starts the remote clangd by itself.
;;
;; What eglot cannot do is pass extra clangd arguments per buffer, and it
;; cannot tell you *why* it found nothing.  Both are handled below.

(declare-function eglot-ensure "eglot" (&optional interactive project))

(defun moyu/remote-clangd-contact (&optional _interactive _project)
  "Return an eglot contact for clangd, resolved on the buffer's own host.
Suitable as a function entry in `eglot-server-programs'; eglot calls it
with INTERACTIVE and PROJECT.  In a remote buffer the program is looked up
on the remote host, so `moyu/remote-clangd-program' may carry remote-only
arguments."
  (let* ((program (car moyu/remote-clangd-program))
         (args (cdr moyu/remote-clangd-program))
         (remote (moyu/remote-workspace-p))
         (found (and remote (moyu/remote--ensure) (executable-find program))))
    (when (and remote (null found))
      (user-error "remote-ext: `%s' not found on %s.  Remote PATH: %s"
                  program (moyu/remote--connection-id)
                  (mapconcat #'identity exec-path ":")))
    (cons (or found program) args)))

;;;###autoload
(defun moyu/remote-register-clangd (&optional remove)
  "Make eglot use `moyu/remote-clangd-contact' for C and C++ buffers.
Only needed when `moyu/remote-clangd-program' carries arguments: without
them eglot's own entry already resolves clangd on the right host.  With
REMOVE, restore eglot's default C/C++ entry."
  (interactive "P")
  (require 'eglot)
  (let ((modes '(c-mode c-ts-mode c++-mode c++-ts-mode objc-mode)))
    (setq eglot-server-programs
          (assoc-delete-all modes eglot-server-programs))
    (unless remove
      (push (cons modes #'moyu/remote-clangd-contact)
            eglot-server-programs))
    (when (called-interactively-p 'interactive)
      (message "remote-ext: clangd contact %s"
               (if remove "restored to eglot's default" "registered")))))

;;;###autoload
(defun moyu/remote-eglot-setup ()
  "Check that this remote buffer will start the remote clangd.
Returns the resolved remote program, or signals a `user-error' with the
remote PATH when clangd is missing there."
  (interactive)
  (unless (moyu/remote-workspace-p)
    (user-error "remote-ext: not a remote buffer"))
  (moyu/remote--ensure)
  (let* ((program (car moyu/remote-clangd-program))
         (found (executable-find program)))
    (unless found
      (user-error "remote-ext: `%s' not found on %s.  Remote PATH: %s"
                  program (moyu/remote--connection-id)
                  (mapconcat #'identity exec-path ":")))
    (when (called-interactively-p 'interactive)
      (message "remote-ext: %s on %s -> %s"
               program (moyu/remote--connection-id) found))
    found))

;;;###autoload
(defun moyu/remote-eglot-ensure ()
  "Verify the remote toolchain, then start eglot in this buffer."
  (interactive)
  (moyu/remote-eglot-setup)
  (eglot-ensure))

;;;###autoload
(defun moyu/remote-eglot-reconnect ()
  "Restart eglot in this buffer on a fresh TRAMP connection.

Worth knowing about: the *first* `initialize' request of a session is
several kilobytes of client capabilities, and over a freshly opened TRAMP
pipe it can stall until `eglot-connect-timeout' fires, after which eglot
kills the server (it exits with status 9).  A second attempt on a new ssh
channel connects immediately.  This command does exactly that, so a stalled
first connection costs one keystroke instead of a debugging session.

Reproduced against a containerised clangd: attempt 1 timed out at 60s,
attempt 2 answered `workspace/symbol', `textDocument/definition' and
published diagnostics normally."
  (interactive)
  (when-let* ((server (eglot-current-server)))
    (ignore-errors (eglot-shutdown server 3)))
  (moyu/remote-cleanup)
  (moyu/remote-eglot-ensure))

;;; Status, self test and cleanup

(defun moyu/remote-status (&optional dir)
  "Describe what is in effect for DIR (default `default-directory').
Returns the report as a string and, interactively, displays it."
  (interactive)
  (let* ((dir (or dir default-directory))
         (remote (moyu/remote-workspace-p dir))
         (report
          (with-temp-buffer
            (insert (format "remote-ext status\n"))
            (insert (format "  default-directory  %s\n" dir))
            (insert (format "  workspace          %s\n"
                            (if remote (format "remote (%s)" remote) "local")))
            (insert (format "  mode               %S\n" moyu/remote-mode))
            (insert (format "  prepared for       %S\n" moyu/remote--prepared))
            (when remote
              (let* ((default-directory dir)
                     (path (moyu/remote--path-for dir)))
                (insert (format "  exec-path source   %s\n"
                                (cond (moyu/remote-exec-path "moyu/remote-exec-path")
                                      ((gethash (moyu/remote--connection-id dir)
                                                moyu/remote--path-cache)
                                       "cached")
                                      (t "unresolved"))))
                (insert (format "  exec-path entries   %d\n" (length path)))
                (insert (format "  PATH                %s\n"
                                (mapconcat #'identity path ":")))
                (insert (format "  clangd (remote)     %s\n"
                                (or (ignore-errors (executable-find "clangd")) "not found")))
                (insert (format "  TRAMP share         %s\n"
                                (if (boundp 'tramp-use-connection-share)
                                    tramp-use-connection-share
                                  "unbound")))))
            (buffer-string))))
    (when (called-interactively-p 'interactive)
      (with-current-buffer (get-buffer-create "*remote-ext status*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert report)
          (goto-char (point-min))
          (special-mode)))
      (display-buffer "*remote-ext status*"))
    report))

(defun moyu/remote--output (dir program &rest args)
  "Run PROGRAM ARGS on the host of DIR and return its stdout, trimmed."
  (with-temp-buffer
    (let ((default-directory dir))
      (moyu/remote--ensure)
      (apply #'process-file program nil t nil args))
    (string-trim (buffer-string))))

(defun moyu/remote--capture (dir thunk)
  "Call THUNK with `default-directory' bound to DIR, returning its output.
THUNK is expected to insert into the current buffer."
  (with-temp-buffer
    (let ((default-directory dir))
      (funcall thunk))
    (string-trim (buffer-string))))

(defun moyu/remote--capture-async (dir name program)
  "Run PROGRAM asynchronously in DIR and return the buffer it wrote to.
Separate helper for the self test, because the output of an asynchronous
process is not guaranteed to have arrived when `accept-process-output'
returns: wait for the process to exit, then flush."
  (let ((buf (get-buffer-create (format " *%s*" name)))
        (proc nil)
        (i 0))
    (with-current-buffer buf (erase-buffer))
    (setq proc (let ((default-directory dir))
                 (moyu/remote--ensure)
                 (make-process :name name :buffer buf
                               :command (list program) :noquery t)))
    (while (and (process-live-p proc) (< (cl-incf i) 100))
      (accept-process-output proc 0.2))
    ;; One more round so the filter runs after the process has exited.
    (accept-process-output proc 0.2)
    buf))

(defun moyu/remote-self-test (&optional dir)
  "Verify that DIR's processes really run on the remote host.
DIR defaults to `default-directory'.  Returns a list of
\(CHECK RESULT STATUS) triples, where STATUS is `ok', `fail' or `info';
interactively the result is also shown in `*remote-ext self test*'.

Run it in batch against a throwaway host:

  emacs --batch -L lisp -l remote-ext \\
    --eval \"(print (moyu/remote-self-test \\\"/ssh:root@localhost#2222:/work/\\\"))\""
  (interactive)
  (let* ((dir (file-name-as-directory (expand-file-name (or dir default-directory))))
         (local-host (system-name))
         (remote-host (ignore-errors (moyu/remote--output dir "hostname")))
         (results nil))
    (cl-flet ((add (check result status) (push (list check result status) results)))
      (add "workspace" (if (moyu/remote-workspace-p dir)
                           (format "remote %s" (moyu/remote--connection-id dir))
                         "LOCAL")
           (if (moyu/remote-workspace-p dir) 'ok 'fail))
      (add "local system-name" local-host 'info)
      (add "process-file hostname"
           (or remote-host "ERROR")
           (if (and remote-host (not (equal remote-host local-host))) 'ok 'fail))
      ;; `call-process' is the one that silently runs locally without advice.
      (let ((out (moyu/remote--capture
                  dir (lambda () (call-process "hostname" nil t)))))
        (add "call-process hostname" out
             (if (equal out remote-host) 'ok 'fail)))
      ;; `shell-command' is `M-!'; it goes through
      ;; `call-process-shell-command', which is not TRAMP-aware on its own.
      (let ((out (moyu/remote--capture
                  dir (lambda () (shell-command "hostname" t)))))
        (add "shell-command hostname" out
             (if (equal out remote-host) 'ok 'fail)))
      ;; `make-process' with no `:file-handler'.
      (let* ((buf (moyu/remote--capture-async dir "remote-ext self test" "hostname"))
             (out (with-current-buffer buf (string-trim (buffer-string)))))
        (add "make-process hostname" out
             (if (equal out remote-host) 'ok 'fail)))
      (let* ((default-directory dir)
             (found (ignore-errors (executable-find "clangd"))))
        (add "executable-find clangd" (or found "not found")
             (if found 'ok 'fail))
        (add "clangd --version"
             (or (ignore-errors
                   (car (split-string (moyu/remote--output dir "clangd" "--version")
                                      "\n")))
                 "ERROR")
             'info)))
    (setq results (nreverse results))
    (when (called-interactively-p 'interactive)
      (with-current-buffer (get-buffer-create "*remote-ext self test*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (dolist (row results)
            (insert (format "%-4s %-26s %s\n"
                            (pcase (nth 2 row)
                              ('ok "[ok]") ('fail "[FAIL]") (_ "[--]"))
                            (nth 0 row) (nth 1 row))))
          (goto-char (point-min))
          (special-mode)))
      (display-buffer "*remote-ext self test*"))
    results))

;;;###autoload
(defun moyu/remote-cleanup ()
  "Drop cached remote state and close this buffer's TRAMP connection.
Needed after changing `moyu/remote-exec-path' or a connection-local
`tramp-remote-path': TRAMP caches the remote PATH per connection, so the
new value only takes effect on a fresh connection."
  (interactive)
  (clrhash moyu/remote--path-cache)
  (setq moyu/remote--prepared nil)
  (when (fboundp 'tramp-cleanup-this-connection)
    (ignore-errors (tramp-cleanup-this-connection)))
  (when (called-interactively-p 'interactive)
    (message "remote-ext: cache cleared and connection closed")))

(provide 'remote-ext)
;;; remote-ext.el ends here
