;;; dsh.el --- DeepSeek Harness clients for agent-shell  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Liu

;; Author: Liu
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.94.2"))
;; Keywords: tools, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Two ACP agent configurations for DeepSeek Harness (DSH), built on
;; `agent-shell':
;;
;;   dsh-engineer  full coding harness (profile "engineer", workspace-write)
;;   dsh-ask       read-only Q&A harness (profile "ask", read-only sandbox)
;;
;; The runtime is *not* started from this file and is independent from Emacs:
;; it lives in `dsh-cache-directory' (default ~/.emacs.d/.cache/dsh), installed
;; by `moyue dsh install'.  Emacs starts it as a separate process over ACP
;; stdio through the stable wrapper `.cache/dsh/bin/dsh'.
;;
;; Per-project isolation is implemented inside DSH: each project gets its
;; own DSH_HOME (sessions, settings, credentials, SQLite caches), while the
;; installed profiles are shared through a symlink to the base home.  The DSH
;; process is started with `DSH_PROJECT_ROOT' set to the project root (the
;; wrapper cd's there), so the workspace, file sandbox root, and PTY working
;; directory are project-local.
;;
;; API key resolution (first match wins):
;;   1. `moyu/dsh-api-key' from .cache/userconfig (add the default in init.org)
;;   2. the DEEPSEEK_API_KEY environment variable
;;   3. `moyu/ai-api-key' when `moyu/ai-provider' is a DeepSeek provider
;;
;; Usage:
;;   M-x dsh-engineer
;;   M-x dsh-ask
;; or select "DSH Engineer"/"DSH Ask" from `M-x agent-shell'.
;;
;; Prerequisites:
;;   moyue dsh install      ; installs runtime + engineer/ask profiles/plugins
;;   M-x package-install RET agent-shell RET
;;   set `moyu/dsh-api-key' (or DEEPSEEK_API_KEY) in .cache/userconfig.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'project)

(eval-when-compile
  (defvar agent-shell-agent-configs)
  (declare-function agent-shell-make-agent-config "agent-shell")
  (declare-function agent-shell--make-acp-client "agent-shell")
  (declare-function agent-shell--dwim "agent-shell")
  (declare-function agent-shell-welcome-message "agent-shell"))

(defgroup dsh nil
  "DeepSeek Harness (DSH) clients for Emacs."
  :group 'tools
  :prefix "dsh-")

(defcustom dsh-cache-directory
  (or (getenv "MOYUE_DSH_CACHE")
      (expand-file-name
       "dsh/"
       (if (boundp 'moyu/cache-directory)
           moyu/cache-directory
         (expand-file-name ".cache/" user-emacs-directory))))
  "Directory managed by `moyue dsh install'.
It contains the runtime, the base DSH home, project homes, logs and the
stable `bin/dsh' wrapper.  Set MOYUE_DSH_CACHE to share a location with the
moyue CLI; otherwise the default matches it."
  :type 'directory)

(defcustom dsh-engineer-profile "engineer"
  "DSH profile used by `dsh-engineer'."
  :type 'string)

(defcustom dsh-ask-profile "ask"
  "DSH profile used by `dsh-ask'."
  :type 'string)

(defcustom dsh-default-config-options nil
  "ACP session config options applied to both DSH agents.
An alist of (OPTION . VALUE), both strings, as advertised by the DSH ACP
server (for example ((\"model\" . \"deepseek-v4-flash\")))."
  :type '(alist :key-type string :value-type string))

(defcustom dsh-extra-environment nil
  "Extra environment variables passed to the DSH process.
A list of KEY=VALUE strings appended after the variables dsh.el sets.
Use this for e.g. DEEPSEEK_BASE_URL or a custom proxy; do not put the API
key here because `dsh--api-key' already handles it."
  :type '(repeat string))

(defcustom dsh-project-home-symlink-profiles t
  "Whether project DSH homes symlink the shared `profiles' directory.
When non-nil (default), all projects share the installed profiles while
keeping sessions/settings/credentials separate.  Set to nil to copy profiles
per project instead (more isolation, more disk use)."
  :type 'boolean)

;;;; Paths and installation checks

(defun dsh--base-home ()
  "Return the base DSH home (profiles, shared runtime state)."
  (expand-file-name "home/" dsh-cache-directory))

(defun dsh--agents-home ()
  "Return the shared DSH agents configuration directory."
  (expand-file-name "agents/" dsh-cache-directory))

(defun dsh--wrapper ()
  "Return the stable DSH wrapper script path."
  (expand-file-name "bin/dsh" dsh-cache-directory))

(defun dsh--runtime-current ()
  "Return the current DSH runtime directory."
  (expand-file-name "runtime/current/" dsh-cache-directory))

(defun dsh-installed-p ()
  "Return non-nil when the DSH runtime and base profiles are installed."
  (and (file-executable-p (dsh--wrapper))
       (file-directory-p (expand-file-name "profiles/" (dsh--base-home)))))

(defun dsh--ensure-installed ()
  "Signal a user error with installation instructions unless DSH is installed."
  (unless (dsh-installed-p)
    (user-error
     "DSH runtime is not installed.  Run `moyue dsh install' (installs under %s)"
     dsh-cache-directory)))

;;;; API key

(defun dsh--nonempty-string (value)
  "Return VALUE when it is a non-empty string, else nil."
  (and (stringp value) (not (string-empty-p (string-trim value)))
       (string-trim value)))

(defun dsh--deepseek-provider-p ()
  "Return non-nil when `moyu/ai-provider' selects a DeepSeek provider."
  (and (boundp 'moyu/ai-provider)
       (member moyu/ai-provider '("deepseek" "deepseek-flash" "deepseek-pro"))))

(defun dsh--api-key ()
  "Return the DeepSeek API key for DSH.
Resolution order: `moyu/dsh-api-key' (userconfig), DEEPSEEK_API_KEY, then
`moyu/ai-api-key' when the configured provider is DeepSeek.  Signal a
user error when none is available."
  (let ((key (or (and (boundp 'moyu/dsh-api-key)
                      (dsh--nonempty-string moyu/dsh-api-key))
                 (dsh--nonempty-string (getenv "DEEPSEEK_API_KEY"))
                 (and (dsh--deepseek-provider-p)
                      (boundp 'moyu/ai-api-key)
                      (dsh--nonempty-string moyu/ai-api-key)))))
    (unless key
      (user-error
       "No DeepSeek API key.  Set `moyu/dsh-api-key' in ~/.emacs.d/.cache/userconfig, or export DEEPSEEK_API_KEY"))
    key))

;;;; Project isolation

(defun dsh--project-root (&optional buffer)
  "Return the local project root for BUFFER (or the current buffer).
Uses `project-current'; falls back to `default-directory'."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((dir (expand-file-name default-directory))
           (root (or (when-let* ((project (project-current nil dir)))
                       (project-root project))
                     dir)))
      (setq root (file-truename (directory-file-name root)))
      (when (file-remote-p root)
        (user-error "DSH project isolation requires a local project, got %s"
                    root))
      root)))

(defun dsh--project-id (root)
  "Return a stable, filesystem-safe id for project ROOT."
  (let* ((root (directory-file-name (file-truename root)))
         (base (file-name-nondirectory root))
         (base (if (string-empty-p base) "project" base))
         (clean (replace-regexp-in-string
                 "[^[:alnum:]._-]" "_" base))
         (hash (substring (secure-hash 'sha1 root) 0 12)))
    (format "%s-%s" clean hash)))

(defun dsh--project-home (root)
  "Ensure and return the isolated DSH home for project ROOT."
  (dsh--ensure-installed)
  (let* ((base-home (dsh--base-home))
         (base-profiles (expand-file-name "profiles/" base-home))
         (home (expand-file-name
                (format "projects/%s/home/" (dsh--project-id root))
                dsh-cache-directory))
         (link (expand-file-name "profiles" home)))
    (unless (file-directory-p base-profiles)
      (user-error "DSH profiles are missing; run `moyue dsh install'"))
    (make-directory home t)
    (if dsh-project-home-symlink-profiles
        (unless (file-symlink-p link)
          (when (file-exists-p link)
            (delete-directory link t))
          (make-symbolic-link base-profiles link))
      (unless (file-exists-p link)
        (copy-directory base-profiles link t t)))
    home))

(defun dsh--environment (root home)
  "Return the process environment for DSH in project ROOT with HOME.
The current Emacs environment is inherited except for DSH_* and
DEEPSEEK_API_KEY entries, which are replaced so a shell that was launched
from a DSH session cannot leak its profile/home into the agent."
  (make-directory (dsh--agents-home) t)
  (let* ((sensitive "\\`\\(?:DSH_[A-Za-z0-9_]*\\|DEEPSEEK_API_KEY\\|MOYUE_DSH_[A-Za-z0-9_]*\\)=")
         (base (seq-remove
                (lambda (entry) (string-match-p sensitive entry))
                process-environment)))
    (append base
            (list (format "DSH_HOME=%s" home)
                  (format "DSH_PROJECT_ROOT=%s" root)
                  (format "DSH_AGENTS_HOME=%s" (dsh--agents-home))
                  (format "DEEPSEEK_API_KEY=%s" (dsh--api-key)))
            dsh-extra-environment)))

;;;; agent-shell client and configurations

(defun dsh--client-maker (buffer profile)
  "Create a DSH ACP client for BUFFER using profile PROFILE.
Computes the project root and the per-project DSH_HOME, and passes the
resulting environment to the DSH wrapper."
  (dsh--ensure-installed)
  (let* ((root (dsh--project-root buffer))
         (home (dsh--project-home root))
         (environment (dsh--environment root home)))
    (let ((default-directory root))
      (agent-shell--make-acp-client
       :command (dsh--wrapper)
       :command-params (list "--profile" profile)
       :environment-variables environment
       :context-buffer buffer))))

(defun dsh--make-agent-config (profile identifier mode-line buffer prompt)
  "Build an agent-shell config for PROFILE.
IDENTIFIER, MODE-LINE, BUFFER and PROMPT control the displayed agent."
  (require 'agent-shell)
  (agent-shell-make-agent-config
   :identifier identifier
   :mode-line-name mode-line
   :buffer-name buffer
   :shell-prompt prompt
   :shell-prompt-regexp (regexp-quote prompt)
   :welcome-function #'agent-shell-welcome-message
   :client-maker (lambda (arg-buffer)
                   (dsh--client-maker arg-buffer profile))
   :default-config-options (lambda () dsh-default-config-options)
   :install-instructions
   (format "Run `moyue dsh install' and set `moyu/dsh-api-key' (or DEEPSEEK_API_KEY).  Cache: %s"
           dsh-cache-directory)))

;;;###autoload
(defun dsh-engineer-config ()
  "Return the DSH Engineer agent-shell configuration."
  (dsh--make-agent-config dsh-engineer-profile
                          'dsh-engineer
                          "DSH Engineer"
                          "DSH Engineer"
                          "DSH Engineer> "))

;;;###autoload
(defun dsh-ask-config ()
  "Return the DSH Ask (read-only Q&A) agent-shell configuration."
  (dsh--make-agent-config dsh-ask-profile
                          'dsh-ask
                          "DSH Ask"
                          "DSH Ask"
                          "DSH Ask> "))

(defun dsh--register-agent-configs ()
  "Register the DSH agent configurations with agent-shell.
Do nothing if agent-shell is not loaded yet; `with-eval-after-load' below
retries at load time."
  (when (boundp 'agent-shell-agent-configs)
    (let ((configs (symbol-value 'agent-shell-agent-configs)))
      (when (listp configs)
        (dolist (maker (list #'dsh-engineer-config #'dsh-ask-config))
          (unless (memq maker configs)
            (setq configs (append configs (list maker)))))
        (set 'agent-shell-agent-configs configs)))))

(with-eval-after-load 'agent-shell
  (dsh--register-agent-configs))

(defun dsh--start (profile)
  "Start the agent-shell DSH agent for PROFILE in the current project."
  (require 'agent-shell)
  (dsh--ensure-installed)
  (dsh--api-key)                        ; fail early with a clear message
  (let* ((root (dsh--project-root (current-buffer)))
         (default-directory root)
         (config (if (equal profile dsh-engineer-profile)
                     (dsh-engineer-config)
                   (dsh-ask-config))))
    (agent-shell--dwim :config config :new-shell t)))

;;;###autoload
(defun dsh-engineer ()
  "Start the DSH Engineer agent (full coding harness) for this project."
  (interactive)
  (dsh--start dsh-engineer-profile))

;;;###autoload
(defun dsh-ask ()
  "Start the DSH Ask agent (read-only Q&A harness) for this project."
  (interactive)
  (dsh--start dsh-ask-profile))

;;;###autoload
(defun dsh-status ()
  "Show the installed DSH runtime, base home and project isolation roots."
  (interactive)
  (let* ((manifest (expand-file-name "manifest.json" (dsh--runtime-current)))
         (data (when (file-readable-p manifest)
                 (ignore-errors
                   (json-parse-string
                    (with-temp-buffer
                      (insert-file-contents manifest)
                      (buffer-string))
                    :object-type 'hash-table))))
         (projects (expand-file-name "projects/" dsh-cache-directory)))
    (message
     "DSH cache: %s\nruntime: %s\nversion: %s\nbase home: %s\nprojects: %s\ninstalled: %s"
     dsh-cache-directory
     (dsh--runtime-current)
     (if data (gethash "version" data) "?")
     (dsh--base-home)
     (if (file-directory-p projects)
         (length (seq-filter (lambda (f) (file-directory-p f))
                             (directory-files projects t "\\`[^.]")))
       0)
     (if (dsh-installed-p) "yes" "no (run `moyue dsh install')"))))

(provide 'dsh)
;;; dsh.el ends here
