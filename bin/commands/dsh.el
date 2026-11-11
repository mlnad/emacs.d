;;; dsh.el --- `moyue dsh' runtime installer  -*- lexical-binding: t; -*-

;;; Commentary:

;; Installs and manages the DeepSeek Harness runtime used by Emacs, entirely
;; under `.cache/dsh':
;;
;;   moyue dsh install [--version V] [--force]
;;   moyue dsh status
;;   moyue dsh plugin PROFILE [PNPM-ARGS...]
;;   moyue dsh help
;;
;; The runtime is the official self-contained `deepseek-harness-runtime-bin'
;; wheel: it embeds Node.js and the DSH CLI, so no system Node/npm/pnpm is
;; needed to boot profiles or the ACP server.  `dsh plugin' (external plugin
;; management) still needs pnpm on PATH; that is only for adding plugins and is
;; not required by `dsh-engineer'/`dsh-ask'.
;;
;; `install' also creates the two harness profiles used by lisp/dsh.el:
;;   engineer : full coding agent, workspace-write sandbox
;;   ask      : read-only Q&A agent, read-only sandbox
;; plus their shared base home.  Project homes are created lazily by dsh.el.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'url)

(defvar moyue-dsh--runtime-version "0.2.1rc2"
  "Pinned DSH runtime wheel version.
Override with `moyue dsh install --version V' for a one-off install.")

(defvar moyue-dsh--base-home-name "home"
  "Base DSH home directory name under the cache root.")

(defvar moyue-dsh--engineer-persona
  "You are DSH Engineer, an expert software engineer working inside the user's project. Inspect the repository before changing it, make focused edits, run relevant tests, and report what you changed and verified."
  "System prompt prefix for the engineer profile.")

(defvar moyue-dsh--ask-persona
  "You are DSH Ask, a careful read-only question-answering assistant. Answer questions about the current project by reading and searching; do not modify files or run commands that change repository or machine state. If a request needs changes, explain the proposed change instead."
  "System prompt prefix for the ask profile.")

;;;; Paths

(defun moyue-dsh--cache ()
  "Return the DSH cache directory.
MOYUE_DSH_CACHE overrides the default .cache/dsh under the configuration."
  (let ((env (getenv "MOYUE_DSH_CACHE")))
    (expand-file-name
     (if (and env (not (string-empty-p env)))
         (file-name-as-directory env)
       ".cache/dsh/")
     (moyue--config-root))))

(defun moyue-dsh--base-home ()
  "Return the base DSH home under the cache."
  (expand-file-name (concat moyue-dsh--base-home-name "/")
                    (moyue-dsh--cache)))

(defun moyue-dsh--versions-dir ()
  "Return the runtime versions directory."
  (expand-file-name "runtime/versions/" (moyue-dsh--cache)))

(defun moyue-dsh--version-dir (version)
  "Return the install directory for runtime VERSION."
  (expand-file-name (concat version "/") (moyue-dsh--versions-dir)))

(defun moyue-dsh--wrapper ()
  "Return the stable dsh wrapper path."
  (expand-file-name "bin/dsh" (moyue-dsh--cache)))

(defun moyue-dsh--manifest (version)
  "Return the manifest path for VERSION."
  (expand-file-name "manifest.json" (moyue-dsh--version-dir version)))

(defun moyue-dsh--platform-tag ()
  "Return the PyPI wheel platform tag for this machine."
  (pcase system-type
    ('gnu/linux
     (if (string-match-p "aarch64\\|arm64" system-configuration)
         "manylinux_2_28_aarch64"
       "manylinux_2_28_x86_64"))
    ('darwin
     (if (string-match-p "aarch64\\|arm64" system-configuration)
         "macosx_14_0_arm64"
       "macosx_14_0_x86_64"))
    ('windows-nt "win_amd64")
    (_
     (error "moyue dsh: no runtime-bin wheel for %s; use the npm/container route"
            system-type))))

;;;; HTTP and checksums

(defun moyue-dsh--http-get (url)
  "Fetch URL and return its body as a string.
Prefer curl (it honours the shell's proxy environment); fall back to
`url-retrieve-synchronously'."
  (if-let* ((curl (executable-find "curl")))
      (with-temp-buffer
        (unless (zerop (call-process curl nil t nil
                                     "--fail" "--silent" "--show-error"
                                     "--location" "--max-time" "120" url))
          (error "moyue dsh: failed to fetch %s" url))
        (buffer-string))
    (let ((url-show-status nil))
      (with-current-buffer (url-retrieve-synchronously url t t 120)
        (goto-char (point-min))
        (unless (search-forward "\n\n" nil t)
          (error "moyue dsh: malformed HTTP response for %s" url))
        (buffer-substring-no-properties (point) (point-max))))))

(defun moyue-dsh--download (url file)
  "Download URL to FILE, preferring curl."
  (make-directory (file-name-directory file) t)
  (if-let* ((curl (executable-find "curl")))
      (unless (zerop (call-process curl nil nil nil
                                   "--fail" "--silent" "--show-error"
                                   "--location" "--retry" "3"
                                   "--max-time" "900"
                                   "--output" file url))
        (error "moyue dsh: download failed: %s" url))
    (url-copy-file url file t))
  (unless (file-exists-p file)
    (error "moyue dsh: download produced no file: %s" file))
  file)

(defun moyue-dsh--sha256 (file)
  "Return the SHA-256 of FILE."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

;;;; Wheel resolution and extraction

(defun moyue-dsh--release-files (version)
  "Return the PyPI release file list for runtime VERSION."
  (let* ((json (json-parse-string
                (moyue-dsh--http-get
                 "https://pypi.org/pypi/deepseek-harness-runtime-bin/json")
                :object-type 'hash-table :array-type 'list))
         (releases (gethash "releases" json))
         (files (and releases (gethash version releases))))
    (unless files
      (error "moyue dsh: version %s not found on PyPI" version))
    files))

(defun moyue-dsh--wheel-file (version tag)
  "Return the release file plist for VERSION and TAG."
  (or (seq-find
       (lambda (file)
         (let ((name (gethash "filename" file)))
           (and (stringp name)
                (string-match-p "\\.whl\\'" name)
                (string-match-p (regexp-quote tag) name))))
       (moyue-dsh--release-files version))
      (error "moyue dsh: no wheel for %s on %s" version tag)))

(defun moyue-dsh--extract-wheel (wheel dest)
  "Extract WHEEL into DEST."
  (make-directory dest t)
  (cond
   ((executable-find "unzip")
    (unless (zerop (call-process "unzip" nil nil nil "-q" wheel "-d" dest))
      (error "moyue dsh: unzip failed for %s" wheel)))
   ((executable-find "python3")
    (unless (zerop (call-process "python3" nil nil nil
                                 "-m" "zipfile" "-e" wheel dest))
      (error "moyue dsh: python3 -m zipfile failed for %s" wheel)))
   ((executable-find "python")
    (unless (zerop (call-process "python" nil nil nil
                                 "-m" "zipfile" "-e" wheel dest))
      (error "moyue dsh: python -m zipfile failed for %s" wheel)))
   (t
    (error "moyue dsh: need unzip or python3 to extract %s" wheel)))
  (unless (file-directory-p
           (expand-file-name "deepseek_harness_runtime" dest))
    (error "moyue dsh: unexpected wheel layout in %s" wheel)))

(defun moyue-dsh--find-runtime-binary (version-dir)
  "Find the DSH native executable under VERSION-DIR."
  (let* ((runtime-dir (expand-file-name
                       "deepseek_harness_runtime/runtime/" version-dir))
         (files (and (file-directory-p runtime-dir)
                     (directory-files runtime-dir t
                                      "\\`deepseek-harness-sdk-runtime-")))
         (binary (seq-find
                  (lambda (file)
                    (and (file-regular-p file)
                         (not (string-match-p
                               "\\(?:-rg\\|-spawn-helper\\)\\'" file))))
                  files)))
    (unless binary
      (error "moyue dsh: runtime binary not found under %s" runtime-dir))
    binary))

;;;; Running DSH

(defun moyue-dsh--process-environment (home)
  "Return a DSH-specific process environment with HOME as DSH_HOME.
Strip inherited DSH_* variables (a shell launched from a DSH session carries
DSH_HOME/DSH_PROFILE/DSH_SESSION_ID), so install/profile operations never
touch the caller's DSH home."
  (let ((sensitive "\\`\\(?:DSH_[A-Za-z0-9_]*\\|MOYUE_DSH_[A-Za-z0-9_]*\\)="))
    (append (seq-remove (lambda (entry) (string-match-p sensitive entry))
                        process-environment)
            (list (format "DSH_HOME=%s" home)
                  (format "DSH_AGENTS_HOME=%s"
                          (expand-file-name "agents/" (moyue-dsh--cache)))))))

(defun moyue-dsh--run (binary home &rest args)
  "Run BINARY with ARGS under DSH home HOME.
Return stdout as a string.  Signal an error on a non-zero exit status."
  (let ((process-environment (moyue-dsh--process-environment home))
        (buffer (generate-new-buffer " *moyue-dsh*")))
    (unwind-protect
        (with-current-buffer buffer
          (let ((status (apply #'call-process binary nil t nil args)))
            (unless (zerop status)
              (error "moyue dsh: %s %s failed (%s): %s"
                     binary (string-join args " ") status
                     (buffer-string)))
            (buffer-string)))
      (kill-buffer buffer))))

(defun moyue-dsh--write-file (file content &optional mode)
  "Write CONTENT to FILE, creating parents; chmod MODE when non-nil."
  (make-directory (file-name-directory file) t)
  (with-temp-buffer
    (set-buffer-file-coding-system 'utf-8-unix)
    (insert content)
    (write-region (point-min) (point-max) file nil 'silent))
  (when mode (set-file-modes file mode))
  file)

;;;; Profiles and patches

(defun moyue-dsh--patch-yaml (persona sandbox)
  "Return a cordis.patch.yml string for PERSONA and SANDBOX.
SANDBOX is either nil or \"read-only\"; the engineer profile keeps the
default workspace-write policy.  `!!js process.cwd()' keeps the workspace
root tied to the project directory the wrapper cd's into."
  (concat
   "# Generated by `moyue dsh install'; edits are preserved unless --force.\n"
   (format "- id: system-prompt\n  config:\n    personaPrefix: >-\n      %s\n"
           persona)
   (when sandbox
     (format "- id: sandbox-policy\n  config:\n    mode: %s\n    workspaceRoot: !!js process.cwd()\n"
             sandbox))))

(defun moyue-dsh--profiles-dir (home)
  "Return the profiles directory under DSH home HOME."
  (expand-file-name "profiles/" home))

(defun moyue-dsh--empty-patch-p (file)
  "Return non-nil when FILE is the untouched default patch (a YAML empty list)."
  (and (file-readable-p file)
       (with-temp-buffer
         (insert-file-contents file)
         (string-match-p
          "\\`[[:space:]\n]*\\(?:#.*\n\\)*\\[\\][[:space:]\n]*\\'"
          (buffer-string)))))

(defun moyue-dsh--ensure-profiles (binary home force)
  "Create the acp/engineer/ask profiles under HOME using BINARY.
When FORCE is non-nil, rewrite the generated patches."
  (make-directory (moyue-dsh--profiles-dir home) t)
  ;; Initialize the shipped acp profile once; the other two are copies of
  ;; the same template with project-specific persona/policy patches.
  (moyue-dsh--run binary home "--profile" "acp" "--dump-config")
  (dolist (spec '(("engineer" . nil)
                  ("ask" . "read-only")))
    (let* ((name (car spec))
           (sandbox (cdr spec))
           (dir (expand-file-name (concat name "/")
                                  (moyue-dsh--profiles-dir home)))
           (patch (expand-file-name "cordis.patch.yml" dir))
           (created (not (file-directory-p dir))))
      (when created
        (moyue-dsh--run binary home
                        "--profile" name
                        "--from-default-profile" "acp"
                        "--dump-default-config"))
      ;; A freshly created profile ships an empty default patch.  Write our
      ;; generated patch when the profile was created, when --force is given,
      ;; or when the patch is missing.  Existing user edits are preserved.
      (when (or force created (not (file-exists-p patch))
                  (moyue-dsh--empty-patch-p patch))
        (moyue-dsh--write-file
         patch
         (moyue-dsh--patch-yaml
          (if (equal name "engineer")
              moyue-dsh--engineer-persona
            moyue-dsh--ask-persona)
          sandbox)))
      ;; Validate the composed tree; a broken patch must fail the install.
      (moyue-dsh--run binary home "--profile" name "--dump-config"))))

(defun moyue-dsh--write-wrapper (_binary relative)
  "Write the stable wrapper that executes the current runtime.
RELATIVE is the binary path under the version directory."
  (let* ((cache (directory-file-name (moyue-dsh--cache)))
         (script
          (concat
           "#!/bin/sh\n"
           "set -eu\n"
           (format "RT=%s\n" (shell-quote-argument cache))
           "if [ -n \"${DSH_PROJECT_ROOT:-}\" ] && [ -d \"$DSH_PROJECT_ROOT\" ]; then\n"
           "  cd \"$DSH_PROJECT_ROOT\"\n"
           "fi\n"
           ": \"${DSH_HOME:=$RT/home}\"\n"
           ": \"${DSH_AGENTS_HOME:=$RT/agents}\"\n"
           "export DSH_HOME DSH_AGENTS_HOME\n"
           (format "exec \"$RT/runtime/current/%s\" \"$@\"\n" relative))))
    (moyue-dsh--write-file (moyue-dsh--wrapper) script #o755)))

(defun moyue-dsh--switch-current (version-dir)
  "Point runtime/current at VERSION-DIR."
  (let* ((current (expand-file-name "runtime/current" (moyue-dsh--cache)))
         (parent (file-name-directory current)))
    (make-directory parent t)
    (when (file-symlink-p current)
      (delete-file current))
    (when (file-exists-p current)
      (error "moyue dsh: runtime/current exists and is not a symlink: %s"
             current))
    (condition-case err
        (make-symbolic-link version-dir current)
      (error
       (error "moyue dsh: cannot create symlink %s -> %s (%s); use a filesystem that supports symlinks"
              current version-dir (error-message-string err))))))

;;;; Commands

(defun moyue-dsh--install-runtime (version force)
  "Install runtime VERSION under the cache, unless already present.
FORCE re-downloads/replaces it.  Return the version directory."
  (let* ((cache (moyue-dsh--cache))
         (tag (moyue-dsh--platform-tag))
         (version-dir (moyue-dsh--version-dir version))
         (windows (eq system-type 'windows-nt)))
    (make-directory (expand-file-name "downloads/" cache) t)
    (if (and (file-directory-p version-dir) (not force))
        (message "moyue dsh: runtime %s already installed at %s"
                 version version-dir)
      (let* ((file (moyue-dsh--wheel-file version tag))
             (filename (gethash "filename" file))
             (url (gethash "url" file))
             (expected (gethash "sha256" (gethash "digests" file)))
             (wheel (expand-file-name filename
                                      (expand-file-name "downloads/" cache)))
             (staging (make-temp-file "moyue-dsh-runtime-" t)))
        (unwind-protect
            (progn
              (message "moyue dsh: downloading %s (%s)" filename url)
              (moyue-dsh--download url wheel)
              (let ((actual (moyue-dsh--sha256 wheel)))
                (unless (and expected (string= (downcase expected) actual))
                  (error "moyue dsh: SHA-256 mismatch for %s (expected %s, got %s)"
                         filename expected actual)))
              (message "moyue dsh: extracting runtime %s" version)
              (moyue-dsh--extract-wheel wheel staging)
              (when (and (file-directory-p version-dir) force)
                (unless (file-in-directory-p version-dir
                                             (moyue-dsh--versions-dir))
                  (error "moyue dsh: refusing to delete %s" version-dir))
                (delete-directory version-dir t))
              (make-directory (moyue-dsh--versions-dir) t)
              (rename-file (directory-file-name staging)
                           (directory-file-name version-dir))
              (setq staging nil)
              (let* ((binary (moyue-dsh--find-runtime-binary version-dir))
                     (relative (file-relative-name binary version-dir)))
                (unless windows
                  (set-file-modes binary #o755)
                  (let ((rg (concat (file-name-sans-extension binary) "-rg")))
                    (when (file-exists-p rg)
                      (set-file-modes rg #o755))))
                (moyue-dsh--switch-current version-dir)
                (moyue-dsh--write-wrapper binary relative)
                (moyue-dsh--write-file
                 (moyue-dsh--manifest version)
                 (json-encode
                  `(("name" . "deepseek-harness-runtime-bin")
                    ("version" . ,version)
                    ("platform" . ,tag)
                    ("wheel" . ,filename)
                    ("sha256" . ,(moyue-dsh--sha256 wheel))
                    ("binary" . ,relative)
                    ("installed_at" . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z")))
                  )
                 )))
          (when (and staging (file-directory-p staging))
            (delete-directory staging t)))))
    (let* ((binary (moyue-dsh--find-runtime-binary version-dir)))
      (moyue-dsh--switch-current version-dir)
      (moyue-dsh--write-wrapper binary
                                (file-relative-name binary version-dir)))
    version-dir))

(defun moyue-dsh--install-profiles (version force)
  "Ensure base home and the engineer/ask profiles for VERSION.
FORCE rewrites the generated profile patches."
  (let* ((binary (moyue-dsh--find-runtime-binary
                  (moyue-dsh--version-dir version)))
         (home (moyue-dsh--base-home)))
    (make-directory home t)
    (make-directory (expand-file-name "agents/" (moyue-dsh--cache)) t)
    (moyue-dsh--ensure-profiles binary home force)
    home))

(defun moyue-dsh--install-command (args)
  "Implement `moyue dsh install'."
  (let ((version moyue-dsh--runtime-version)
        (force nil))
    (while args
      (pcase (car args)
        ("--version"
         (setq version (cadr args))
         (unless version (error "moyue dsh install: --version needs a value"))
         (setq args (cdr args)))
        ("--force" (setq force t))
        (arg (error "moyue dsh install: unknown option %s" arg)))
      (setq args (cdr args)))
    (let ((version-dir (moyue-dsh--install-runtime version force)))
      (moyue-dsh--install-profiles version force)
      (message "moyue dsh: installed %s\n  runtime: %s\n  wrapper: %s\n  base home: %s\n  profiles: acp, engineer, ask"
               version version-dir (moyue-dsh--wrapper)
               (moyue-dsh--base-home)))))

(defun moyue-dsh--status-command ()
  "Implement `moyue dsh status'."
  (let* ((cache (moyue-dsh--cache))
         (versions (expand-file-name "runtime/versions/" cache))
         (current (expand-file-name "runtime/current" cache))
         (base-home (moyue-dsh--base-home))
         (profiles (expand-file-name "profiles/" base-home))
         (projects (expand-file-name "projects/" cache))
         (project-count 0))
    (when (file-directory-p projects)
      (setq project-count
            (length (seq-filter
                     (lambda (file)
                       (and (file-directory-p file)
                            (not (member (file-name-nondirectory file)
                                         '("." "..")))))
                     (directory-files projects t)))))
    (message
     (concat
      "DSH cache: %s\n"
      "runtime current: %s -> %s\n"
      "installed versions: %s\n"
      "wrapper: %s\n"
      "base home: %s\n"
      "profiles: %s\n"
      "project homes: %d\n"
      "runtime installed: %s")
     cache current (if (file-exists-p current) (file-truename current) "missing")
     (if (file-directory-p versions)
         (string-join (seq-filter
                       (lambda (name) (not (string-prefix-p "." name)))
                       (directory-files versions nil "\\`[^.]"))
                      ", ")
       "none")
     (moyue-dsh--wrapper)
     base-home
     (if (file-directory-p profiles)
         (string-join
          (seq-filter (lambda (name) (not (string-prefix-p "." name)))
                      (directory-files profiles nil "\\`[^.]"))
          ", ")
       "missing")
     project-count
     (if (and (file-executable-p (moyue-dsh--wrapper))
              (file-directory-p profiles))
         "yes"
       "no"))))

(defun moyue-dsh--plugin-command (args)
  "Implement `moyue dsh plugin PROFILE [PNPM-ARGS...]'.
This forwards to `dsh plugin', which needs pnpm on PATH."
  (let ((profile (pop args)))
    (unless profile
      (error "moyue dsh plugin: missing PROFILE (engineer or ask)"))
    (unless (executable-find "pnpm")
      (error "moyue dsh plugin: pnpm is not on PATH; install pnpm to add external DSH plugins"))
    (let* ((version (car (seq-filter
                          (lambda (name) (not (string-prefix-p "." name)))
                          (directory-files
                           (moyue-dsh--versions-dir) nil "\\`[^.]"))))
           (binary (moyue-dsh--find-runtime-binary
                    (moyue-dsh--version-dir version)))
           (home (moyue-dsh--base-home)))
      (message "%s"
               (apply #'moyue-dsh--run binary home
                      "plugin" "--profile" profile args)))))

(defun moyue-dsh--help-command ()
  "Print `moyue dsh' help."
  (message
   (concat
    "Usage: moyue dsh COMMAND [ARGS...]\n\n"
    "Commands:\n"
    "  install [--version V] [--force]   install runtime + engineer/ask profiles\n"
    "  status                            show runtime, profiles and project homes\n"
    "  plugin PROFILE [PNPM-ARGS...]     manage external plugins (needs pnpm)\n"
    "  help                              this text\n\n"
    "The runtime is self-contained (no system Node.js).  Runtime cache: %s"
    (moyue-dsh--cache))))

(moyue-defcommand "dsh" "install|status|plugin|help [ARGS...]"
                  "Manage the independent DSH runtime under .cache/dsh."
  (let ((sub (pop args)))
    (pcase sub
      ("install" (moyue-dsh--install-command args))
      ("status" (moyue-dsh--status-command))
      ((pred null) (moyue-dsh--status-command))
      ("plugin" (moyue-dsh--plugin-command args))
      ("help" (moyue-dsh--help-command))
      (_ (error "moyue dsh: unknown command %s (try `moyue dsh help')" sub)))))

(provide 'moyue-dsh)
;;; dsh.el ends here
