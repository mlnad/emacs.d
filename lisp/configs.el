;;; configs.el -- configurations for my Emacs

;;; Commentary:

;;; Code:
(defvar user/userconfig-file
  (expand-file-name "cache/userconfig" user-emacs-directory))

(defvar user/custom-file
  (expand-file-name "cache/custom.el" user-emacs-directory))

(defvar user/recentf-save-file
  (expand-file-name "cache/recentf" user-emacs-directory))

(defvar user/save-place-file
  (expand-file-name "cache/places" user-emacs-directory))

(defvar user/projectile-known-projects-file
  (expand-file-name "cache/projectile-bookmarks.eld" user-emacs-directory))

(defvar user/projectile-cache-file
  (expand-file-name "cache/projectile.cache" user-emacs-directory))

(defvar user/auto-save-list-prefix
  (expand-file-name "cache/auto-save-list/.saves-" user-emacs-directory))

(defvar user/notes-dir "~/org"
  "User defined notes directory.")

(defvar user/notes-extensions '("org" "md" "markdown"))

(defvar user/mode-line-format
  (list
   "%e" ;; print error message
   mode-line-front-space
   '(:eval evil-mode-line-tag) ;; Show evil mode.
   mode-line-mule-info mode-line-client mode-line-modified
   mode-line-remote
   mode-line-frame-identification mode-line-buffer-identification ;; buffer files
   mode-line-modes ;; Major mode and some important minor modes.
   " "
   mode-line-position ;; position of this buffer
   ;; "   "
   '(vc-mode vc-mode) ;; version control messages.
   mode-line-misc-info mode-line-end-spaces))

(defvar elpa-pack-dir
  (expand-file-name "elpa" user-emacs-directory )
  "Packages install by package-initilize.")

(defvar default-package-mirror '(("melpa" . "https://melpa.org/packages/")
                                 ("org" . "https://orgmode.org/elpa/")
                                 ("gnu" . "https://elpa.gnu.org/packages/"))
  )

(defvar emacs-china-package-mirror '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                                     ("melpa" . "http://elpa.emacs-china.org/melpa/")
                                     ("org"   . "http://elpa.emacs-china.org/org/"))
  )

(defvar user/package-mirror default-package-mirror)

(defvar user/nox-server-programs
  '((rust-mode . (nox-rls "rls"))
    ((c++-mode c-mode) . ("clangd"))
    (python-mode . ("pyls" "-v" "--tcp" "--host" "localhost" "--port" :autoport))
    (haskell-mode . (nox-hie "hie-wrapper" "--lsp"))
    ))

(defvar user/nox-list
  (list
    'python-mode-hook
    'c-mode-hook
    'c-mode-common-hook
    'c++-mode-hook
    'haskell-mode-hook))

(defvar user/lsp-client 'nox
  "If `nox' use nox as lsp client.
If `lsp-mode' use lsp-mode as lsp client.")

(provide 'configs)
;;; configs.el ends here
