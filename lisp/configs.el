;;; configs.el -- configurations for my Emacs

;;; Commentary:

;;; Code:
(defvar user/full-name "Liu Miao")
(defvar user/email-address "liumiaogemini@foxmail.com")

(defconst *sys/win32*
  (eq system-type 'windows-nt))

(defconst *sys/linux*
  (eq system-type 'gnu/linux))

(defconst *sys/mac*
  (eq system-type 'darwin))

(defvar user/default-font '("Consolas"
                            :size 10
                            :weight normal
                            :width normal))

(defvar user/cache-directory
  (expand-file-name ".cache/" user-emacs-directory))

(defvar user/userconfig-file
  (expand-file-name "userconfig" user/cache-directory))

(defvar user/custom-file
  (expand-file-name "custom.el" user/cache-directory))

(defvar user/recentf-save-file
  (expand-file-name "recentf" user/cache-directory))

(defvar user/save-place-file
  (expand-file-name "places" user/cache-directory))

(defvar user/backup-directory-alist
  (expand-file-name "backup/" user/cache-directory))

(defvar user/projectile-known-projects-file
  (expand-file-name "projectile-bookmarks.eld" user/cache-directory))

(defvar user/projectile-cache-file
  (expand-file-name "projectile.cache" user/cache-directory))

(defvar user/auto-save-list-prefix
  (expand-file-name "auto-save-list/.saves-" user/cache-directory))

(defvar user/layouts-directory
  (expand-file-name "layouts/" user/cache-directory))

(defvar user/quelpa-dir
  (expand-file-name "quelpa/" user/cache-directory))

(defvar user/notes-dir "~/org"
  "User defined notes directory.")

(defvar user/org-roam-dir user/notes-dir
  "User defined org roam directory.")

(defvar user/notes-extensions '("org" "md" "markdown"))

(defvar user/rime-data-dir
  (expand-file-name "rime/" user/cache-directory))

(defvar elpa-pack-dir
  (expand-file-name "elpa" user-emacs-directory )
  "Packages install by package-initilize.")

(defvar elpa-subdirectory 'emacs-version)

(defvar default-package-mirror '(("melpa" . "https://melpa.org/packages/")
                                 ("org" . "https://orgmode.org/elpa/")
                                 ("gnu" . "https://elpa.gnu.org/packages/")))

(defvar emacs-china-package-mirror '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                                     ("melpa" . "http://elpa.emacs-china.org/melpa/")
                                     ("org"   . "http://elpa.emacs-china.org/org/")))

(defvar user/package-mirror default-package-mirror)

(defvar user/enable-org nil)

(defvar user/enable-org-roam nil)

(defvar user/enable-eaf nil)

(defvar user/org-journal-type 'daily)

(defvar user/org-src-block-face '(:family "Jetbrains Mono")
  "Face for org source block.")

(defvar user/roam-templates nil)

(provide 'configs)
;;; configs.el ends here
