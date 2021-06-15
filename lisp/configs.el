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

(defvar user/org-default-font '("等距更纱黑体 SC"
                                :height 15
                                :weight normal
                                :width normal))

(defvar user/cache-directory
  (expand-file-name "cache/" user-emacs-directory))

(defvar user/userconfig-file
  (expand-file-name "cache/userconfig" user-emacs-directory))

(defvar user/custom-file
  (expand-file-name "cache/custom.el" user-emacs-directory))

(defvar user/recentf-save-file
  (expand-file-name "cache/recentf" user-emacs-directory))

(defvar user/save-place-file
  (expand-file-name "cache/places" user-emacs-directory))

(defvar user/backup-directory-alist
  (expand-file-name "cache/backup/" user-emacs-directory))

(defvar user/projectile-known-projects-file
  (expand-file-name "cache/projectile-bookmarks.eld" user-emacs-directory))

(defvar user/projectile-cache-file
  (expand-file-name "cache/projectile.cache" user-emacs-directory))

(defvar user/auto-save-list-prefix
  (expand-file-name "cache/auto-save-list/.saves-" user-emacs-directory))

(defvar user/layouts-directory
  (expand-file-name "cache/layouts/" user-emacs-directory))

(defvar user/notes-dir "~/org"
  "User defined notes directory.")

(defvar user/org-roam-dir "~/org/roam"
  "User defined org roam directory.")

(defvar user/notes-extensions '("org" "md" "markdown"))

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

(defvar user/lsp-client 'lsp-mode
  "If `nox' use nox as lsp client.
If `lsp-mode' use lsp-mode as lsp client.")

(defvar user/evil-collection-mode-list
  '(ag apropos bm bookmark
       (buff-menu "buff-menu")
       calc calendar
       cus-theme debug dictionary diff-mode dired dired-sidebar disk-usage doc-view docker ebib edbi edebug ediff eglot explain-pause-mode elfeed elisp-mode elisp-refs elisp-slime-nav emms epa ert eshell eval-sexp-fu evil-mc eww finder free-keys geiser ggtags git-timemachine gnus go-mode grep guix hackernews helm help helpful hg-histedit hungry-delete ibuffer image image-dired image+ imenu imenu-list
       (indent "indent")
       lispy log-edit log-view lsp-ui-imenu man magit magit-todos neotree nov
       (occur replace)
       org-present outline
       (package-menu package)
       pass
       (pdf pdf-view)
       popup proced
       (process-menu simple)
       prodigy profiler python quickrun realgud reftex restclient rg ripgrep scroll-lock sh-script simple slime sly speedbar tab-bar tablist tabulated-list tar-mode
       (term term ansi-term multi-term)
       tetris thread timer-list vc-annotate vc-dir vc-git vdiff view vlf vterm wdired wgrep woman xref
       (ztree ztree-diff)
       xwidget)
  )


(provide 'configs)
;;; configs.el ends here
