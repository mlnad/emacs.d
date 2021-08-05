;;; editor.el --- configs for Emacs settings
;;
;;; Commentary:
;;
;;; Code:

;; Resolve symlinks when opening files
(setq find-file-visit-truename t
      vc-follow-symlinks t)

(setq find-file-suppress-same-file-warnings t)

;; Create missing directory when we open a file that doesn't exist under
;; a directory tree tha may not exist.
(add-hook 'find-file-not-found-hooks
	  (lambda ()
	    (unless (file-remote-p buffer-file-name)
	      (let ((parent-directory (file-name-directory buffer-file-name)))
		(and (not (file-directory-p parent-directory))
		     (y-or-n-p (format "Directory `%s' does not exist! Create it? "
				       parent-directory))
		     (progn (make-directory parent-directory)
			    t))))))

;; Don't generate backups or lockfiles.
(setq create-lockfiles nil
      make-backup-files nil
      ring-bell-function 'ignore
      ;; build-in packages
      version-control t
      backup-by-copying t
      delete-old-versions t
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist user/backup-directory-alist
      tramp-backup-directory-alist backup-directory-alist)

(setq-default scroll-step 1) ;; smooth scroll

(setq-default auto-image-file-mode t)

(setq auto-save-list-file-prefix user/auto-save-list-prefix)

(setq-default initial-scratch-message nil
              inhibit-splash-screen t
              initial-major-mode 'text-mode
              frame-title-format "%b")

;;; Formatting
(setq-default indent-tabs-mode nil
              tab-width 4
              tab-always-indent nil)

(setq-default fill-column 80)

;; Word wrap by category. Break up for CJK.
(setq-default word-wrap t)
(when (>= emacs-major-version 28)
  (setq-default word-wrap-by-category t))

;; Default to soft line-wrapping in text modes.
(add-hook 'text-mode-hook #'visual-line-mode)

;; truncate-lines only on prog mode. Then it will not break lines.
(setq-default truncate-lines t)
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

(unless (assq 'menu-bar-lines default-frame-alist)
  (add-to-list 'default-frame-alist '(menu-bar-lines . 0))
  (add-to-list 'default-frame-alist '(tool-bar-lines . 0))
  (add-to-list 'default-frame-alist '(vertical-scroll-bars)))

(delete-selection-mode 1)
(electric-pair-mode 1)
(size-indication-mode t)

(add-hook 'emacs-startup-hook #'window-divider-mode)

;; Don't display floating tooltips;
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; Font set
(when (find-font (font-spec :name (car user/default-font)))
  (let* ((font (car user/default-font))
         (props (cdr user/default-font))
         (fontspec (apply 'font-spec :name font props)))
    (set-frame-font fontspec nil t)))

;;; Build-in packages
;;; tramp
(unless *sys/win32*
  (setq tramp-default-method "ssh"))

(use-package paren
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package recentf
  :commands (recentf-save-list)
  :init
  (add-hook 'find-file-hook (lambda () (unless recentf-mode
					                     (recentf-mode)
					                     (recentf-track-opened-file))))
  (setq recentf-save-file user/recentf-save-file
	    recentf-max-saved-items 1000
	    recentf-auto-cleanup 'never)

  (recentf-mode 1)
  :config
  (user/set-global-leader-key "fr" 'recentf-open-files))

(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode))

(use-package savehist
  :init
  ;; Minibuffer history
  (setq savehist-file (expand-file-name "savehist" user/cache-directory))
  (savehist-mode 1)
  :config
  (setq savehist-save-minibuffer-history t
        history-length 100
        savehist-autosave-interval 60
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history
                                        kill-ring)))

(use-package saveplace
  :hook (after-init . save-place-mode)
  :init
  (setq save-place-file user/save-place-file))

(use-package subword
  :hook (after-init . global-subword-mode)
  :diminish subword-mode)

(use-package winner-mode
  :ensure nil
  :hook (after-init . winner-mode))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

(use-package imenu
  :defer t
  :config
  (user/set-global-leader-key "ji" 'imenu))

(when (>= emacs-major-version 27)
  (use-package display-fill-column-indicator
    :ensure nil))

;;; Minibuffers
;; Allow for minibuffer-ception.
(setq enable-recursive-minibuffers t)

(setq echo-keystrokes 0.02)
(setq resize-mini-windows 'grow-only
      max-mini-window-height 0.15)

(fset #'yes-or-no-p #'y-or-n-p)

;; Try really hard to keep the cursor from getting stuce in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;;; Pop windows
(use-package popwin
  :ensure t
  :config
  (progn
    (popwin-mode 1)
    (setq popwin:special-display-config nil)

    ;; buffer that we manage
    (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect nil   :height 0.4) popwin:special-display-config)
    (push '("*Process List*"         :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
    (push '(compilation-mode         :dedicated nil :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
    (push '(dap-server-log-mode      :dedicated nil :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
    (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*undo-tree*"            :dedicated t :position right  :stick t :noselect nil :width   60) popwin:special-display-config)
    (push '("*undo-tree Diff*"       :dedicated t :position bottom :stick t :noselect nil :height 0.3) popwin:special-display-config)
    (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
    (push '("^\*WoMan.+\*$" :regexp t             :position bottom                                   ) popwin:special-display-config)
    (push '("*Google Translate*"     :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
    ))


;;; Modeline
(use-package doom-modeline
  :ensure t
  :hook (window-setup . doom-modeline-mode)
  :init
  (unless after-init-time
    (setq-default mode-line-format nil))

  (setq projectile-dynamic-mode-line nil)

  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-file-name-style 'relative-from-project)

  (use-package anzu
    :ensure t
    :defer t
    :hook (isearch-mode . anzu-mode))

  (use-package evil-anzu
    :ensure t
    :defer t
    :config
    (global-anzu-mode +1)))

(use-package unicode-fonts
  :ensure t
  :init
  (setq unicode-fonts-skip-font-groups '(decorative low-quality-glyphs))
  (unicode-fonts-setup))

;;; doom themes
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

;;; Undo tree mode
(use-package undo-tree
  :ensure t
  :defer t
  :init
  (progn
    (setq undo-tree-visualizer-timestamps t
          undo-tree-visualizer-diff t
          ;; 10X bump of the undo limits to avoid issues with premature
          ;; Emacs GC which truncages the undo history very aggresively
          undo-limit 800000
          undo-strong-limit 12000000
          undo-outer-limit 120000000)
    (global-undo-tree-mode)))

(use-package writeroom-mode
  :ensure t
  :config
  (setq writeroom-width 100)
  (setq writeroom-global-effects
        '(;; writeroom-set-fullscreen
          writeroom-set-alpha
          writeroom-set-menu-bar-lines
          writeroom-set-tool-bar-lines
          writeroom-set-vertical-scroll-bars
          writeroom-set-bottom-divider-width))
  (user/set-global-leader-key "wc" 'writeroom-mode))

(provide 'editor)
;;; editor.el ends here
