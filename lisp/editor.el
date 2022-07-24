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
(add-hook 'find-file-not-found-functions
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
      backup-directory-alist configs/backup-directory-alist
      tramp-backup-directory-alist backup-directory-alist)

;;; Scrolling
(setq hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)

(setq-default auto-image-file-mode t)

(setq auto-save-list-file-prefix configs/auto-save-list-prefix)

(setq-default initial-scratch-message nil
              inhibit-splash-screen t
              initial-major-mode 'text-mode
              frame-title-format "%b")

;;; Formatting
(setq-default tab-width 4
	      indent-tabs-mode nil)
;; Make `tabify' and `untabify' only affect indentation. Not tabs/spaces in the
;; middle of a line.
(setq tabify-regexp "^\t* [ \t]+")

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

(add-hook 'emacs-startup-hook #'window-divider-mode)

;; Don't display floating tooltips;
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; Font set
(defun editor/init-font ()
  "Initialize Emacs font."
  (when (find-font (font-spec :name (car configs/default-font)))
    (let* ((font (car configs/default-font))
           (props (cdr configs/default-font))
           (fontspec (apply 'font-spec :name font props)))
      (set-frame-font fontspec nil t)))
  (when (find-font (font-spec :name (car configs/unicode-font)))
    (let* ((font (car configs/unicode-font))
           (props (cdr configs/unicode-font))
           (fontspec (apply 'font-spec :name font props)))
      (set-fontset-font t 'unicode fontspec)))
  (run-hooks 'after-setting-font-hook))

;; Theme set
(defun editor/init-theme ()
  "Initialize Emacs theme."
  (when (and configs/theme (not (custom-theme-enabled-p configs/theme)))
    (load-theme configs/theme t)))

;;; Build-in packages
;;; tramp
(unless *sys/win32*
  (setq tramp-default-method "ssh"
        tramp-auto-save-directory (expand-file-name "tramp-autosave/" configs/cache-directory)
        tramp-backup-directory-alist (expand-file-name "backup/" configs/cache-directory)))

(with-eval-after-load 'tramp
  (setq remote-file-name-inihibit-cache 60
        tramp-completion-reread-directory-timeout 60
        tramp-verbose 1))

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
  (setq recentf-save-file configs/recentf-save-file
	    recentf-max-saved-items 1000
	    recentf-auto-cleanup 'never)

  (recentf-mode 1))

(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode))

(use-package savehist
  :init
  ;; Minibuffer history
  (setq savehist-file (expand-file-name "savehist" configs/cache-directory))
  (savehist-mode)
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
  (setq save-place-file configs/save-place-file))

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
  :defer t)

(when (>= emacs-major-version 27)
  (use-package display-fill-column-indicator
    :ensure nil))

(use-package compile
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error))

(use-package emacs
  :config
  (delete-selection-mode 1)
  (electric-pair-mode 1)
  (size-indication-mode t)

  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  (defun crm-indicator (args)
    (cons (concat "[CRM]" (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (fset #'yes-or-no-p #'y-or-n-p)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  (setq tab-always-indent t)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t
        echo-keystrokes 0.02
        resize-mini-windows 'grow-only
        max-mini-window-height 0.15))

;;; Minibuffers

;; Try really hard to keep the cursor from getting stuce in the read-only prompt
;; portion of the minibuffer.
(setq minibuffer-prompt-properties
      '(read-only t intangible t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


;;; Modeline
(use-package doom-modeline
  :ensure t
  :hook (window-setup . doom-modeline-mode)
  :init
  (unless after-init-time
    (setq-default mode-line-format nil))
  :config
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name t
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-file-name-style 'relative-from-project)

  (use-package anzu
    :ensure t
    :defer t
    :hook (isearch-mode . anzu-mode)))

;;; doom themes
(use-package doom-themes
  :ensure t)

(let ((hook (if (daemonp)
                'server-after-make-frame-hook
              'after-init-hook)))
  (add-hook hook #'editor/init-font -100)
  (add-hook hook #'editor/init-theme -99))

;;; Undo tree mode
(use-package undo-tree
  :ensure t
  :hook (after-init . global-undo-tree-mode)
  :custom (undo-tree-history-directory-alist `(("." . ,(concat configs/cache-directory "undo-tree-hist/"))))
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        ;; 10X bump of the undo limits to avoid issues with premature
        ;; Emacs GC which truncages the undo history very aggresively
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000))

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
          writeroom-set-bottom-divider-width)))

;;; General - for keybindings
(use-package general
  :ensure t
  :init)

(use-package restart-emacs
  :ensure t)

(provide 'editor)
;;; editor.el ends here
