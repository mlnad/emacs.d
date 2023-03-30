;;; editor.el --- configs for Emacs settings
;;
;;; Commentary:
;;
;;; Code:
;; Resolve symlinks when opening files
(setq find-file-visit-truename t
      vc-follow-symlinks t
      find-file-suppress-same-file-warnings t
      create-lockfiles nil
      make-backup-files nil
      ring-bell-function 'ignore
      ;; build-in packages
      version-control t
      backup-by-copying t
      delete-old-versions t
      kept-old-versions 5
      kept-new-versions 5
      backup-directory-alist configs/backup-directory-alist
      tramp-backup-directory-alist backup-directory-alist
      hscroll-margin 2
      hscroll-step 1
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2
      auto-save-list-file-prefix configs/auto-save-list-prefix
      tabify-regexp "^\t* [ \t]+")

(setq-default auto-image-file-mode t
              initial-scratch-message nil
              inhibit-splash-screen t
              initial-major-mode 'text-mode
              frame-title-format "%b"
              tab-width 4
	      indent-tabs-mode nil
              fill-column 80
              word-wrap t
              truncate-lines t)

(when (>= emacs-major-version 28)
  (setq-default word-wrap-by-category t))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Default to soft line-wrapping in text modes.
(add-hook 'text-mode-hook #'visual-line-mode)

;; Create missing directory when we open a file that doesn't exist under
;; a directory tree tha may not exist.
(add-hook 'find-file-not-found-functions #'core/create-if-not-found)

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
  (add-hook 'find-file-hook (lambda ()
                              (unless recentf-mode
                                (recentf-mode)
                                (recentf-track-opened-file))))
  (setq recentf-save-file configs/recentf-save-file
        recentf-max-saved-items 1000
        recentf-auto-cleanup 'never)

  (recentf-mode 1))

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
  (use-package display-fill-column-indicator))

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

(use-package restart-emacs
  :ensure t)

(provide 'editor)
;;; editor.el ends here
