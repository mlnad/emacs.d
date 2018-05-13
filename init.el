;;; init.el --- emacs initilize file

;; Author: mark Liu <liumiaogemini@gmail.com>
;; License: See the LICENSE in the root directory.
;;
;;; Commentary:
;; This file is

;;; Code:
(setq debug-on-error t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defconst *is-a-win* (eq system-type 'windows-nt))
;;-----------------------------------------------------------------------

;; Packages
;;======================================================================
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'package)
(require 'cl)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") )
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(require 'user-fun)
(require-package 'dash)
(require-package 'use-package)
(require-package 'undo-tree)
(require-package 'multi-term)

(require 'dash)
(require 'keyboard)

;;======================================================================

;;; Basic
;; ===================================================================================
(setq-default make-backup-files nil ;; Don't make a backup file which end with "~"
              visible-bell t ;; Flash the frame to represent a bell
              auto-image-file-mode t
	      
              initial-scratch-message nil
              inhibit-splash-screen t
	      column-number-mode nil
	      line-number-mode nil

              initial-major-mode 'text-mode
	      )

;; Set font
(require-package 'cnfonts)

(when *is-a-win*
  (set-frame-font "Consolas 13")) ;; set when the OS is windows

(set-fontset-font "fontset-default" 'gb18030 '("DengXian" . "unicode-bmp"))

;; Language and coding
(set-language-environment 'Chinese-GB)
(set-keyboard-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; ===================================================================================

;;; Require *.el files
;; ===================================================================================
(defconst pack-dir
  (expand-file-name "elpa" user-emacs-directory )
  "Packages install by package-initilize.")

;; ===================================================================================

;;; Helm
;; ===================================================================================
(require-package 'helm)
(require-package 'helm-ebdb)
(require 'helm-config)
(helm-mode 1)
;; ===================================================================================

;;; Org
;; ===================================================================================
(require-package 'org)
(use-package org
  :init
  (require-package 'org2ctex)
  (require-package 'org-autolist)
  (require-package 'org-plus-contrib)
  
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (add-hook 'org-mode-hook 'turn-on-font-lock)
  (add-hook 'message-mode-hook 'turn-on-orgstruct)
  (add-hook 'message-mode-hook 'turn-on-orgstruct++)
  (add-hook 'org-mode-hook (lambda () (org-autolist-mode)));;enable org autolist
  (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
  (add-hook 'org-mode-hook 'iimage-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
)
;; ===================================================================================

;;; UI
;; ===================================================================================
(require-package 'atom-one-dark-theme)
(require-package 'diminish)
(require-package 'list-utils)

(setq-default custom-enabled-themes '(atom-one-dark))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

(add-hook 'tty-setup-hook 'tty-setup-theme)

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(recentf-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(electric-pair-mode 1)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(setq-default mode-line-format ;; set mode line
	      (quote
	       ("%e" ;; print error message
		mode-line-front-space
		mode-line-mule-info mode-line-client mode-line-modified
		mode-line-remote
		mode-line-frame-identification mode-line-buffer-identification ;; buffer files
		"   "
		"["
		mode-line-position ;; position of this buffer
		"/%I] "
		"  "
		mode-line-modes ;; Major mode and some important minor modes.
		(vc-mode vc-mode) ;; version control messages.
		mode-line-misc-info mode-line-end-spaces))) ;; end of mode line

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

(setq dim-list
      ;; minor modes list followed will not show in the mode line.
      '(helm-mode company-mode yas-minor-mode abbrev-mode org-autolist-mode
		  image-mode iimage-mode visual-line-mode eldoc-mode))
(hide-minor-mode dim-list)
(add-hook 'find-file-hook (lambda () (hide-minor-mode dim-list)))

(setq-default frame-title-format 'buffer-file-name
	      )
;; ===================================================================================

;;; Company
;; ===================================================================================
(require-package 'company)
(require-package 'company-quickhelp)

(require 'pos-tip)
(require 'company)
(add-hook 'prog-mode-hook 'company-mode) ;; Only load company mode when you are programming
(setq-default company-backends (delete 'company-semantic company-backends))

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
(add-hook 'after-init-hook 'company-quickhelp-mode)
;; ===================================================================================

;;; Yasnippet
;; ===================================================================================
(require-package 'yasnippet)
(require 'yasnippet)

(add-hook 'prog-mode-hook 'yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; ===================================================================================

;;; Deft
;; ===================================================================================
(require-package 'deft)
(setq-default deft-extensions '("org")
	      deft-directory "~/notebook"
	      deft-recursive t
	      )
;; ===================================================================================

;;; Flycheck
;; ===================================================================================
(require-package 'flycheck)
(add-hook 'prog-mode-hook 'flycheck-mode)
;; ===================================================================================

;;; Program
;;====================================================================================
(require-package 'go-mode)
(require-package 'company-go)
(require-package 'wolfram-mode)
(require-package 'matlab-mode)
(require-package 'markdown-mode)
(require-package 'lispy)
(require-package 'ecb)
(require-package 'magit)

;; c/cpp mode
(require 'c-cpp-dev)
(require 'java-dev)
(require 'python-dev)
;;====================================================================================

(when (file-exists-p custom-file)
  (load custom-file))


(provide 'init)
;;; init.el ends here
