;;; init.el --- emacs initilize file

;; Author: mark Liu <liumiaogemini@gmail.com>
;; License: See the LICENSE in the root directory.
;;
;;; Commentary:
;; This file is

;;; Code:

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'no-error 'no-message)
(defun emacs-custom-settings()
  )

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
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
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))
(require 'user-fun)

(require 'dash)
(require 'pos-tip)
(require 'keyboard)
;;======================================================================

;;; Basic
;; ===================================================================================
(load-theme 'atom-one-dark)
(add-hook 'tty-setup-hook 'tty-setup-theme)
(setq-default make-backup-files nil ;; Don't make a backup file which end with "~"
              visible-bell t ;; Flash the frame to represent a bell
              auto-image-file-mode t
              initial-scratch-message nil
              inhibit-splash-screen t
              initial-major-mode 'text-mode
              )

;; Set font
(set-frame-font "Consolas 11")
;;(set-frame-font "Source Code Pro 11")
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

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; ===================================================================================

;;; Helm
;; ===================================================================================
(require 'helm-config)
(helm-mode 1)
;; ===================================================================================

;;; Org
;; ===================================================================================
(use-package org
  :init
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
;; (set-background-color "white smoke")

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;; (require 'powerline)
;; (powerline-default-theme)
(setq dim-list
      '(helm-mode company-mode yas-minor-mode abbrev-mode org-autolist-mode
		  image-mode iimage-mode visual-line-mode))
(hide-minor-mode dim-list)
(add-hook 'find-file-hook (lambda () (hide-minor-mode dim-list)))

(setq-default frame-title-format 'buffer-file-name
;; 	      powerline-display-hud nil
	      )
;; ===================================================================================

;;; Company
;; ===================================================================================
(require 'company)
;;(require 'init-ac-company)
(add-hook 'prog-mode-hook 'company-mode)
(setq-default company-backends (delete 'company-semantic company-backends))

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
(add-hook 'after-init-hook 'company-quickhelp-mode)
;; ===================================================================================

;;; Yasnippet
;; ===================================================================================
(require 'yasnippet)

(add-hook 'prog-mode-hook 'yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; ===================================================================================

;;; Deft
;; ===================================================================================
(setq-default deft-extensions '("org")
	      deft-directory "~/notebook"
	      deft-recursive t
	      )
;; ===================================================================================

;;; Flycheck
;; ===================================================================================
(add-hook 'prog-mode-hook 'flycheck-mode)
;; ===================================================================================

;;; Program
;;====================================================================================
;; c/cpp mode
(require 'c-cpp-dev)
(require 'java-dev)
(require 'python-dev)
;;====================================================================================


(provide 'init)
;;; init.el ends here
