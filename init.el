;;; init.el --- M.Liu's emacs initilize file

;; Author: M.Liu <liumiaogemini@gmail.com>
;; License: See the LICENSE in the root directory.
;;
;;; Commentary:
;; This file is

;;; Code:
;; (setq debug-on-error t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Language and coding
(set-language-environment 'Chinese-GB)
(set-keyboard-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defconst *is-a-win* (eq system-type 'windows-nt))
(defconst *is-a-mac* (eq system-type 'darwin))
;;-----------------------------------------------------------------------

(require 'basic)
(require 'cl)
(require 'user-fun)
(require 'keybinding)

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
(when *is-a-win*
  (set-frame-font "Consolas 11")) ;; set when the OS is windows
(when *is-a-win*
  (set-fontset-font "fontset-default" 'gb18030 '("DengXian" . "unicode-bmp")))

;; ===================================================================================

;;; Require *.el files
;; ===================================================================================
(defconst pack-dir
  (expand-file-name "elpa" user-emacs-directory )
  "Packages install by package-initilize.")

;; ===================================================================================

;; ===================================================================================
;; (setq-default custom-enabled-themes '(solarized-light))
(setq solarized-use-variable-pitch nil)
(setq solarized-use-less-bold t)
(setq solarized-use-more-italic t)
(setq solarized-emphasize-indicators nil)
(setq solarized-scale-org-headlines nil)
;; (setq solarized-high-contrast-mode-line t)

(add-hook 'after-init-hook 'reapply-themes)
;; (add-hook 'tty-setup-hook 'tty-setup-theme)

;; Disable tool bar etc. to simple the Emacs
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

;; (let ((no-border '(internal-border-width . 0)))
;;   (add-to-list 'default-frame-alist no-border)
;;   (add-to-list 'initial-frame-alist no-border))


(setq-default mode-line-format ;; set mode line
	      (list
	       "%e" ;; print error message
	       mode-line-front-space
	       '(:eval evil-mode-line-tag)
	       mode-line-mule-info mode-line-client mode-line-modified
	       mode-line-remote
	       mode-line-frame-identification mode-line-buffer-identification ;; buffer files
	       "   "
	       "["
	       mode-line-position ;; position of this buffer
	       "/%I] "
	       "  "
	       mode-line-modes ;; Major mode and some important minor modes.
	       '(vc-mode vc-mode) ;; version control messages.
	       mode-line-misc-info mode-line-end-spaces)) ;; end of mode line

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

(defun hide-minor-mode ()
  "This function hide HIDED-LIST from the modeline to save the space of modeline."
  (let ((dim-list
         ;; minor modes list followed will not show in the mode line.
         '(helm-mode company-mode yas-minor-mode abbrev-mode org-autolist-mode
		                 image-mode iimage-mode visual-line-mode eldoc-mode undo-tree-mode))
        )
    (dolist (list dim-list)
      (diminish list)))
  )
(add-hook 'after-init-hook 'hide-minor-mode)
(add-hook 'find-file-hook (lambda () (hide-minor-mode)))

(setq-default frame-title-format "%b"
	      )
;; (require 'powerline)
;; (powerline-default-theme)
;; ===================================================================================

;;; Deft
;; ===================================================================================
(setq-default deft-extensions '("org")
	      deft-directory "~/notebook"
	      deft-recursive t
	      )
;; ===================================================================================

;;; Program
;;====================================================================================
;; c/cpp mode
(require 'c-cpp-dev)
(require 'java-dev)
(require 'python-dev)
;;====================================================================================

(when (file-exists-p custom-file)
  (load custom-file))
