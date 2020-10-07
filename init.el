;; init.el --- M.Liu's emacs initilize file

;; Author: M.Liu <liumiaogemini@gmail.com>
;; License: See the LICENSE in the root directory.
;;
;;; Commentary:
;; This file is

;;; Code:
;; (setq debug-on-error t)


(let (
      ;; adjust garbage collection at startup
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6))

  ;; Use a hook so the messages doesn't get clobbered by other messages.
  (add-hook 'emacs-startup-hook
	          (lambda ()
	            (message "Emacs ready in %s with %d garbage collections."
		                   (format "%.2f seconds"
			                         (float-time
			                          (time-subtract after-init-time before-init-time)))
		                   gcs-done)))

  ;; add `lisp' to `load-path'.
  (add-to-list 'load-path
               (expand-file-name "lisp" user-emacs-directory))

  ;; load user configs.
  (require 'configs)
  (or (file-exists-p user/userconfig-file)
      (copy-file (concat user-emacs-directory "lisp/templates/userconfig.template")
		 user/userconfig-file)
      )
  (load user/userconfig-file)

  ;; load `custom-file'
  (setq custom-file user/custom-file)
  (when (file-exists-p custom-file)
    (load custom-file))

 (require 'cl-lib)
  ;; Language and coding
  (set-language-environment "utf-8")
  (set-keyboard-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)


  (if (fboundp 'with-eval-after-load)
      (defalias 'after-load 'with-eval-after-load)
    (defmacro after-load (feature &rest body)
      "After FEATURE is loaded, evaluate BODY."
      (declare (indent defun))
      `(eval-after-load ,feature
         '(progn ,@body))))


  (defun add-to-hook (fun hooks)
    "Add FUN to HOOKS."
    (dolist (hook hooks)
      (add-hook hook fun)))

  (defun add-all-to-list (usr-list vars)
    "USR-LIST get th VARS in."
    (dolist (list vars)
      (add-to-list usr-list list))
    )

;;; Basic
  (setq-default make-backup-files nil ;; Don't make a backup file which end with "~"
                visible-bell t        ;; Flash the frame to represent a bell
		scroll-step 1 ;; smooth scroll
                auto-image-file-mode t
                initial-scratch-message nil
                inhibit-splash-screen t
                column-number-mode nil
                line-number-mode nil
                initial-major-mode 'text-mode
                frame-title-format "%b"
                mode-line-format user/mode-line-format)

  ;; Set font
  (set-frame-font "Source Code Pro 11" t t)

;;; Interface
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'set-scroll-bar-mode)
    (set-scroll-bar-mode nil))
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
  (show-paren-mode 1)
  (delete-selection-mode 1)
  (electric-pair-mode 1)
  (size-indication-mode t)
  ;; use y-n to replace yes-no
  (fset 'yes-or-no-p 'y-or-n-p)

  (add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))
  
;;; Packages
  (require 'package)
  (setq package--init-file-ensured t
        package-enable-at-startup nil
        package-archives user/package-mirror
        )

  (if (< emacs-major-version 27.0)
      (package-initialize)
    )
  (or (package-installed-p 'use-package)
      (progn
        (package-refresh-contents)
        (package-install 'use-package))
      )

;;; Built-In packages
  (use-package recentf
    :defer 1
    :commands (recentf-save-list)
    :init
    (progn
      (add-hook 'find-file-hook (lambda () (unless recentf-mode
					     (recentf-mode)
					     (recentf-track-opened-file))))
      (setq recentf-save-file user/recentf-save-file
	    recentf-max-saved-items 1000
	    recentf-auto-cleanup 'never
	    recentf-auto-save-timer (run-with-idle-timer 600 t
							 'recentf-save-list)))
    )

  (use-package display-line-numbers
    :hook
    (prog-mode . display-line-numbers-mode)
    )

  ;;
  (use-package saveplace
    :hook (after-init . save-place-mode)
    :init
    (setq save-place-file user/save-place-file)
    )

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
    :bind (("C-c j i" . 'imenu))
    )

;;; Navigation
  (use-package ivy
    :ensure t
    :diminish ivy-mode)

  (use-package swiper
    :ensure t
    :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    :bind
    (("C-s" . 'swiper)
     )
    )

  (use-package counsel
    :ensure t
    :config
    :bind
    (;;("M-x" . 'counsel-M-x)
     ;;("C-h f" . 'counsel-describe-function)
     ;;("C-h v" . 'counsel-describe-variable)
     )
    )

  (use-package snails
    :load-path "lisp/snails"
    :bind (("C-c s s" . 'snails))
    )

  (use-package window-jump
    :ensure t
    :bind
    (("C-c w l" . 'window-jump-right)
     ("C-c w h" . 'window-jump-left)
     ("C-c w k" . 'window-jump-up)
     ("C-c w j" . 'window-jump-down)
     ("C-c w 2" . 'split-window-right)
     ("C-c w 0" . 'delete-window)
     ("C-c w 1" . 'delete-other-windows)
     ))

  (use-package popwin
    :ensure t)


  ;; Projectile------------------------------------------------------------------------
  (use-package counsel-projectile
    :ensure t)
  (use-package projectile
    :ensure t
    :init
    (progn
      (setq projectile-indexing-method 'alien
	    projectile-generic-command "find . -type f")
      (setq projectile-sort-order 'recentf
	    projectile-cache-file user/projectile-cache-file
	    projectile-known-projects-file user/projectile-known-projects-file)
      )
    :config
    (projectile-mode +1)
    :diminish projectile-mode
    :bind (("C-c p f" . 'counsel-projectile-find-file)
	   ("C-c p p" . 'counsel-projectile-switch-project)
	   ("C-c p b" . 'counsel-projectile-switch-to-buffer)
	   ("C-c p k" . 'projectile-kill-buffers))
    )

  ;; Version Control=========================================================================
  (use-package magit
    :ensure t
    :bind (("C-c g s" . 'magit-status)
	   ("C-c g d" . 'magit-diff-range)
	         )
    )
  ;;=========================================================================================


  ;;-------------------------------------------------------------------------------------
  (use-package diminish
    :ensure t)
  (defun hide-minor-mode ()
    "This function hide HIDED-LIST from the modeline to save the space of modeline."
    (let ((dim-list
           ;; minor modes list followed will not show in the mode line.
           '(abbrev-mode hs-minor-mode auto-revert-mode
			 hs-minor-mode image-mode iimage-mode visual-line-mode
			 eldoc-mode undo-tree-mode company-mode))
          )
      (dolist (list dim-list)
        (diminish list)))
    )
  (add-hook 'after-init-hook 'hide-minor-mode)
  (add-hook 'find-file-hook (lambda () (hide-minor-mode)))

;;; Deft
  (use-package deft
    :ensure t
    :config
    (setq-default deft-extensions user/notes-extensions
                  deft-directory user/notes-dir
                  deft-recursive t
	                ))

;;; youdao-dict
  (use-package youdao-dictionary
    :ensure t
    :bind (("C-c o y" . 'youdao-dictionary-search-at-point+))
    )

;;; Keybinding
  (use-package evil
    :ensure t)
  (use-package evil-leader
    :ensure t)

  (use-package which-key
    :ensure t
    :init
    (which-key-mode)
    :config
    :diminish which-key-mode
    )
  (require 'functions)
  (require 'keybindings)
  (require 'prog-common)
  (require 'prog-c-cpp)
  (require 'prog-python)
  (require 'prog-haskell)
  (require 'init-org)
  )


(provide 'init)
;;; init.el ends here
