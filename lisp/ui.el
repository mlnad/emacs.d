;;; ui.el --- UI configuration for emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 刘邈
;;
;; Author: 刘邈 <liumiaogemini@foxmail.com>
;; Maintainer: 刘邈 <liumiaogemini@foxmail.com>
;; Created: Feb 12, 2023
;; Modified: Feb 12, 2023
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  UI configuration for emacs
;;
;;; Code:
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
    (disable-theme custom-enabled-themes)
    (load-theme configs/theme t)))

(let ((hook (if (daemonp)
                'server-after-make-frame-hook
              'after-init-hook)))
  (add-hook hook #'editor/init-font -100)
  (add-hook hook #'editor/init-theme -99))

(add-hook 'emacs-startup-hook #'window-divider-mode)

;; Don't display floating tooltips;
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;;; Line numbers
(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode))

;;; Modeline
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-bar-width 3)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-persp-name t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-icon t)
  :config
  (use-package anzu
    :ensure t)
  (use-package evil-anzu
    :config (global-anzu-mode +1)))

;;; doom themes
(use-package doom-themes
  :ensure t)

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :preface
  (add-hook 'after-setting-font-hook
            (lambda ()
              (when (fboundp 'set-fontset-font)
                (dolist (font (list "Weather Icons"
                                    "github-octicons"
                                    "FontAwesome"
                                    "all-the-icons"
                                    "file-icons"
                                    "Material Icons"))
                  (set-fontset-font t 'unicode (font-spec :family font) nil 'append))))))

(use-package writeroom-mode
  :ensure t
  :config
  (setq writeroom-width 100)
  (setq writeroom-global-effects
        '(writeroom-set-alpha
          writeroom-set-menu-bar-lines
          writeroom-set-tool-bar-lines
          writeroom-set-vertical-scroll-bars
          writeroom-set-bottom-divider-width)))

(use-package popper
  :ensure t
  :bind (("C-`"    . popper-toggle-latest)
         ("M-`"    . popper-cycle))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\.*eshell.*\\*$" eshell-mode
          "^\\*vc-diff"
          "^\\*Python"
          "^\\*Completions"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  :config
  (setq popper-group-function #'popper-group-by-project))


(provide 'ui)
;;; ui.el ends here
