;;; basic.el --- basic emacs settings

;; Author: M.Liu <liumiaogemini@foxmail.com>
;;; Commentary:
;;
;;; Code:
(require 'package)
(require 'user-fun)

;;; Packages
;;=========================================================================================
(setq package--init-file-ensured t
      package-enable-at-startup nil
      package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("org"   . "http://elpa.emacs-china.org/org/"))
      )

;;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))


;; Fire up package.el
(package-initialize)

(require 'cl-lib)

(defun install-basic-package ()
   "Install some basic package for Emacs."
   (let ((basic-edit-pack-list
          '(dash swiper youdao-dictionary deft
                 window-jump avy avy-menu counsel use-package undo-tree multi-term
                 cnfonts powerline atom-one-dark-theme diminish list-utils
                 company company-quickhelp cl-lib helm
                 )))
     (dolist (pack basic-edit-pack-list)
       (require-package pack))
     )
   (let ((basic-maybe-pack-list
          '(helm-ebdb)))
     (dolist (pack basic-maybe-pack-list)
       (maybe-require-package pack)))
   )
(install-basic-package)
;;=========================================================================================

;;; Completion
;;=========================================================================================
(require 'pos-tip)
(require 'company)
(add-hook 'prog-mode-hook 'company-mode) ;; Only load company mode when you are programming
(setq-default company-backends (delete 'company-semantic company-backends))

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)
(add-hook 'after-init-hook 'company-quickhelp-mode)

(require 'yasnippet)
(add-hook 'prog-mode-hook 'yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(require 'helm-config)
(helm-mode 1)
;;=========================================================================================

;;; Flycheck
;; ===================================================================================
(require-package 'flycheck)
(add-hook 'prog-mode-hook 'flycheck-mode)
;; ===================================================================================


(provide 'basic)
;;; basic.el ends here
