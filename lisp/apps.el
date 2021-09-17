;;; apps.el --- Emacs apps
;;
;;; Commentary:

;;; Code:
;;; Deft
(use-package deft
  :ensure t
  :config
  (setq-default deft-extensions user/notes-extensions
                deft-directory user/notes-dir
                deft-recursive t))

;;; youdao-dict
(use-package youdao-dictionary
  :ensure t
  :config
  (push '("*Youdao Dictionary*" :dedicated t :position bottom :stick nil :noselect nil :height 0.4) popwin:special-display-config))

;;; Magit for git
(use-package magit
  :ensure t
  :init
  (use-package forge
    :ensure t)

  (use-package magit-gitflow
    :ensure t
    :hook (maigt-mode . turn-on-magit-gitflow))

  (use-package git-gutter
    :ensure t
    :custom
    (git-gutter:update-interval 2)
    :config
    (global-git-gutter-mode +1)))

;;; Shell
(quelpa '(aweshell :fetcher github
                   :repo "manateelazycat/aweshell"
                   :files ("*")))

(use-package aweshell
  :config
  (with-eval-after-load "esh-opt"
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda))
  :custom
  (aweshell-dedicated-window-height 20))

;;; Rime
(use-package rime
  :ensure t
  :config
  (setq rime-show-candidate 'posframe
        default-input-method "rime"
        rime-user-data-dir user/rime-data-dir)
  (add-hook 'after-init-hook
            (lambda ()
              (or (file-exists-p user/rime-data-dir)
                  (progn
                    (make-directory user/rime-data-dir)
                    (copy-file (concat user-emacs-directory "lisp/templates/user.yaml")
                               (concat user/rime-data-dir "user.yaml")))))))

;;; EAF
(use-package eaf
  :if user/enable-eaf
  :init
  (use-package epc :defer t :ensure t)
  (use-package ctable :defer t :ensure t)
  (use-package deferred :defer t :ensure t)
  (quelpa '(eaf :fetcher github
                :repo "manateelazycat/emacs-application-framework"
                :files ("*")))
  :custom
  (eaf-browser-continue-where-left-off t)
  :config
  (eaf-setq eaf-browser-enable-adblocker "true")
  (eaf-setq eaf-pdf-dark-mode "false")
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding))

;;; Docker
(use-package docker
  :ensure t)

(use-package docker-tramp
  :ensure t)

(provide 'apps)
;;; apps.el ends here
