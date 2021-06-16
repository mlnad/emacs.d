;;; apps.el --- Emacs apps
;;
;;; Commentary:

;;; Code:
(require 'evil)

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
  (evil-define-key nil 'global (kbd "<leader>oy") 'youdao-dictionary-search-at-point+))

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
    (global-git-gutter-mode +1))

  :config
  (user/set-global-leader-key*
    "gs" 'magit-status
    "gd" 'magit-diff-range))

;;; Shell
(use-package eshell
  :ensure nil
  :init
  (setq eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-kill-processes-on-exit t
        eshell-hist-ignoredups t
        eshell-input-filter (lambda (input) (not (string-match-p "\\`\\s-+" input)))
        eshell-prompt-regexp "^.* λ "
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t))

;;; Rime
(use-package rime
  :ensure t
  :config
  (setq rime-show-candidate 'posframe
        default-input-method "rime"))

;;; EAF
(quelpa '(eaf :fetcher github
              :repo "manateelazycat/emacs-application-framework"
              :files ("*")))

(use-package eaf
  :load-path "quelpa/build/eaf"
  :init
  (use-package epc :defer t :ensure t)
  (use-package ctable :defer t :ensure t)
  (use-package deferred :defer t :ensure t)
  :custom
  (eaf-browser-continue-where-left-off t)
  :config
  (eaf-setq eaf-browser-enable-adblocker "true")
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding))

(provide 'apps)
;;; apps.el ends here
