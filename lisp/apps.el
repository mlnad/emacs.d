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
                deft-recursive t
                ))

;;; youdao-dict
(use-package youdao-dictionary
  :ensure t
  :config
  (evil-define-key nil 'global (kbd "<leader>oy") 'youdao-dictionary-search-at-point+)
  )

;;; Magit for git
(use-package magit
  :ensure t
  :init
  (use-package forge
    :ensure t)
  (use-package magit-gitflow
    :ensure t
    :hook (maigt-mode . turn-on-magit-gitflow))
  :config
  (evil-define-key* nil 'global
    (kbd "<leader>gs") 'magit-status
    (kbd "<leader>gd") 'magit-diff-range)
  )


(provide 'apps)
;;; apps.el ends here
