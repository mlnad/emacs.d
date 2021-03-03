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
  :bind (("C-c o y" . 'youdao-dictionary-search-at-point+))
  )

;;; Magit for git
(use-package magit
  :ensure t
  :bind (("C-c g s" . 'magit-status)
         ("C-c g d" . 'magit-diff-range))
  :init
  (use-package forge
    :ensure t)
  (use-package magit-gitflow
    :ensure t
    :hook (maigt-mode . turn-on-magit-gitflow))
  )


(provide 'apps)
;;; apps.el ends here
