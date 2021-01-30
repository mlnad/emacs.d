;;; init-git.el --- git configurations

;;; Commentary:

;;; Code:
(use-package magit
  :ensure t
  :bind (("C-c g s" . 'magit-status)
         ("C-c g d" . 'magit-diff-range)
         )
  )


(use-package forge
  :ensure t
  )

(use-package magit-gitflow
  :ensure t
  :hook (magit-mode . turn-on-magit-gitflow)
  )

(provide 'init-git)
;;; init-git.el ends here
