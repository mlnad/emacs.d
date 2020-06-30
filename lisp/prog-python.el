
;;; Code:

(use-package importmagic
  :defer t
  :ensure t
  :diminish importmagic-mode
  :init
  (progn
    (add-hook 'python-mode-hook 'importmagic-mode)
    ))

(use-package pipenv
  :defer t
  :ensure t
  )

(provide 'prog-python)
;;; prog-python.el ends here
