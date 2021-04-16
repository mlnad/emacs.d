
;;; Code:

(use-package importmagic
  :defer t
  :ensure t
  :diminish importmagic-mode
  :init
  (add-hook 'python-mode-hook 'importmagic-mode))

(use-package pipenv
  :defer t
  :ensure t
  :init
  (add-hook 'python-mode-hook #'pyvenv-tracking-mode))

(use-package python
  :defer t
  :init
  :config
  )

(provide 'prog-python)
;;; prog-python.el ends here
