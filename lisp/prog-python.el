
;;; Code:

(defun python/pyenv-executable-find (command)
  "Find executable taking pyenv shims into account."
  ())
(defun python/execute-file (args)
  "Execute a python script in a shell."
  (interactive "P")
  (let ((universal-argument t)
        (compile-command (format "%s %s")))))

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

(use-package yapfify
  :ensure t
  :defer t
  :hook (python-mode . yapf-mode))

(use-package python
  :defer t)

(provide 'prog-python)
;;; prog-python.el ends here
