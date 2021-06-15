;;; prog-python.el --- Configuration for python.
;;; Commentary:

;;; Code:
(defvar python/pyvenv-modes nil)

(defun python/pyvenv-set-local-virtualenv ()
  "Set pyvenv virtualenv from \".venv\" by looking in parent directories."
  (interactive)
  (let ((root-path (locate-dominating-file default-directory ".venv")))
    (when root-path
      (let ((file-path (expand-file-name ".venv" root-path)))
        (cond ((file-directory-p file-path)
               (pyvenv-activate file-path)
               (message "Activated local virtualenv"))
              (t (message ".venv is not a directory")))))))

(use-package python
  :defer t
  :ensure nil
  :after flycheck
  :mode "\\.py\\'"
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  :config
  (setq python-shell-interpreter "python3"))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))

(use-package yapfify
  :ensure t
  :defer t
  :hook (python-mode . yapf-mode))

(use-package pyvenv
  :ensure t
  :init
  (add-hook 'python-mode-hook #'pyvenv-tracking-mode)
  (add-to-list 'python/pyvenv-modes 'python-mode)
  ;; Set for auto active virtual env
  (dolist (m python/pyvenv-modes)
    (add-hook (intern (format "%s-hook" m))
              'python/pyvenv-set-local-virtualenv())))

(provide 'prog-python)
;;; prog-python.el ends here
