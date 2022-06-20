;;; prog-rust.el --- rust config
;;
;;; Commentary:
;;
;;; Code:
(use-package rustic
  :ensure t
  :mode ("\\.rs$" . rustic-mode)
  :after (projectile)
  :config
  (add-to-list 'projectile-project-root-files "Cargo.toml")
  (setq rustic-indent-method-chain t
        rustic-babel-format-src-block nil)

  (remove-hook 'rustic-mode-hook #'flycheck-mode)
  (remove-hook 'rustic-mode-hook #'flycheck-mode-off)
  ;; HACK `rustic-lsp' sets up lsp-mode/eglot too early. We move it to
  ;;      `rustic-mode-local-vars-hook' so file/dir local variables can be used
  ;;      to reconfigure them.
  (setq rustic-lsp-client 'lsp-mode))

(provide 'prog-rust)
;;; prog-rust.el ends here
