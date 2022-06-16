;; prog-lsp.el --- init-file for emacs language server protocol
;;; Commentary:
;;

;;; Code:
;;;

;;; Language server protocol
(defvar lsp-company-backends
  '(:separate company-capf company-yasnippet))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :hook ((c-mode c++-mode python-mode) . lsp-deferred)
  :config
  (setq lsp-enable-snippet nil
        lsp-modeline-diagnostics-enable nil
        lsp-prefer-capf t
        lsp-keep-workspace-alive nil)

  (add-hook 'lsp-completion-mode-hook
            (defun lsp-init-company-backends-h ()
              (when lsp-completion-mode
                (set (make-local-variable 'company-backends)
                     (cons lsp-company-backends
                           (remove lsp-company-backends
                                   (remq 'company-capf company-backends)))))))

  ;; Tramp
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote))

  (push '("^\\*[Ll]sp.+\\*$"
          :regexp t
          :dedicated t
          :position bottom
          :stick t
          :noselect t)
        popwin:special-display-config)

  :commands (lsp-install-server))

(use-package lsp-ui
  :ensure t
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable nil
        lsp-ui-peek-enable nil
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-actions-icon lsp-ui-sideline-actions-icon-default)
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :after (lsp-mode)
  :ensure t)

;;; Completion
(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-extra-mode
             yas-active-extra-mode
             yas-deactive-extra-mode
             yas-maybe-expand-abbrev-key-filter)
  :init
  (setq yas-trigger-in-field t
        yas-wrap-around-region t
        yas-prompt-functions '(yas-completing-prompt))

  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  :config
  (add-hook 'prog-mode-hook 'yas-reload-all)
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-yasnippet))
  :diminish yas-minor-mode
  )

(use-package yasnippet-snippets
  :ensure t)

;;; Flycheck
(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode)
  :init
  (push '("^\\*Flycheck.+\\*$"
          :regexp t
          :dedicated t
          :position bottom
          :stick t
          :noselect t)
        popwin:special-display-config)
  :config)

(provide 'prog-common)
;;; prog-common.el ends here
