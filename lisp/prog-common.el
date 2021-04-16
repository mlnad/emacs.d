;; prog-lsp.el --- init-file for emacs language server protocol
;;; Commentary:
;;

;;; Code:
;;;

;;; Language server protocol
(cond
 ;; Use nox as client.
 ((eq 'nox user/lsp-client)
  (use-package nox
    :load-path "lisp/nox"
    :config
    (setq nox-server-programs user/nox-server-programs)
    (dolist (hook user/nox-list)
      (add-hook hook '(lambda () (nox-ensure))))))
 ;; Use lsp-mode as client
 ((eq 'lsp-mode user/lsp-client)
  (use-package lsp-mode
    :ensure t
    :init
    (setq lsp-keymap-prefix "C-c l")
    :hook ((c-mode . lsp-deferred)
           (c++-mode . lsp-deferred)
           (python-mode . lsp-deferred)
           (lsp-mode . lsp-enable-which-key-integration))
    :config
    (setq lsp-enable-snippet nil)
    (setq lsp-modeline-diagnostics-enable nil)
    :commands (lsp lsp-deferred)
    )

 (use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-enable nil)
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :after (lsp-mode)
  :ensure t)
 ))

;;; Completion
(use-package yasnippet
  :ensure t
  :commands (yas-global-mode yas-minor-mode yas-active-extra-mode)
  :init
  (setq yas-trigger-in-field t
        yas-wrap-around-region t
        yas-prompt-functions '(yas-completing-prompt))

  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  :config
  (add-hook 'prog-mode-hook 'yas-reload-all)
  :diminish yas-minor-mode
  )

(use-package yasnippet-snippets
  :ensure t)

(use-package company-mode
  :ensure company
  :hook prog-mode
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (tab-always-indent 'complete)
  :init
  (add-to-list 'user/evil-collection-mode-list 'company)
  :config
  (setq-default company-backends (delete 'company-semantic company-backends))
  (push '(company-semantic :with company-yasnippet) company-backends)
  (define-key company-active-map (kbd "C-/") 'counsel-company)
  :diminish company-mode)

(use-package company-statistics
  :ensure t
  :init
  (setq company-statistics-file (concat user/cache-directory
                                        "company-statistics-cache.el"))
  (add-hook 'company-mode-hook 'company-statistics-mode))

(use-package company-quickhelp
  :ensure t
  :defer company
  :commands company-quickhelp-manual-begin
  :bind (("C-c d" . 'company-quickhelp-manual-begin)))

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
  :config
  (user/set-global-leader-key*
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error
    "eb" 'flycheck-buffer
    "ec" 'flycheck-clear
    "eh" 'flycheck-describe-checker
    "es" 'flycheck-select-checker
    "ex" 'flycheck-explain-error-at-point))

(provide 'prog-common)
;;; prog-common.el ends here
