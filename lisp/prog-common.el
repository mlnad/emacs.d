;; prog-lsp.el --- init-file for emacs language server protocol
;;; Commentary:
;;

;;; Code:
;;;

;;; Language server protocol
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred)
  :hook ((c-mode c++-mode python-mode) . lsp-deferred)
  :config
  (setq lsp-enable-snippet nil
        lsp-modeline-diagnostics-enable nil
        lsp-prefer-capf t)
  (setq lsp-enable-snippet nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (push '("^\\*[Ll]sp.+\\*$"
          :regexp t
          :dedicated t
          :position bottom
          :stick t
          :noselect t)
        popwin:special-display-config)

  (user/set-leader-key* nil lsp-mode-map
                        ;; format
                        "=b" #'lsp-format-buffer
                        "=r" #'lsp-format-region
                        "=o" #'lsp-organize-imports
                        ;; code
                        "cr" #'lsp-rename
                        ;; backends
                        "bd" #'lsp-describe-session
                        "br" #'lsp-workspace-restart
                        "bx" #'lsp-workspace-shutdown)
  :commands (lsp lsp-deferred))

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
