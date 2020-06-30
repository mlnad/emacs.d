;; prog-lsp.el --- init-file for emacs language server protocol
;;; Commentary:
;;

;;; Code:
;;;

;;; Language server protocol
(use-package nox
  :load-path "lisp/nox"
  :config
  (add-to-list 'nox-server-programs
	       `(python-mode . ("pyls" "-v" "--tcp" "--host"
				"localhost" "--port" :autoport)))
  (dolist (hook (list
		 'python-mode-hook
		 'c-mode-hook
		 'c-mode-common-hook
		 'c++-mode-hook
		 'haskell-mode-hook))
    (add-hook hook '(lambda () (nox-ensure)))))

;; (use-package eglot
;;   :ensure t
;;   :config
;;   (dolist (hook (list
;; 		 'python-mode-hook
;; 		 'c-mode-hook
;; 		 'c-mode-common-hook
;; 		 'c++-mode-hook
;; 		 'haskell-mode-hook))
;;     (add-hook hook '(lambda () (eglot-ensure)))
;;     )
;;   )

;; (use-package lsp-mode
;;   :ensure t
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (
;; 	 (c-mode . lsp-deferred)
;; 	 (c++-mode . lsp-deferred)
;; 	 (python-mode . lsp-deferred)
;; 	 (lsp-mode . lsp-enable-which-key-integration)
;; 	 )
;;   :config
;;   (setq lsp-enable-snippet nil)
;;   :commands (lsp lsp-deferred)
;;   )

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-doc-enable nil)
  :commands lsp-ui-mode)
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :ensure t)

;;; Flycheck
(use-package flycheck-mode
  :ensure flycheck
  :hook prog-mode)

(provide 'prog-common)
;;; prog-common.el ends here
