;;; programming.el --- Emacs configuration for programming
;;
;;; Commentary:
;; This file is not a part of Emacs
;;
;;; Code:
;;; Language server protocol

;;; Project
(use-package project)

;;; Emacs Lisp
(use-package elisp-mode
  :mode ("\\.Cask\\'" . emacs-lisp-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode))

(use-package buttercup
  :ensure t
  :defer t
  :mode ("/test[/-].+\.el$" . buttercup-minor-mode))

(use-package debug)

(use-package edebug)

(use-package emr
  :ensure t)

;;; lsp
(use-package eglot
  :ensure t
  :init
  (setq eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5))

(use-package consult-eglot
  :ensure t
  :bind (([remap xref-find-apropos] . consult-eglot-symbols)))

;;; Snippet
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
  (add-hook 'prog-mode-hook 'yas-reload-all))

(use-package bison-mode
  :ensure t
  :mode (("\\.lex\\'" . flex-mode)
         ("\\.y\\'" . bison-mode)
         ("\\.grm\\'" . bison-mode)))

(use-package cmake-mode
  :ensure t)

;;; Scheme
(use-package geiser
  :ensure t
  :commands run-geiser)

;;; Haskell
(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'")

;;; Python
(use-package python
  :mode (("\\.py\\'" . python-mode))
  :custom
  (python-indent-offset 4)
  :config)

;;; Rust
(use-package rustic
  :ensure t
  :mode ("\\.rs$" . rustic-mode)
  :after (projectile)
  :config
  (add-to-list 'projectile-project-root-files "Cargo.toml")
  (setq rustic-indent-method-chain t
        rustic-babel-format-src-block nil)

  ;; HACK `rustic-lsp' sets up lsp-mode/eglot too early. We move it to
  ;;      `rustic-mode-local-vars-hook' so file/dir local variables can be used
  ;;      to reconfigure them.
  (setq rustic-lsp-client 'lsp-mode))


(provide 'program)
;;; programming.el ends here
