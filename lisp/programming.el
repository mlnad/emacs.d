;;; programming.el --- Emacs configuration for programming
;;
;;; Commentary:
;; This file is not a part of Emacs
;;
;;; Code:
;;; Language server protocol
(defvar lsp-company-backends
  '(:separate company-capf company-yasnippet))

(defvar python/pyvenv-modes nil)

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
  :config)

;;; C/C++
(use-package cc-mode
  :ensure t
  :defer t
  :config
  (setq c-basic-offset tab-width
        c-backspace-functon #'delete-backward-char))

(use-package clang-format
  :ensure t
  :defer t)

(use-package bison-mode
  :ensure t
  :mode (("\\.lex\\'" . flex-mode)
         ("\\.y\\'" . bison-mode)
         ("\\.grm\\'" . bison-mode)))

(use-package cmake-mode
  :ensure t)

;;; Assembly
(use-package nasm-mode
  :ensure t
  :mode "\\.nasm\\'")

;;; Emacs Lisp
(use-package ielm
  :defer t)

(use-package debug
  :defer t)

(use-package edebug
  :ensure nil
  :defer t)

(use-package emr
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
(defun python/pyvenv-set-local-virtualenv ()
  "Set pyvenv virtualenv from \".venv\" by looking in parent directories."
  (interactive)
  (let ((root-path (locate-dominating-file default-directory ".venv")))
    (when root-path
      (let ((file-path (expand-file-name ".venv" root-path)))
        (cond ((file-directory-p file-path)
               (pyvenv-activate file-path)
               ;; (setq pyvenv-activate file-path)
               (message "Activated local virtualenv"))
              (t (message ".venv is not a directory")))))))

(use-package python
  :after flycheck
  :mode (("\\.py\\'" . python-mode))
  :custom
  (python-indent-offset 4)
  (flycheck-python-pycompile-executable "python3")
  :config
  (setq python-shell-interpreter "python3"))

(use-package lsp-pyright
  :ensure t
  :init (when (and *sys/linux* (executable-find "python3")
                   (setq lsp-pyright-python-executable-cmd "python3")))
  
  :hook (python-mode . (lambda () (require 'lsp-pyright))))

(use-package yapfify
  :ensure t
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


;;; Rust
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


(provide 'programming)
;;; programming.el ends here
