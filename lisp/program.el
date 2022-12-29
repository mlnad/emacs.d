;;; programming.el --- Emacs configuration for programming
;;
;;; Commentary:
;; This file is not a part of Emacs
;;
;;; Code:
;;; Language server protocol

;;; Project
(use-package project)

;;; lsp
(use-package eglot
  :ensure t
  :hook (prog-mode . eglot-ensure)
  :init
  (setq eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5))

(use-package consult-eglot
  :ensure t
  :init
  (define-key eglot-mode-map [remap xref-find-apropos] #'consult-eglot-symbols))

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

;;; C/C++
(use-package cc-mode
  :ensure t
  :defer t
  :config
  (setq c-basic-offset tab-width
        c-backspace-function #'delete-backward-char))

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
  :mode (("\\.py\\'" . python-mode))
  :custom
  (python-indent-offset 4)
  :config
  (setq python-shell-interpreter "python3"))

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

  ;; HACK `rustic-lsp' sets up lsp-mode/eglot too early. We move it to
  ;;      `rustic-mode-local-vars-hook' so file/dir local variables can be used
  ;;      to reconfigure them.
  (setq rustic-lsp-client 'lsp-mode))


(provide 'programming)
;;; programming.el ends here
