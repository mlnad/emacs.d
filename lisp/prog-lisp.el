;;; prog-lisp.el --- common configuration for lisp
;;
;;; Commentary:
;;
;;; Code:
;;; Scheme
(use-package geiser
  :ensure t
  :commands run-geiser)

(use-package parinfer-rust-mode
  :ensure t
  :hook emacs-lisp-mode scheme-mode common-lisp-mode)

(use-package lispy-mode
  :ensure lispy
  :hook emacs-lisp-mode
  :diminish lispy-mode)

(use-package ielm
  :defer t)

(use-package debug
  :defer t)

(use-package edebug
  :ensure nil
  :defer t)

(use-package emr
  :ensure t)

(provide 'prog-lisp)
;;; prog-lisp.el ends here
