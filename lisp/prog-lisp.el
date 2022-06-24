;;; prog-lisp.el --- common configuration for lisp
;;
;;; Commentary:
;;
;;; Code:
;;; emacs-lisp
;; (use-package lispy-mode
;;   :ensure lispy
;;   :hook emacs-lisp-mode
;;   :diminish lispy-mode)

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

(provide 'prog-lisp)
;;; prog-lisp.el ends here
