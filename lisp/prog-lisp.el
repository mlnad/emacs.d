;;; prog-lisp.el --- common configuration for lisp
;;
;;; Commentary:
;;
;;; Code:
;;; Scheme
(use-package geiser
  :ensure t
  :commands run-geiser)

(use-package parinfer
  :ensure t
  :hook (scheme-mode . parinfer-mode))

(provide 'prog-lisp)
;;; prog-lisp.el ends here
