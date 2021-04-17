
;;; Code:

;; (use-package ccls
;;   :ensure t
;;   :defer t)

(use-package clang-format
  :ensure t
  :defer t)

(use-package company-c-headers
  :ensure t
  :init
  (add-to-list 'company-backends 'company-c-headers))

(use-package bison-mode
  :ensure t)

(provide 'prog-c-cpp)
;;; prog-c-cpp.el ends here
