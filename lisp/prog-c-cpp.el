
;;; Code:

;; (use-package ccls
;;   :ensure t
;;   :defer t)

(use-package clang-format
  :ensure t
  :defer t)

(use-package bison-mode
  :ensure t
  :mode (("\\.lex\\'" . flex-mode)
         ("\\.y\\'" . bison-mode)
         ("\\.grm\\'" . bison-mode)))

(provide 'prog-c-cpp)
;;; prog-c-cpp.el ends here
