;; program.el --- initilize file for emacs programming

;;; Commentary:

;;; Code:

;; Flycheck
(use-package flycheck-mode
  :ensure flycheck
  :hook prog-mode)
;; hs-minor-mode

(require 'prog-c-cpp)
(require 'prog-python)
(require 'prog-haskell)
(require 'prog-lsp)

(defun program ()
  "Manage programming issues for Emacs."
  (interactive)
  )

(provide 'program)
;;; program.el ends here
