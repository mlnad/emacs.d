;;; init-python.el --- emacs initilze file
;;; Commentary:
;;; Code:

(require 'company)

(defun python-config()
  (elpy-enable)
  (add-to-list 'company-backends '(company-jedi company-files))
  )

(add-hook 'python-mode-hook 'python-config)

(provide 'python-dev)
;;; python-dev.el ends here
