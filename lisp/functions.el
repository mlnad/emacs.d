;;; functions.el -- my functions & macros

;;; Commentary:

;;; Code:
(defun open-init-file()
  "Find and open the init.el."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun load-init-file()
  "Load init.el."
  (interactive)
  (load-file (concat user-emacs-directory "init.el"))
  )

(defun open-userconfig-file()
  "Open userconfig."
  (interactive)
  (find-file user/userconfig-file))

(provide 'functions)
;;; functions.el ends here
