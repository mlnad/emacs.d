;;; core-libs.el -- my functions & macros

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

(defun user/counsel-search-rg (&optional initial-directory)
  "Searching with rg in Emacs.
If INITIAL-DIRECTORY is non nil start in that directory."
  (interactive)
  (require 'counsel)
  (let* ((default-directory
	   (or initial-directory (read-directory-name "Start from directory: ")))
	 )
    (counsel-rg "" default-directory nil "rg: "))
  )

(defun user/counsel-search-project()
  "Seraching project with rg."
  (interactive)
  (user/counsel-search-rg (projectile-project-root)))

(defun user/counsel-search-dir ()
  "Searching directory with rg."
  (interactive)
  (user/counsel-search-rg default-directory))

(provide 'core-libs)
;;; functions.el ends here
