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
  (load-file (concat user-emacs-directory "init.el")))

(defun open-userconfig-file()
  "Open userconfig."
  (interactive)
  (find-file user/userconfig-file))

(defun user/counsel-search-rg (&optional use-initial-input initial-directory)
  "Searching with rg in Emacs.
If INITIAL-DIRECTORY is non nil start in that directory."
  (interactive)
  (require 'counsel)
  (let* ((initial-input (if use-initial-input
                            (if (region-active-p)
                                (buffer-substring-no-properties
                                 (region-beginning) (region-end))
                              (thing-at-point 'symbol t))
                          ""))
         (default-directory
	       (or initial-directory (read-directory-name "Start from directory: "))))
    (counsel-rg initial-input default-directory nil "rg: ")))

(defun user/counsel-search-project()
  "Seraching project with rg."
  (interactive)
  (user/counsel-search-rg nil (projectile-project-root)))

(defun user/counsel-search-project-at-point ()
  "Seraching project with rg."
  (interactive)
  (user/counsel-search-rg t (projectile-project-root)))

(defun user/counsel-search-dir ()
  "Searching directory with rg."
  (interactive)
  (user/counsel-search-rg nil default-directory))

(defun user/counsel-search-dir-at-point ()
  "Searching directory with rg."
  (interactive)
  (user/counsel-search-rg t default-directory))

(provide 'core-libs)
;;; core-libs.el ends here
