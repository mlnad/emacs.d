;;; init-function.el --- This is a file about function setting

;;; Commentary:
;; Here gets some function that could be used when the
;; Emacs started.

;;; Code:

;;; Get init.el
;;---------------------------------------------------------------------------
(defun mark/open-init-file()
  "Find and open the init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
;;---------------------------------------------------------------------------

;;; Rename the current file
;;---------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Rename both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
    (filename (buffer-file-name)))
  (unless filename
    (error "Buffer '%s' is not visiting a file! " name))
  (progn
    (when (file-exists-p filename)
      (rename-file filename new-name 1))
    (set-visited-file-name new-name)
    (rename-buffer new-name))
 ))
;;--------------------------------------------------------------------------

;;; Packages
;;-----------------------------------------------------------------------
;;(defun mark/package-installed-p ()
;;  (interactive)

;;  (loop for pkg in package-selected-packages
;;        when (not (package-installed-p pkg)) do (return nil)
;;        finally (return t)))

;; (unless (mark/package-installed-p)
;;  (message "%s" "Refreshing package database...")
;;  (package-refresh-contents)
;;  (dolist (pkg package-selected-packages)
;;    (when (not (package-installed-p pkg))
;;      (package-install pkg))))

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
nIn the event of failure, return nil and print a warning message.
pOptionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))
;;-----------------------------------------------------------------------

(defun add-to-hook (fun hooks)
  "Add FUN to HOOKS."
  (dolist (hook hooks)
    (add-hook hook fun)))

(defun add-all-to-list (usr-list vars)
  "USR-LIST get th VARS in."
  (dolist (list vars)
    (add-to-list usr-list list))
  )

(defvar dim-list)

(defun hide-minor-mode (hided-list)
  "This function hide HIDED-LIST from the modeline to save the space of modeline."
  (dolist (list hided-list)
    (diminish list))
  )

(defun tty-setup-theme ()
  "Disable theme when use terminal."
    (disable-theme 'atom-one-dark)
    )

(provide 'user-fun)
;;; user-fun.el ends here
