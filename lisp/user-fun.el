;;; init-function.el --- This is a file about function setting

;;; Commentary:
;; Here gets some function that could be used when the
;; Emacs started.

;;; Code:

;;; Get init.el
;;---------------------------------------------------------------------------
(defun open-init-file()
  "Find and open the init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun load-init-file()
  "Load init.el."
  (interactive)
  (load-file "~/.emacs.d/init.el")
  )
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

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;;-----------------------------------------------------------------------

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))


(defun add-to-hook (fun hooks)
  "Add FUN to HOOKS."
  (dolist (hook hooks)
    (add-hook hook fun)))

(defun add-all-to-list (usr-list vars)
  "USR-LIST get th VARS in."
  (dolist (list vars)
    (add-to-list usr-list list))
  )

(defun tty-setup-theme ()
  "Disable theme when use terminal."
    (disable-theme 'atom-one-dark)
    )

(provide 'user-fun)
;;; user-fun.el ends here
