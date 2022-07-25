;;; core.el -- my functions & macros

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
  (find-file configs/userconfig-file))

(defun core/garbage-collect-h ()
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function
                    (lambda ()
                      (unless (frame-focus-state)
                        (garbage-collect))))
    (add-hook 'after-focus-change-function 'garbage-collect))
  (defun gc-minibuffer-setup ()
    (setq gc-cons-threshold (* better-gc-cons-threshold 2)))
  
  (defun gc-minibuffer-exit ()
    (garbage-collect)
    (setq gc-cons-threshold better-gc-cons-threshold))
  (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit))

(provide 'core)
;;; core-libs.el ends here
