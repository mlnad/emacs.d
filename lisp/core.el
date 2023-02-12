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

(defun gc-minibuffer-setup ()
  (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

(defun gc-minibuffer-exit ()
  (garbage-collect)
  (setq gc-cons-threshold better-gc-cons-threshold))

(defun core/garbage-collect-h ()
  (if (boundp 'after-focus-change-function)
      (add-function :after after-focus-change-function
                    (lambda ()
                      (unless (frame-focus-state)
                        (garbage-collect))))
    (add-hook 'after-focus-change-function 'garbage-collect))
 
  (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup)
  (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit))

(defun core/elpa-package-dir ()
  "Generate the elpa package directory."
  (file-name-as-directory
   (if (not configs/elpa-subdirectory)
       configs/elpa-pack-dir
     (let ((subdir (format "%d%s%d"
                           emacs-major-version
                           version-separator
                           emacs-minor-version)))
       (expand-file-name subdir configs/elpa-pack-dir)))))

(defvar core/profiler nil)
;;;autoload
(defun core/toggle-profiler ()
  "Toggle the Emacs profiler"
  (interactive)
  (if (not core/profiler)
      (profiler-start 'cpu+mem)
    (profiler-report)
    (profiler-stop))
  (setq core/profiler (not core/profiler)))

(defun core/create-if-not-found ()
  "Create file if not found"
  (unless (file-remote-p buffer-file-name)
    (let ((parent-dir (file-name-directory buffer-file-name)))
      (and (not (file-directory-p parent-dir))
           (y-or-n-p (format "Directory `%s' does not exist! Create it? "
                             parent-dir))
           (make-directory parent-dir)))))

(provide 'core)
;;; core-libs.el ends here
