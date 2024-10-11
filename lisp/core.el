;;; core.el -- my functions & macros

;;; Commentary:

;;; Code:
(require 'project)
(require 'profiler)
(require 'server)

(defun open-init-file()
  "Find and open the init.el."
  (interactive)
  (find-file (concat user-emacs-directory "init.org")))

(defun load-init-file()
  "Load init.el."
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

;;;###autoload
(defun create-if-not-found ()
  "Create file if not found."
  (unless (file-remote-p buffer-file-name)
    (let ((parent-dir (file-name-directory buffer-file-name)))
      (and (not (file-directory-p parent-dir))
           (y-or-n-p (format "Directory `%s' does not exist! Create it? "
                             parent-dir))
           (make-directory parent-dir)))))

;;;###autoload
(defun restart-server ()
  "Restart the Emacs server."
  (interactive)
  (server-force-delete)
  (while (server-running-p)
    (sleep-for 1))
  (server-start))

;;;###autoload
(defun search--dir (dir &optional initial)
  "Search directory.

DIR for the search directory.
INITIAL for the initial input."
  (require 'consult)
  (cond ((executable-find "rg")
         (consult-ripgrep dir initial))
        ((executable-find "grep")
         (consult-grep dir initial))
        (t (user-error "Couldn't find ripgrep or grep in PATH"))))

(defun search-project (&optional dir)
  "Search current project in DIR."
  (interactive "P")
  (search--dir dir nil))

(defun search-project-at (&optional dir)
  "Search current project at point."
  (interactive "P")
  (search--dir dir (thing-at-point 'symbol)))

(defun search-current-work-dir ()
  "Search current directory."
  (interactive)
  (search--dir default-directory nil))

(defun search-current-work-dir-at ()
  "Search current directory at point."
  (interactive)
  (search--dir default-directory (thing-at-point 'symbol)))

;;;###autoload
(defun find--file-in-dir (dir &optional include-all)
  "Find file in DIR."
  (unless (file-directory-p dir)
    (error "Directory %S does note exist" dir))
  (unless (file-readable-p dir)
    (error "Directory %S isn't readable" dir))
  (require 'project)
  (let* ((default-directory (file-truename dir))
         (project--list (list `(,dir)))
         (pr `(transient . ,dir))
         (root (project-root pr))
         (dirs (list root)))
    (project-find-file-in
     (or (thing-at-point 'filename)
         (and buffer-file-name (file-relative-name buffer-file-name root)))
     dirs pr include-all)))

;;;###autoload
(defun init-fonts-in-emacs (default-font serif-font &optional symbol-font emoji-font)
  "Loads fonts."
  (dolist (map `((default . ,default-font)
                 (fixed-pitch . ,default-font)
                 (fixed-pitch-serif . ,serif-font)))
    (when-let* ((face (car map))
                (font (cdr map)))
      (dolist (frame (frame-list))
        (when (display-multi-font-p frame)
          (set-face-attribute face frame
                              :width 'normal :width	'normal
                              :slant 'normal :font font)))))
  (when (fboundp 'set-fontset-font)
    (when symbol-font
      (dolist (script '(symbol mathematical))
        (set-fontset-font t script symbol-font)))
    (when emoji-font
      (set-fontset-font t 'symbol emoji-font nil 'append)))
  (run-hooks 'after-setting-font-hook))

;;;###autoload
(defun insert-file-path (arg)
  "Insert the file name (absolute path if prefix ARG)."
  (interactive "P")
  (let ((path (or buffer-file-name default-directory)))
    (insert
     (if arg
         (abbreviate-file-name path)
       (file-name-nondirectory path)))))

(provide 'core)
;;; core.el ends here
