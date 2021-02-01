;; init.el --- M.Liu's emacs initilize file

;; Author: M.Liu <liumiaogemini@gmail.com>
;; License: See the LICENSE in the root directory.
;;
;;; Commentary:
;; This file is

;;; Code:
;; (setq debug-on-error t)


(let (
      ;; adjust garbage collection at startup
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6))

  ;; Use a hook so the messages doesn't get clobbered by other messages.
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)))
  
  ;; add `lisp' to `load-path'.
  (add-to-list 'load-path
               (expand-file-name "lisp" user-emacs-directory))
  
  ;; load user configs.
  (require 'configs)
  (or (file-exists-p user/userconfig-file)
      (copy-file (concat user-emacs-directory "lisp/templates/userconfig.template")
                 user/userconfig-file)
      )
  (load user/userconfig-file)

  ;; load `custom-file'
  (setq custom-file user/custom-file)
  (when (file-exists-p custom-file)
    (load custom-file))

  (require 'cl-lib)
  ;; Language and coding
  (set-language-environment "utf-8")
  (set-keyboard-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)

  ;; Set font
  (set-frame-font "Source Code Pro 11" t t)

  (add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))
  
;;; Packages
  (require 'package)
  (setq package--init-file-ensured t
        package-enable-at-startup nil
        package-archives user/package-mirror
        )

  ;; Evaluate the correct package subdirectory of packages.
  (setq package-user-dir
	(file-name-as-directory
	 (if (not elpa-subdirectory)
	     elpa-pack-dir
	   (let ((subdir (format "%d%s%d"
				 emacs-major-version
				 version-separator
				 emacs-minor-version)))
	     (expand-file-name subdir elpa-pack-dir))
	     )
	 ))

  ;; Load Emacs packages and initialize them.
  (package-initialize)

  ;; Install use-package from melpa
  (or (package-installed-p 'use-package)
      (progn
        (package-refresh-contents)
        (package-install 'use-package))
      )

  (require 'editor)
  (require 'completion)

  (require 'core-libs)
  (require 'keybindings)
  (require 'init-git)
  (require 'prog-common)
  (require 'prog-c-cpp)
  (require 'prog-python)
  (require 'prog-haskell)
  (require 'init-org)
  (require 'apps)

  (user/lazy-load)
  )


(provide 'init)
;;; init.el ends here
