;; init.el --- M.Liu's emacs initilize file

;; Author: M.Liu <liumiaogemini@gmail.com>
;; License: See the LICENSE in the root directory.
;;
;;; Commentary:
;; This file is

;;; Code:
;; (setq debug-on-error t)


;; adjust garbage collection at startup
(defvar better-gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq gc-cons-percentage 0.6)))

;; Use a hook so the messages doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; AutoGC
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))
            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

;; add `lisp' to `load-path'.
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

;; load user configs.
(require 'configs)
(or (file-exists-p user/cache-directory)
    (make-directory user/cache-directory))
(or (file-exists-p user/userconfig-file)
    (copy-file (concat user-emacs-directory "lisp/templates/userconfig.template")
               user/userconfig-file))
(load user/userconfig-file)

;; Config before init
(user/config-before-init)

;; load `custom-file'
(setq custom-file user/custom-file)
(when (file-exists-p custom-file)
  (load custom-file))

(require 'cl-lib)
;; Language and coding
(set-language-environment "utf-8")
;; (set-keyboard-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)

;;; Packages
(require 'package)
(setq package--init-file-ensured t
      package-enable-at-startup nil
      package-archives user/package-mirror)

;; Evaluate the correct package subdirectory of packages.
(setq package-user-dir
	  (file-name-as-directory
	   (if (not elpa-subdirectory)
	       elpa-pack-dir
	     (let ((subdir (format "%d%s%d"
				               emacs-major-version
				               version-separator
				               emacs-minor-version)))
	       (expand-file-name subdir elpa-pack-dir)))))

;; Load Emacs packages and initialize them.
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; Install use-package from melpa
(unless (package-installed-p 'use-package)
  (progn
    (package-refresh-contents)
    (package-install 'use-package)))

;; Install quelpa from melpa
(unless (package-installed-p 'quelpa)
  (progn
    (package-refresh-contents)
    (package-install 'quelpa)))
(setq quelpa-checkout-melpa-p nil
      quelpa-dir user/quelpa-dir)

(require 'core-libs)
(require 'editor)
(require 'completion)

(require 'prog-common)
(require 'prog-c-cpp)
(require 'prog-python)
(require 'prog-lisp)
(require 'prog-rust)
(require 'prog-verilog)
(require 'init-org)
(require 'apps)
(require 'keybindings)

;; Configurations after init
(user/config-after-init)

(provide 'init)
;;; init.el ends here
