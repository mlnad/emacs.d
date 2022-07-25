;; init.el --- M.Liu's emacs initilize file

;; Author: M.Liu <liumiaogemini@gmail.com>
;; License: See the LICENSE in the root directory.
;;
;;; Commentary:
;; This file is not a part of Emacs

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

;; add `lisp' to `load-path'.
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

;; load user configs.
(or (file-exists-p configs/cache-directory)
    (make-directory configs/cache-directory))
(or (file-exists-p configs/userconfig-file)
    (copy-file (concat user-emacs-directory "lisp/templates/userconfig.template")
               configs/userconfig-file))
(load configs/userconfig-file)

(require 'core)

;; AutoGC
(add-hook 'emacs-startup-hook
          #'core/garbage-collect-h)

;; Config before init
(user/config-before-init)

;; load `custom-file'
(setq custom-file configs/custom-file)
(when (file-exists-p custom-file)
  (load custom-file))

(require 'cl-lib)
;; Language and coding
(set-language-environment "utf-8")

;;; Packages
(require 'package)
(setq package--init-file-ensured t
      package-enable-at-startup nil
      package-archives configs/package-mirror)

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
      quelpa-dir configs/quelpa-dir)

(require 'editor)
(require 'completion)

(require 'programming)
(require 'init-org)
(require 'apps)
(require 'keybinds)

;; Configurations after init
(user/config-after-init)

(provide 'init)
;;; init.el ends here
