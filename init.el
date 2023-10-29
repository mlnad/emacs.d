;; init.el --- M.Liu's emacs initilize file

;; Author: M.Liu <liumiaogemini@gmail.com>
;; License: See the LICENSE in the root directory.
;;
;;; Commentary:
;; This file is not a part of Emacs

;;; Code:
(setq debug-on-error t)

;; adjust garbage collection at startup
(defvar better-gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq gc-cons-percentage 0.6)))

;; add `lisp' to `load-path'.
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

(when-let (realhome
           (and *sys/win32*
                (getenv "USERPROFILE")))
  (setenv "HOME" realhome)
  (setq abbreviated-home-dir nil))

;; make cache directory
(unless (file-exists-p configs/cache-directory)
    (make-directory configs/cache-directory))
;; load user configs.
(unless (file-exists-p configs/userconfig-file)
    (copy-file (concat user-emacs-directory "lisp/templates/userconfig.template")
               configs/userconfig-file))
(load-file configs/userconfig-file)

(require 'core)
(require 'env)

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
(setq package-enable-at-startup nil
      package-archives configs/package-mirror
      package-user-dir (core/elpa-package-dir))

;; Load Emacs packages and initialize them.
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; Install use-package from melpa
(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(require 'editor)
(require 'ui)
(require 'completion)

(require 'program)
(require 'writting)
(require 'apps)
(require 'keybinds)

;; Configurations after init
(user/config-after-init)

(provide 'init)
;;; init.el ends here
