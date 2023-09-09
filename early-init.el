;;; early-init.el --- early initilize Emacs -*- lexical-binding: t; -*-
;;
;; Author: Liu Miao
;;
;;; Commentary:
;; This file is not part of GNU Emacs
;;
;;; Code:
(setq gc-cons-threshold most-positive-fixnum)

(setq load-prefer-newer noninteractive)

(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))
;; Before Emacs 27, the init file was responsible for initializing the package
;; manager by calling `package-initialize'. Emacs 27 changed the default
;; behavior: It now calls `package-initialize' before loading the init file.
;; Earlier Emacs versions do not load the early init file and do not initialize
;; the package manager before loading the init file, so this file is neither
;; needed nor loaded on those versions.
;; (setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(load (concat user-emacs-directory "lisp/configs") nil 'nomessage)

;;; early-init.el ends here
