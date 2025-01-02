;;; moyu.el --- Command Line Supports -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Liu
;;
;; Author: Liu <liumiaogemini@foxmail.com>
;; Maintainer: Liu <liumiaogemini@foxmail.com>
;; Created: 十月 11, 2024
;; Modified: 十月 11, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage:
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Command Line Supports for Emacs
;;
;;; Code:

(defun moyu/tangle-init ()
  "Tangle the init.org file to generate Emacs configuration files."
  (interactive)
  (require 'org)
  (let ((init-file (expand-file-name "init.org" user-emacs-directory)))
    (if (file-exists-p init-file)
        (org-babel-tangle-file init-file)
      (message "init.org file not found in %s" user-emacs-directory))))

(provide 'moyu)
;;; moyu.el ends here
