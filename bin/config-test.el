;;; config-test.el --- Configuration integrity tests -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Verifies that the Emacs configuration defined in init.org is complete:
;; packages are installed, key files exist, and the environment is sane.
;;
;; Run with:  moyue test config
;;
;;; Code:

(require 'ert)
(require 'package)

;; Initialize package system so package-installed-p works.
(package-initialize)

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Helpers
;;;; ──────────────────────────────────────────────────────────────────────────

(defmacro config-defcheck (name doc &rest body)
  "Define a configuration check named NAME with DOC and BODY.
The test is registered under the `config/' namespace for easy filtering:
  moyue test config/NAME"
  (declare (indent 2) (doc-string 2))
  `(ert-deftest ,(intern (format "config/%s" name)) ()
     ,doc
     ,@body))

(defmacro config-check-package (pkg)
  "Assert that PKG is available: installed in elpa OR provided as a built-in."
  `(config-defcheck ,(intern (format "package-%s" pkg))
     ,(format "Package `%s' should be available (elpa or built-in)." pkg)
     (should (or (package-installed-p ',pkg)
                 (locate-library ,(symbol-name pkg))))))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Environment checks
;;;; ──────────────────────────────────────────────────────────────────────────

(config-defcheck emacs-version
  "Emacs version must be at least 28.1."
  (should (version<= "28.1" emacs-version)))

(config-defcheck init-org-exists
  "init.org must exist in user-emacs-directory."
  (should (file-exists-p
           (expand-file-name "init.org" user-emacs-directory))))

(config-defcheck init-el-exists
  "init.el must exist in user-emacs-directory (tangled from init.org)."
  (should (file-exists-p
           (expand-file-name "init.el" user-emacs-directory))))

(config-defcheck elpa-dir-exists
  "The elpa/ package directory must exist."
  (should (file-directory-p
           (expand-file-name "elpa" user-emacs-directory))))

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Package installation checks
;;;; ──────────────────────────────────────────────────────────────────────────

;; Bootstrap / completion framework
(config-check-package use-package)
(config-check-package vertico)
(config-check-package orderless)
(config-check-package consult)
(config-check-package corfu)
(config-check-package cape)
(config-check-package marginalia)
(config-check-package embark)
(config-check-package embark-consult)
(config-check-package tempel)

;; LSP / development
(config-check-package eglot)
(config-check-package consult-eglot)
(config-check-package apheleia)
(config-check-package dape)
(config-check-package projection)
(config-check-package projection-multi)
(config-check-package projection-multi-embark)

;; Language support
(config-check-package rust-mode)
(config-check-package rustic)
(config-check-package python)
(config-check-package pyimport)
(config-check-package poetry)
(config-check-package geiser)
(config-check-package lispy)
(config-check-package buttercup)
(config-check-package dockerfile-ts-mode)

;; Org-mode ecosystem
(config-check-package org)
(config-check-package org-roam)
(config-check-package org-modern)
(config-check-package valign)
(config-check-package org-fragtog)
(config-check-package gnuplot)

;; Markdown / TeX
(config-check-package markdown-mode)
(config-check-package auctex-latexmk)
(config-check-package cdlatex)

;; Version control
(config-check-package magit)
(config-check-package magit-todos)
(config-check-package diff-hl)

;; UI / theme
(config-check-package doom-themes)
(config-check-package doom-modeline)
(config-check-package nerd-icons-corfu)
(config-check-package all-the-icons)
(config-check-package writeroom-mode)
(config-check-package popper)
(config-check-package svg-tag-mode)

;; Editing
(config-check-package evil)
(config-check-package which-key)

;; Misc
(config-check-package rime)
(config-check-package docker)
(config-check-package aidermacs)

;;;; ──────────────────────────────────────────────────────────────────────────
;;;; Font configuration tests
;;;; ──────────────────────────────────────────────────────────────────────────

;; Bootstrap: load the face-fonts definitions from the tangled init.el so that
;; `moyu/face-fonts' and `moyu/apply-face-fonts' are available without loading
;; all of init.el (which needs a live display).
(let ((init-el (expand-file-name "init.el" user-emacs-directory)))
  (when (file-exists-p init-el)
    (with-temp-buffer
      (insert-file-contents init-el)
      (goto-char (point-min))
      ;; Org-babel tangles `:var face-fonts=face-fonts' as a let block; find it.
      (when (search-forward "(let ((face-fonts" nil t)
        (goto-char (match-beginning 0))
        (ignore-errors (eval (read (current-buffer))))))))

(config-defcheck face-fonts-structure
  "Every row in `moyu/face-fonts' is a (string string positive-number) triple."
  (should (boundp 'moyu/face-fonts))
  (should (listp moyu/face-fonts))
  (should (> (length moyu/face-fonts) 0))
  (dolist (row moyu/face-fonts)
    (should (= (length row) 3))
    (cl-destructuring-bind (face family size) row
      (should (stringp face))
      (should (and (stringp family) (not (string-empty-p family))))
      (should (and (numberp size) (> size 0))))))

(config-defcheck face-fonts-default-row
  "The face-fonts table must contain a `default' row."
  (should (boundp 'moyu/face-fonts))
  (should (cl-find "default" moyu/face-fonts :key #'car :test #'string=)))

(config-defcheck face-fonts-cjk-rows
  "face-fonts must contain a single CJK font row."
  (should (boundp 'moyu/face-fonts))
  (should (cl-find "cjk" moyu/face-fonts :key #'car :test #'string=))
  (should (= (length (cl-remove-if-not
                      (lambda (row) (string-prefix-p "cjk" (car row)))
                      moyu/face-fonts))
             1)))

(config-defcheck face-fonts-latin-unified
  "The Latin face rows (default, fixed-pitch, fixed-pitch-serif) share one font family."
  (should (boundp 'moyu/face-fonts))
  (let* ((latin-rows (cl-remove-if (lambda (r) (string-prefix-p "cjk" (car r)))
                                   moyu/face-fonts))
         (families (mapcar #'cadr latin-rows)))
    (should (cl-every (lambda (f) (string= f (car families))) families))))

(config-defcheck face-fonts-apply-dispatches-correctly
  "`moyu/apply-face-fonts' routes each row to the right handler."
  (should (fboundp 'moyu/apply-face-fonts))
  (let ((latin-calls 0) (cjk-calls 0))
    (cl-letf (((symbol-function 'display-graphic-p)     (lambda ()       t))
              ((symbol-function 'set-face-attribute)     (lambda (&rest _) (cl-incf latin-calls)))
              ((symbol-function 'set-fontset-font)       (lambda (&rest _) (cl-incf cjk-calls))))
      (moyu/apply-face-fonts))
    ;; default + fixed-pitch + fixed-pitch-serif → set-face-attribute ×3
    (should (= latin-calls 3))
    ;; cjk → set-fontset-font ×4 (han cjk-misc bopomofo kana)
    (should (= cjk-calls 4))))

(config-defcheck face-fonts-skipped-without-display
  "`moyu/apply-face-fonts' does nothing when there is no graphical display."
  (should (fboundp 'moyu/apply-face-fonts))
  (let ((called nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda () nil))
              ((symbol-function 'set-face-attribute) (lambda (&rest _) (setq called t))))
      (moyu/apply-face-fonts))
    (should-not called)))

(provide 'config-test)
;;; config-test.el ends here
