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

(provide 'config-test)
;;; config-test.el ends here
