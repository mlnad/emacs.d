;;; search.el --- Search configs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 John Doe
;;
;; Author: John Doe <http://github/l>
;; Maintainer: John Doe <john@doe.com>
;; Version: 0.0.1
;; Keywords:
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(use-package projectile
  :ensure t
  :defer t
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             )
  :init
  (progn
    (setq projectile-indexing-method 'alien
          projectile-generic-command "find . -type f"
          projectile-ignored-projects '("~/" "/tmp")
          projectile-globally-ignored-files '(".DS_Store" "TAGS")
          projectile-globally-ignored-directories '(".ccls-cache")
          projectile-kill-buffers-filter 'kill-only-files)

    (setq projectile-sort-order 'recentf
          projectile-cache-file user/projectile-cache-file
          projectile-known-projects-file user/projectile-known-projects-file)
    )
  :config
  (projectile-mode +1)

  (setq projectile-project-root-files-bottom-up
        (append '(".projectile"         ; projctiles's root marker
                  ".project"            ; doom project marker
                  ".git")               ; Git
                )
        ;; This will be filled by other pakages
        projectile-project-root-files '()
        projectile-project-root-files-top-down-recurring '("Makefile"))

  ;; Per-project compilation buffers
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name
        compilation-save-buffers-predicate #'projectile-current-project-buffer-p)

  :diminish projectile-mode
  :bind (("C-c p f" . 'counsel-projectile-find-file)
         ("C-c p p" . 'counsel-projectile-switch-project)
         ("C-c p b" . 'counsel-projectile-switch-to-buffer)
         ("C-c p k" . 'projectile-kill-buffers))
  )

(defun user/search-project (&optional arg)
  "Conduct a text search in the current project root.
If prefix ARG is set, include ignored/hidden files."
  (interactive "P")
  (let* ((projectile-project-root nil)
         (disabled-command-function nil)
         (current-prefix-arg (unless (eq arg 'other) arg))
         (default-directory
           (if (eq arg 'other)
               (if-let (projects (projectile-relevant-known-projects))
                   (completing-read "Search project: " projects nil t)
                 (user-error "There are no known projects"))
             default-directory)))
    (call-interactively
     #'ivy/project-search)
    ))

(provide 'search)
;;; search.el ends here
