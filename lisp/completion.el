;;; completion.el --- Completion configs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Liu Miao
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
             projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-dired
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-test-project
             projectile-grep
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-p
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-replace-regexp
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :init
  (progn
    (setq projectile-ignored-projects '("~/" "/tmp")
          projectile-globally-ignored-files '(".DS_Store" "TAGS")
          projectile-kill-buffers-filter 'kill-all)
    (setq projectile-mode-line-function
          (lambda ()
            (if (file-remote-p default-directory) ""
              (projectile-default-mode-line))))

    (setq projectile-sort-order 'recentf
          projectile-cache-file user/projectile-cache-file
          projectile-known-projects-file user/projectile-known-projects-file))
  :config
  (projectile-mode +1)

  (setq projectile-project-root-files-bottom-up
        (append '(".projectile"         ; projctiles's root marker
                  ".project"            ; doom project marker
                  ".git"))              ; Git
        ;; This will be filled by other pakages
        projectile-project-root-files '()
        projectile-project-root-files-top-down-recurring '("Makefile" "CMakeLists.txt"))

  ;; Per-project compilation buffers
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name
        compilation-save-buffers-predicate #'projectile-current-project-buffer-p)

  :diminish projectile-mode)

(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package corfu
    ;; Optional customizations
    ;; :custom
    ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
    ;; (corfu-auto t)                 ;; Enable auto completion
    ;; (corfu-separator ?\s)          ;; Orderless field separator
    ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
    ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
    ;; (corfu-preview-current nil)    ;; Disable current candidate preview
    ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
    ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
    ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
    ;; (corfu-scroll-margin 5)        ;; Use scroll margin

    ;; Enable Corfu only for certain modes.
    ;; :hook ((prog-mode . corfu-mode)
    ;;        (shell-mode . corfu-mode)
    ;;        (eshell-mode . corfu-mode))

    ;; Recommended: Enable Corfu globally.
    ;; This is recommended since Dabbrev can be used globally (M-/).
    ;; See also `corfu-excluded-modes'.
    :init
    (global-corfu-mode))

(provide 'completion)
;;; completion.el ends here
