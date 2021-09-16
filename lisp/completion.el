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

(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-sort-max-size 7500)

  (require 'counsel nil t)

  (setq ivy-wrap t
        projectile-completion-system 'ivy
        ivy-use-virtual-buffers nil
        ivy-virtual-abbreviate 'full
        ivy-on-del-error-function #'ignore
        ivy-use-selectable-prompt t)

  (dolist (map (list ivy-minibuffer-map
                     ivy-switch-buffer-map
                     ivy-reverse-i-search-map))
    (define-key map (kbd "C-j") 'ivy-next-line)
    (define-key map (kbd "C-k") 'ivy-previous-line))

(defun user/counsel-search-rg (&optional use-initial-input initial-directory)
  "Searching with rg in Emacs.
If INITIAL-DIRECTORY is non nil start in that directory."
  (interactive)
  (require 'counsel)
  (let* ((initial-input (if use-initial-input
                            (if (region-active-p)
                                (buffer-substring-no-properties
                                 (region-beginning) (region-end))
                              (thing-at-point 'symbol t))
                          ""))
         (default-directory
           (or initial-directory (read-directory-name "Start from directory: "))))
    (counsel-rg initial-input default-directory nil "rg: ")))

(defun user/counsel-search-project()
  "Seraching project with rg."
  (interactive)
  (user/counsel-search-rg nil (projectile-project-root)))

(defun user/counsel-search-project-at-point ()
  "Seraching project with rg."
  (interactive)
  (user/counsel-search-rg (current-word t nil) (projectile-project-root)))

(defun user/counsel-search-dir ()
  "Searching directory with rg."
  (interactive)
  (user/counsel-search-rg nil default-directory))

(defun user/counsel-search-dir-at-point ()
  "Searching directory with rg."
  (interactive)
  (user/counsel-search-rg (current-word t nil) default-directory))

  (use-package swiper
    :ensure t
    :config
    (global-set-key "\C-s" 'swiper)))

(use-package ivy-avy
  :ensure t
  :after ivy)

(use-package ivy-xref
  :ensure t
  :defer t
  :init
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-references)))

(use-package counsel
  :ensure t
  :defer t
  :init
  (define-key!
    [remap apropos]                  #'counsel-apropos
    [remap bookmark-jump]            #'counsel-bookmark
    [remap describe-bindings]        #'counsel-descbinds
    [remap describe-face]            #'counsel-faces
    [remap describe-function]        #'counsel-describe-function
    [remap describe-variable]        #'counsel-describe-variable
    [remap evil-ex-registers]        #'counsel-evil-registers
    [remap evil-show-marks]          #'counsel-mark-ring
    [remap execute-extended-command] #'counsel-M-x
    [remap find-file]                #'counsel-find-file
    [remap find-library]             #'counsel-find-library
    [remap imenu]                    #'counsel-imenu
    [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
    [remap load-theme]               #'counsel-load-theme
    [remap locate]                   #'counsel-locate
    [remap org-goto]                 #'counsel-org-goto
    [remap org-set-tags-command]     #'counsel-org-tag
    [remap recentf-open-files]       #'counsel-recentf
    [remap set-variable]             #'counsel-set-variable
    [remap swiper]                   #'counsel-grep-or-swiper
    [remap unicode-chars-list-chars] #'counsel-unicode-char
    [remap switch-to-buffer]         #'counsel-switch-buffer
    [remap yank-pop]                 #'counsel-yank-pop)
  :config
  ;; Don't use ^
  (setq ivy-initial-inputs-alist nil)
  )

(use-package counsel-projectile
  :ensure t
  :init
  (define-key!
    [remap projectile-find-file]        #'counsel-projectile-find-file
    [remap projectile-find-dir]         #'counsel-projectile-find-dir
    [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer
    [remap projectile-grep]             #'counsel-projectile-grep
    [remap projectile-ag]               #'counsel-projectile-ag
    [remap projectile-switch-project]   #'counsel-projectile-switch-project)

  :config
  (ivy-set-display-transformer #'counsel-projectile-find-file nil)
  ;; (evil-define-key* nil 'global
  ;;   (kbd "<leader>pk") 'counsel-projectile-kill-buffers
  ;;   (kbd "<leader>pf") 'counsel-projectile-find-file
  ;;   (kbd "<leader>pb") 'counsel-projectile-switch-to-buffer
  ;;   (kbd "<leader>pp") 'counsel-projectile-switch-project)
  )

(use-package ivy-rich
  :ensure t
  :after counsel
  :init
  (setq ivy-rich-path-stytle 'abbrev
        ivy-virtual-abbreviate 'full)
  :config
  (progn
    (setq ivy-rich-parse-remote-buffer nil)
    (ivy-rich-mode)))

(use-package smex
  :ensure t
  :defer t
  :init
  (setq-default smex-history 32
                smex-save-file (expand-file-name "smex-items" user/cache-directory)))

(provide 'completion)
;;; completion.el ends here
