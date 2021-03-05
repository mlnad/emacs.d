;;; completion.el --- Completion configs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Liu Miao
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
             projectile-project-p)
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
          projectile-known-projects-file user/projectile-known-projects-file))
  :config
  (projectile-mode +1)

  (setq projectile-project-root-files-bottom-up
        (append '(".projectile"         ; projctiles's root marker
                  ".project"            ; doom project marker
                  ".git"))              ; Git
        ;; This will be filled by other pakages
        projectile-project-root-files '()
        projectile-project-root-files-top-down-recurring '("Makefile"))

  ;; Per-project compilation buffers
  (setq compilation-buffer-name-function #'projectile-compilation-buffer-name
        compilation-save-buffers-predicate #'projectile-current-project-buffer-p)
  (evil-define-key* nil 'global
    (kbd "<leader>pk") 'projectile-kill-buffers
    (kbd "<leader>pf") 'projectile-find-file
    (kbd "<leader>pb") 'projectile-switch-to-buffer
    (kbd "<leader>pp") 'projectile-switch-project)

  :diminish projectile-mode)
(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :init
  (let ((standard-seaarch-fn #'ivy--regex-plus)
        (alt-search-fn #'ivy--regex-ignore-order))
    (setq ivy-more-chars-alist
          `((counsel-rg . 1)
            (counsel-search . 2)
            (t . 3)))
    (setq ivy-re-builders-alist
          `((counsel-rg . ,standard-seaarch-fn)
            (swiper . ,standard-seaarch-fn)
            (swiper-isearch . ,standard-seaarch-fn))))
  (add-to-list 'user/evil-collection-mode-list 'ivy)
  :config
  (setq ivy-sort-max-size 7500)

  (require 'counsel nil t)

  (setq ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
        ivy-use-virtual-buffers nil
        ivy-virtual-abbreviate 'full
        ivy-on-del-error-function #'ignore
        ivy-use-selectable-prompt t)

  (global-set-key "\C-s" 'swiper)

  (dolist (map (list ivy-minibuffer-map
                     ivy-switch-buffer-map
                     ivy-reverse-i-search-map))
    (define-key map (kbd "C-j") 'ivy-next-line)
    (define-key map (kbd "C-k") 'ivy-previous-line))
  (evil-define-key* nil 'global
    (kbd "<leader>ss") 'swiper
    (kbd "<leader>sp") 'user/counsel-search-project
    (kbd "<leader>sd") 'user/counsel-search-dir))

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
                smex-save-file (concat user-emacs-directory "cache/smex-items")))

(provide 'completion)
;;; completion.el ends here
