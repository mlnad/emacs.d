;;; init-ivy.el --- -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 John Doe
;;
;; Author: John Doe <http://github/L>
;; Maintainer: John Doe <john@doe.com>
;; Keywords:
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
  :init
  (progn
    (setq projectile-indexing-method 'alien
    projectile-generic-command "find . -type f")
    (setq projectile-sort-order 'recentf
    projectile-cache-file user/projectile-cache-file
    projectile-known-projects-file user/projectile-known-projects-file)
    )
  :config
  (projectile-mode +1)
  :diminish projectile-mode
  :bind (("C-c p f" . 'counsel-projectile-find-file)
   ("C-c p p" . 'counsel-projectile-switch-project)
   ("C-c p b" . 'counsel-projectile-switch-to-buffer)
   ("C-c p k" . 'projectile-kill-buffers))
  )

(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :init
  (let ((standard-seaarch-fn #'ivy--regex-plus)
        (alt-search-fn #'ivy--regex-ignore-order))
    (setq ivy-re-builders-alist
          `((counsel-rg . ,standard-seaarch-fn)
            (swiper . ,standard-seaarch-fn)
            (swiper-isearch . ,standard-seaarch-fn)
            (t . ,alt-search-fn))
          ivy-more-chars-alist
          '((counsel-rg . 1)
            (counsel-search . 2)
            (t . 3))))
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
  )

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
    [remap yank-pop]                 #'counsel-yank-pop)
  :config
  ;; Don't use ^
  (setq ivy-initial-inputs-alist nil)

  
  )

(use-package counsel-projectile
  :ensure t
  :defer t
  :init
  (define-key!
    [remap projectile-find-file]        #'+ivy/projectile-find-file
    [remap projectile-find-dir]         #'counsel-projectile-find-dir
    [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer
    [remap projectile-grep]             #'counsel-projectile-grep
    [remap projectile-ag]               #'counsel-projectile-ag
    [remap projectile-switch-project]   #'counsel-projectile-switch-project)

  :config
  (ivy-set-display-transformer #'counsel-projectile-find-file nil)
  )

(use-package ivy-rich
  :ensure t
  :after counsel
  :init
  (progn
    (setq ivy-rich-path-stytle 'abbrev
          ivy-virtual-abbreviate 'full))
  :config
  (progn
    (setq ivy-rich-parse-remote-buffer nil)
    (ivy-rich-mode)
    )
  )

(use-package ivy-posframe
  :ensure t
  :hook (ivy-mode . ivy-posframe-mode)
  :config
  (setq ivy-fixed-height-minibuffer nil
        ivy-postframe-border-width 10
        ivy-postframe-parameters
        `((min-width . 90)
          (min-height . ,ivy-height)))
  )

(provide 'completion)
;;; completion.el ends here
