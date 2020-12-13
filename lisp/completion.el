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

(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :init
  (let ((standard-seaarch-fn #'ivy--regex-plus)
        (alt-search-fn #'ivy--regex-ignore-order))
    (setq ivy-more-chars-alist
          `((counsel-rg . 1)
            (counsel-search . 2)
            (t . 3))
          )
    (setq ivy-re-builders-alist
          `((counsel-rg . ,standard-seaarch-fn)
            (swiper . ,standard-seaarch-fn)
            (swiper-isearch . ,standard-seaarch-fn)))
;;            (t . ,alt-search-fn)) ;; Error when use this line
    )
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
        ivy-use-selectable-prompt t
        )
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
          ivy-virtual-abbreviate 'full
          ))
  :config
  (progn
    (setq ivy-rich-parse-remote-buffer nil)
    (ivy-rich-mode)
    )
  )

;; (use-package ivy-posframe
;;   :ensure t
;;   :hook (ivy-mode . ivy-posframe-mode)
;;   :config
;;   (setq ivy-fixed-height-minibuffer nil
;;         ivy-postframe-border-width 10
;;         ivy-postframe-parameters
;;         `((min-width . 90)
;;           (min-height . ,ivy-height)))
;;   )

(cl-defun ivy-file-search (&key query in all-files (recursive t) prompt args)
  "Conduct a file search using ripgrep.

:query STRING
  Determines the initial input to search for.
:in PATH
  Sets what directory to base the search out of. Defaults to the current
  project's root.
:recursive BOOL
  Whether or not to search files recursively from the base directory."
  (declare (indent defun))
  (unless (executable-find "rg")
    (user-error "Couldn't find ripgrep in your PATH"))
  (require 'counsel)
  (let* ((this-command 'counsel-rg)
         (project-root default-directory)
         (directory (or in project-root))
         (args (concat (if all-files " -uu")
                       (unless recursive " --maxdepth 1")
                       " --hidden -g!.git "
                       (mapconcat #'shell-quote-argument args " "))))
    (setq deactivate-mark t)
    (counsel-rg
     ;; (or query
     ;;     (when (doom-region-active-p)
     ;;       (replace-regexp-in-string
     ;;        "[! |]" (lambda (substr)
     ;;                  (cond ((and (string= substr " ")
     ;;                              (not (featurep! +fuzzy)))
     ;;                         "  ")
     ;;                        ((string= substr "|")
     ;;                         "\\\\\\\\|")
     ;;                        ((concat "\\\\" substr))))
     ;;        (rxt-quote-pcre (doom-thing-at-point-or-region)))))
     query
     directory args
     (or prompt
         (format "rg%s [%s]: "
                 args
                 (cond ((equal directory default-directory)
                        "./")
                       ((equal directory project-root)
                        (projectile-project-name))
                       ((file-relative-name directory project-root))))))))


(defun ivy/project-search (&optional arg initial-query directory)
  "Performs a live project search from the project root using ripgrep.

If ARG (universal argument), include all files, even hidden or compressed ones,
in the search."
  (interactive "P")
  (ivy-file-search :query initial-query :in directory :all-files arg))

(defun ivy/project-search-from-cwd (&optional arg initial-query)
  "Performs a project search recursively from the current directory.

If ARG, include all files."
  (interactive "P")
  (ivy/project-search arg initial-query default-directory)
  )

(provide 'completion)
;;; completion.el ends here
