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
  :commands (projectile-project-root
             projectile-project-name
             projectile-project-p
             projectile-compile-project
             projectile-dired
             projectile-find-dir
             projectile-find-file
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-recentf
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
          projectile-cache-file configs/projectile-cache-file
          projectile-known-projects-file configs/projectile-known-projects-file))
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

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-resize nil
        vertico-scroll-margin 0
        vertico-cycle t)
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char))
  :config
  (cl-defun vetico/search-files (&key query path all-files (recursive t) prompt args)
    "Conduct a file search using ripgrep.
:query STRING
	The initial input to search for.
:path PATH
	Set the base directory to search out of. Default to the current project's root.
:recursive BOOL
	Wheather or not to search files recursively from the base directory."
    (declare (indent defun))
    (unless (executable-find "rg")
      (user-error "Couldn't find ripgrep in your PATH"))
    (require 'consult)
    ())

  (defun vertico/search-project (&optional arg)
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
      ()))
  ;; Bind directory delete
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package consult
  :ensure t
  :defer t
  :after (projectile vertico)
  :bind (([remap apropos]                       . consult-apropos)
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap evil-show-marks]               . consult-mark)
         ([remap evil-show-registers]           . consult-register)
         ;;([remap evil-show-jumps]             . consult-xref)
         ([remap goto-line]                     . consult-goto-line)
         ([remap imenu]                         . consult-imenu)
         ([remap locate]                        . consult-locate)
         ([remap load-theme]                    . consult-theme)
         ([remap man]                           . consult-man)
         ([remap recentf-open-files]            . consult-recent-file)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
         ([remap projectile-switch-to-buffer]   . consult-project-buffer)
         ([remap yank-pop]                      . consult-yank-pop))
  :preface
  (advice-add #'multi-occur :override #'consult-multi-occur)
  :config
  (setq consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay 0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file))

(use-package corfu
  :ensure t
  ;; Optional customizations
  :custom
    (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
    (corfu-auto t)                 ;; Enable auto completion
    (corfu-separator ?\s)          ;; Orderless field separator
    ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
    ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
    ;; (corfu-preview-current nil)    ;; Disable current candidate preview
    ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
    ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
    (corfu-echo-documentation nil) ;; Disable documentation in the echo area
    (corfu-scroll-margin 5)        ;; Use scroll margin

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
