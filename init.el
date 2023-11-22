(defvar config/full-name "Liu Miao")
(defvar config/email-address "liumiaogemini@foxmail.com")

(defconst *sys/win32*
  (eq system-type 'windows-nt))

(defconst *sys/linux*
  (eq system-type 'gnu/linux))

(defconst *sys/mac*
  (eq system-type 'darwin))

(defvar ripgrep-p
  (executable-find "rg"))

(defvar grep-p
  (executable-find "grep"))

(defvar configs/default-font '("Consolas"
                               :size 10
                               :weight normal
                               :width normal))

(defvar configs/unicode-font '("Noto Sans Mono CJK SC"))

(defvar configs/cache-directory
  (expand-file-name ".cache/" user-emacs-directory))

(defvar configs/userconfig-file
  (expand-file-name "userconfig" configs/cache-directory))

(defvar configs/custom-file
  (expand-file-name "custom.el" configs/cache-directory))

(defvar configs/recentf-save-file
  (expand-file-name "recentf" configs/cache-directory))

(defvar configs/save-place-file
  (expand-file-name "places" configs/cache-directory))

(defvar configs/backup-directory-alist
  (expand-file-name "backup/" configs/cache-directory))

(defvar configs/project-list-file
  (expand-file-name "projects" configs/cache-directory))

(defvar configs/auto-save-list-prefix
  (expand-file-name "auto-save-list/.saves-" configs/cache-directory))

(defvar configs/layouts-directory
  (expand-file-name "layouts/" configs/cache-directory))

(defvar configs/quelpa-dir
  (expand-file-name "quelpa/" configs/cache-directory))

(defvar configs/notes-dir "~/org/"
  "User defined notes directory.")

(defvar configs/org-roam-dir configs/notes-dir
  "User defined org roam directory.")

(defvar configs/org-roam-db-location
  (expand-file-name "org-roam.db" configs/cache-directory))

(defvar configs/notes-extensions '("org" "md" "markdown"))

(defvar configs/rime-data-dir
  (expand-file-name "rime/" configs/cache-directory))

(defvar configs/elpa-pack-dir
  (expand-file-name "elpa" user-emacs-directory )
  "Packages install by package-initilize.")

(defvar configs/elpa-subdirectory 'emacs-version)

(defvar default-package-mirror '(("melpa" . "https://melpa.org/packages/")
                                 ("gnu" . "https://elpa.gnu.org/packages/")))

(defvar emacs-china-package-mirror '(("gnu"   . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                                     ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")
                                     ("nongnu"   . "http://mirrors.ustc.edu.cn/elpa/nongnu/")))

(defvar emacs-tuna-package-mirror '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                                    ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                                    ("nongnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))

(defvar configs/package-mirror default-package-mirror)

(defvar configs/org-journal-type 'daily)

(defvar configs/theme 'doom-one)

(defvar configs/profile-eln-caches-dir (expand-file-name "eln-caches" configs/cache-directory))

(defvar configs/env-file
  (expand-file-name "env" configs/cache-directory))

(defvar configs/transient-history-file
  (expand-file-name "transient/history.el" configs/cache-directory))

;; add `lisp' to `load-path'.
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))

(when-let (realhome
           (and *sys/win32*
                (getenv "USERPROFILE")))
  (setenv "HOME" realhome)
  (setq abbreviated-home-dir nil))

;; make cache directory
(unless (file-exists-p configs/cache-directory)
  (make-directory configs/cache-directory))
;; load user configs.
(unless (file-exists-p configs/userconfig-file)
  (copy-file (concat user-emacs-directory "lisp/templates/userconfig.template")
             configs/userconfig-file))
(load-file configs/userconfig-file)

;; load `custom-file'
(setq custom-file configs/custom-file)
(when (file-exists-p custom-file)
  (load custom-file))

;; Language and coding
(set-language-environment "utf-8")

(require 'core)
(require 'cl-lib)
(require 'package)

;; adjust garbage collection at startup
(defvar better-gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq gc-cons-percentage 0.6)))

;; AutoGC
(add-hook 'emacs-startup-hook
          #'core/garbage-collect-h)

(setq package-enable-at-startup nil
      package-archives configs/package-mirror
      package-user-dir (core/elpa-package-dir))

;; Load Emacs packages and initialize them.
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; Install use-package from melpa
(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  (defun crm-indicator (args)
    (cons (concat "[CRM]" (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable indentation+completion using the TAB key.
  (setq tab-always-indent t)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t
        echo-keystrokes 0.02
        resize-mini-windows 'grow-only
        max-mini-window-height 0.15
        find-file-visit-truename t
        vc-follow-symlinks t
        find-file-suppress-same-file-warnings t
        create-lockfiles nil
        make-backup-files nil
        ring-bell-function 'ignore
        version-control t
        backup-by-copying t
        delete-old-versions t
        kept-old-versions 5
        kept-new-versions 5
        backup-directory-alist configs/backup-directory-alist
        hscroll-margin 2
        hscroll-step 1
        scroll-conservatively 101
        scroll-margin 0
        scroll-preserve-screen-position t
        auto-window-vscroll nil
        mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 2
        auto-save-list-file-prefix configs/auto-save-list-prefix
        tabify-regexp "^\t* [ \t]+")

  (setq-default auto-image-file-mode t
                initial-scratch-message nil
                inhibit-splash-screen t
                initial-major-mode 'org-mode
                frame-title-format "%b"
                tab-width 4
	            indent-tabs-mode nil
                fill-column 80
                word-wrap t
                truncate-lines t)

  (when (boundp 'native-comp-eln-load-path)
    (add-to-list 'native-comp-eln-load-path configs/profile-eln-caches-dir))

  (fset #'yes-or-no-p #'y-or-n-p)

  :config
  (delete-selection-mode 1)
  (electric-pair-mode 1)
  (size-indication-mode t))

(when (>= emacs-major-version 28)
  (setq-default word-wrap-by-category t))

;; Default to soft line-wrapping in text modes.
(add-hook 'text-mode-hook #'visual-line-mode)

;; Create missing directory when we open a file that doesn't exist under
;; a directory tree tha may not exist.
(add-hook 'find-file-not-found-functions #'core/create-if-not-found)

(unless *sys/win32*
  (setq tramp-default-method "ssh"
        tramp-backup-directory-alist backup-directory-alist
        tramp-auto-save-directory (expand-file-name "tramp-autosave/" configs/cache-directory)
        tramp-backup-directory-alist (expand-file-name "backup/" configs/cache-directory)))

(with-eval-after-load 'tramp
  (setq remote-file-name-inihibit-cache 60
        tramp-completion-reread-directory-timeout 60
        tramp-verbose 1))

(use-package paren
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package recentf
  :commands (recentf-save-list)
  :init
  (add-hook 'find-file-hook (lambda ()
                              (unless recentf-mode
                                (recentf-mode)
                                (recentf-track-opened-file))))
  (setq recentf-save-file configs/recentf-save-file
        recentf-max-saved-items 1000
        recentf-auto-cleanup 'never)

  (recentf-mode 1))

(use-package savehist
  :init
  ;; Minibuffer history
  (setq savehist-file (expand-file-name "savehist" configs/cache-directory))
  (savehist-mode)
  :config
  (setq savehist-save-minibuffer-history t
        history-length 100
        savehist-autosave-interval 60
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history
                                        kill-ring)))

(use-package saveplace
  :hook (after-init . save-place-mode)
  :init
  (setq save-place-file configs/save-place-file))

(use-package subword
  :hook (after-init . global-subword-mode))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

(use-package imenu)

(when (>= emacs-major-version 27)
  (use-package display-fill-column-indicator))

(use-package compile
  :config
  (setq compilation-always-kill t
        compilation-ask-about-save nil
        compilation-scroll-output 'first-error))

(use-package smartparens-mode
  :ensure smartparens
  :hook (prog-mode text-mode markdown-mode org-mode)
  :config
  (require 'smartparens-config))

(use-package restart-emacs
  :ensure t)

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("DEL" . vertico-directory-delete-char))
  :init
  (vertico-mode)
  (setq vertico-resize nil
        vertico-cycle t)
  :config
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package consult
  :ensure t
  :after (vertico)
  :bind (([remap apropos]                       . consult-apropos)
         ([remap bookmark-jump]                 . consult-bookmark)
         ([remap evil-show-marks]               . consult-mark)
         ([remap evil-show-registers]           . consult-register)
         ([remap goto-line]                     . consult-goto-line)
         ([remap imenu]                         . consult-imenu)
         ([remap locate]                        . consult-locate)
         ([remap load-theme]                    . consult-theme)
         ([remap man]                           . consult-man)
         ([remap recentf-open-files]            . consult-recent-file)
         ([remap switch-to-buffer]              . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame]  . consult-buffer-other-frame)
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

(use-package consult-xref
  :after xref
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

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
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package project
  :bind (([remap project-shell] . project-eshell))
  :init
  (setq project-list-file configs/project-list-file))

(defvar program/lsp-client 'eglot)

(defvar program/build-actions-map (make-sparse-keymap))

(defvar program/debug-actions-map (make-sparse-keymap))

(use-package eglot
  :ensure t
  :init
  (advice-add #'eglot-ensure :around
              (lambda (fn)
                (when (alist-get major-mode eglot-server-programs nil nil
                                 (lambda (modes key)
                                   (if (listp modes)
                                       (member key modes)
                                     (eq key modes))))
                  (funcall fn))))
  (setq eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5))

(use-package consult-eglot
  :ensure t
  :bind (([remap xref-find-apropos] . consult-eglot-symbols)))

(use-package yasnippet
  :ensure t
  :commands (yas-minor-mode-on
             yas-expand
             yas-expand-snippet
             yas-lookup-snippet
             yas-insert-snippet
             yas-new-snippet
             yas-visit-extra-mode
             yas-active-extra-mode
             yas-deactive-extra-mode
             yas-maybe-expand-abbrev-key-filter)
  :init
  (setq yas-trigger-in-field t
        yas-wrap-around-region t
        yas-prompt-functions '(yas-completing-prompt))

  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode)
  :config
  (add-hook 'prog-mode-hook 'yas-reload-all))

(use-package elisp-mode
  :mode ("\\.Cask\\'" . emacs-lisp-mode)
  :config
  (add-hook 'emacs-lisp-mode-hook #'outline-minor-mode))

(use-package buttercup
  :ensure t
  :mode ("/test[/-].+\.el$" . buttercup-minor-mode))

(use-package rustic
  :ensure t
  :mode ("\\.rs$" . rustic-mode)
  :config
  (setq rustic-indent-method-chain t
        rustic-babel-format-src-block nil)

  ;; HACK `rustic-lsp' sets up lsp-mode/eglot too early. We move it to
  ;;      `rustic-mode-local-vars-hook' so file/dir local variables can be used
  ;;      to reconfigure them.
  (setq rustic-lsp-client program/lsp-client))

(use-package python
  :mode (("\\.py\\'" . python-mode))
  :custom
  (python-indent-offset 4))

(use-package geiser
  :ensure t
  :commands run-geiser)

(defvar org/default-roam-capture
  '("d" "default" plain "%?"
    :if-new (file+head "${slug}.org"
                       "#+title: ${title}\n\n#+startup: indent\n")
    :unnarrowed t))

(defvar org/roam-templates nil)

(defvar org/roam-dailies-map (make-sparse-keymap))

(defvar org/todo-keywords
  '((sequence "TODO(t)" "LOOP(r)" "START(s)" "WAIT(w)" "HOLD(h)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")))

(use-package org
  :preface
  ;; org files
  (setq-default org-directory configs/notes-dir)
  (setq org-id-locations-file (expand-file-name ".orgids" org-directory)
        org-preview-latex-image-directory (concat configs/cache-directory "org/latex/")
        org-list-allow-alphabetical t)
  ;; org babels
  (setq org-src-preserve-indentation t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-link-elisp-confirm-function nil
        org-src-window-setup 'other-window)
  ;; org faces
  (setq org-indirect-buffer-display 'current-window
        org-log-done 'time
        org-enforce-todo-dependencies t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-hide-leading-stars t
        org-image-actual-width nil
        org-startup-with-inline-images t
        org-imenu-depth 6
        org-startup-indented t
        org-tags-column 0
        org-startup-folded nil)
  (setq org-todo-keywords org/todo-keywords)
  ;; org agenda
  (setq-default org-agenda-files (list (concat "agendas/" configs/notes-dir))
                org-agenda-skip-unavailable-files t
                org-agenda-span 20
                org-agenda-start-on-weekday nil
                org-agenda-start-day "-5d"
                org-agenda-inhibit-startup t)
  ;; attachements
  (setq org-attach-store-link-p t
        org-attach-use-inheritance t))

(use-package org-roam
  :ensure org-roam
  :hook (after-init . org-roam-db-autosync-enable)
  :custom
  (org-roam-directory configs/org-roam-dir)
  :commands (org-roam-buffer-toggle-display
             org-roam-tag-add
             org-roam-tag-delete)
  :init
  (require 'org-roam-dailies)
  (setq org-roam-db-location configs/org-roam-db-location)
  :config
  (add-to-list 'org/roam-templates org/default-roam-capture)
  (setq org-roam-capture-templates org/roam-templates
        org-roam-node-display-template "${org-hierarchy}"))

(cl-defmethod org-roam-node-org-hierarchy ((node org-roam-node))
  "Return hierarchy for NODE, constructed of its file title, OLP and direct title.
If some elements are missing, the will be stripped out."
  (let* ((title (org-roam-node-title node))
         (olp (org-roam-node-olp node))
         (level (org-roam-node-level node))
         (filetitle (or (if (= level 0)
                            title
                          (org-roam-node-file-title node))))
         (separator (propertize ":" 'face 'shadow)))
    (cl-case level
      (0 filetitle)
      (1 (concat (propertize filetitle 'face '(shadow italic))
                 separator title))
      (t (concat (propertize filetitle 'face '(shadow italic))
                 separator (propertize (string-join olp separator) 'face '(shadow italic))
                 separator title)))))

;;;###autoload
(defun org/find-in-notes ()
  "Find file in notes directory."
  (interactive)
  (completion/find-file configs/notes-dir))

(use-package valign
  :ensure t
  :config
  (add-hook 'org-mode-hook #'valign-mode))

(use-package gnuplot
  :ensure gnuplot)

(use-package markdown-mode
  :ensure t
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :init
  (setq markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-fontify-whole-heading-line t))

(setq TeX-parse-self t
      TeX-auto-save t
      TeX-auto-local ".auctex-auto"
      TeX-style-local ".auctex-style"
      TeX-source-correlate-mode t
      TeX-source-correlate-method 'synctex
      TeX-save-query nil)

(use-package auctex-latexmk
  :ensure t
  :after latex
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

(use-package tex
  :ensure auctex)

(with-eval-after-load 'bibtex
  (setq bibtex-align-at-equal-sign t
        bibtex-text-indentation 20))

(use-package magit
  :ensure t)

(use-package magit-gitflow
  :ensure t
  :hook (maigt-mode . turn-on-magit-gitflow))

(use-package magit-todos
  :ensure t)

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

(use-package rime
  :ensure t
  :if (not *sys/win32*)
  :custom
  (rime-show-candidate 'posframe)
  (default-input-method "rime")
  (rime-user-data-dir configs/rime-data-dir)
  :config
  (add-hook 'after-init-hook
            (lambda ()
              (unless (file-exists-p configs/rime-data-dir)
                (make-directory configs/rime-data-dir)
                (copy-file (concat user-emacs-directory "lisp/templates/user.yaml")
                           (concat configs/rime-data-dir "user.yaml"))))))

(with-eval-after-load 'eshell
  (setq eshell-banner-message '(format "%s %s\n"
                                (propertize (format " %s " (string-trim (buffer-name)))
                                            'face 'mode-line-highlight)
                                (propertize (current-time-string)
                                            'face 'font-lock-keyword-face))
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-kill-processes-on-exit t
        eshell-hist-ignoredups t
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t))

(defvar keybinds/leader-key "<SPC>"
  "The leader prefix key.")

(defvar keybinds/localleader-key "M-m"
  "The localleader prefix key.")

(defun keybinds/define-key (keymap &rest binds)
  "Define KEY-OPs at KEYMAP."
  (while (length> binds 1)
    (define-key keymap (kbd (pop binds)) (pop binds))))

(defmacro keybinds/set-leader (states keymap &rest binds)
  `(evil-define-key ,states ,keymap
     ,@(let ((binds-list))
         (while (length> binds 1)
           (add-to-list 'binds-list `(kbd ,(concat "<leader>" (pop binds))) t)
           (add-to-list 'binds-list (pop binds) t))
         binds-list)))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 5)
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-show-early-on-C-h t
        which-key-max-description-length nil))

(use-package undo-fu
  :ensure t)

;;; Keybinding
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil
        evil-want-integration t)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-fu)
  (evil-set-leader '(normal motion visual) (kbd keybinds/leader-key))
  (evil-set-leader '(insert replace emacs) (kbd keybinds/localleader-key)))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))

(defvar keybinds/file-manage-map
  (let ((map (make-sparse-keymap)))
    (keybinds/define-key map
                         "f" #'find-file
                         "s" #'save-buffer
                         "S" #'write-file
                         "r" #'recentf-open-files)
    map)
  "Emacs file management commands.")

(defvar keybinds/buffer-manage-map
  (let ((map (make-sparse-keymap)))
    (keybinds/define-key map
                         "b" #'consult-buffer
                         "d" #'kill-current-buffer
                         "i" #'ibuffer
                         "k" #'kill-buffer-and-window
                         "r" #'revert-buffer
                         "R" #'rename-buffer
                         "]" #'next-buffer
                         "[" #'previous-buffer
                         "x" #'kill-buffer-and-window)
    map)
  "Emacs buffer management commands.")

(defvar keybinds/code-actions-map
  (let ((map (make-sparse-keymap)))
    (keybinds/define-key map
                         "r" #'eglot-rename
                         "a" #'eglot-code-actions
                         "c" #'compile
                         "C" #'recompile
                         "b" (cons "build" program/build-actions-map))
    map)
  "Code actions.")

(defvar keybinds/search-map
  (let ((map (make-sparse-keymap)))
    (keybinds/define-key map
                         "i" #'imenu
                         "s" #'consult-line
                         "S" #'consult-mark
                         "p" #'completion/search-project
                         "d" #'completion/search-cwd
                         "M" #'consult-man)
    map)
  "Searching in Emacs.")

(defvar keybinds/git-actions-map
  (let ((map (make-sparse-keymap)))
    (keybinds/define-key map
                         "g" #'magit-status
                         "b" #'magit-branch-checkout
                         "t" #'git-timemachine-toggle
                         "C" #'magit-clone
                         "S" #'magit-stage-file
                         "U" #'magit-unstage-file
                         "R" #'vc-revert)
    map)
  "Version control")

(defvar keybinds/goto-actions-map
  (let ((map (make-sparse-keymap)))
    (keybinds/define-key map
                         "l" #'goto-line)))

(defvar keybinds/notes-manage-map
  (let ((map (make-sparse-keymap)))
    (keybinds/define-key map
                         "c" #'org-capture
                         "r" #'org-roam-node-find
                         "n" #'org-roam-capture
                         "v" #'org-search-view
                         "f" #'org/find-in-notes
                         "d" (cons "daily" org/roam-dailies-map))
    map))

(defvar keybinds/open-map
  (let ((map (make-sparse-keymap)))
    (keybinds/define-key map
                         "a" (cons "Agenda" #'org-agenda)
                         "f" #'make-frame
                         "F" #'select-frame-by-name
                         "s" #'eshell
                         "t" #'org-todo-list
                         "T" #'core/toggle-profiler)
    map)
  "Open someting")

(defvar keybinds/quit-map
  (let ((map (make-sparse-keymap)))
    (keybinds/define-key map
                         "d" #'core/restart-server
                         "K" #'kill-emacs
                         "R" #'restart-emacs
                         "f" #'delete-frame)
    map)
  "Quit Emacs")

(keybinds/define-key help-map
                     "'" #'describe-char
                     "a" #'apropos
                     "A" #'apropos-documentation
                     "F" #'describe-face
                     "t" #'load-theme
                     "p" #'find-library
                     "C-l" #'describe-language-environment
                     "C-m" #'info-emacs-manual
                     "C-c" #'describe-coding-system)

(keybinds/define-key evil-window-map
                     "m" #'delete-other-windows
                     "u" #'winner-undo
                     "d" #'evil-window-delete
                     "T" #'tear-off-window)

(keybinds/set-leader nil 'global
                     "<SPC>" '("Exec" . execute-extended-command)
                     "." '("Find file" . find-file)
                     "a" '("Actions" . embark-act)
                     "b" (cons "buffer" keybinds/buffer-manage-map)
                     "c" (cons "code" keybinds/code-actions-map)
                     "f" (cons "file" keybinds/file-manage-map)
                     "g" (cons "git" keybinds/git-actions-map)
                     "G" (cons "goto" keybinds/goto-actions-map)
                     "h" (cons "help" help-map)
                     "n" (cons "notes" keybinds/notes-manage-map)
                     "p" (cons "projects" project-prefix-map)
                     "q" (cons "quit/restart" keybinds/quit-map)
                     "o" (cons "open" keybinds/open-map)
                     "s" (cons "searching" keybinds/search-map)
                     "w" (cons "window" evil-window-map))

(defun editor/init-font ()
  "Initialize Emacs font."
  (when (find-font (font-spec :name (car configs/default-font)))
    (let* ((font (car configs/default-font))
           (props (cdr configs/default-font))
           (fontspec (apply 'font-spec :name font props)))
      (set-frame-font fontspec nil t)))
  (when (find-font (font-spec :name (car configs/unicode-font)))
    (let* ((font (car configs/unicode-font))
           (props (cdr configs/unicode-font))
           (fontspec (apply 'font-spec :name font props)))
      (set-fontset-font t 'unicode fontspec)))
  (run-hooks 'after-setting-font-hook))

(let ((hook (if (daemonp)
                'server-after-make-frame-hook
              'after-init-hook)))
  (add-hook hook #'editor/init-font -100))

(defun editor/init-theme ()
  "Initialize Emacs theme."
  (when (and configs/theme (not (custom-theme-enabled-p configs/theme)))
    (disable-theme custom-enabled-themes)
    (load-theme configs/theme t)))

(let ((hook (if (daemonp)
                'server-after-make-frame-hook
              'after-init-hook)))
  (add-hook hook #'editor/init-theme -99))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-bar-width 3
        doom-modeline-github nil
        doom-modeline-mu4e nil
        doom-modeline-persp-name nil
        doom-modeline-minor-modes nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-buffer-encoding t)

  (when (daemonp)
    (setq doom-modeline-icon t))

  :config
  (setq doom-modeline-project-detection 'project)

  (use-package anzu
    :ensure t)
  (use-package evil-anzu
    :ensure t
    :config (global-anzu-mode +1)))

(use-package display-line-numbers
  :hook
  (prog-mode . display-line-numbers-mode))

;; Don't display floating tooltips;
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

(add-hook 'emacs-startup-hook #'window-divider-mode)

(use-package writeroom-mode
  :ensure t
  :config
  (setq writeroom-width 100)
  (setq writeroom-global-effects
        '(writeroom-set-alpha
          writeroom-set-menu-bar-lines
          writeroom-set-tool-bar-lines
          writeroom-set-vertical-scroll-bars
          writeroom-set-bottom-divider-width)))

(use-package popper
  :ensure t
  :bind (("C-`"    . popper-toggle)
         ("M-`"    . popper-cycle))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\.*eshell.*\\*$" eshell-mode
          "\\.*-shell.*\\*$" shell-mode
          "^\\*vc-diff"
          "^\\*Python"
          "^\\*Completions"
          "^\\*cargo-.*\\*$"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  :config
  (setq popper-group-function #'popper-group-by-project
        popper-mode-line-position 2))
