;; init.el --- M.Liu's emacs initilize file

;; Author: M.Liu <liumiaogemini@gmail.com>
;; License: See the LICENSE in the root directory.
;;
;;; Commentary:
;; This file is

;;; Code:
;;======================================================================================
;; (setq debug-on-error t)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; (require 'cl)
(require 'cl-lib)
;; Language and coding
(set-language-environment 'Chinese-GB)
(set-keyboard-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; consts==============================================================================
(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defconst *is-a-win* (eq system-type 'windows-nt))
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst elpa-pack-dir
  (expand-file-name "elpa" user-emacs-directory )
  "Packages install by package-initilize.")

;; variables----------------------------------------------------------------------------
(defvar helm-display-buffer-regexp
  `("*.*helm.**"
    (display-buffer-in-side-window)
    (inhibit-same-window . t)
    (side . bottom)
    (window-width . 0.6)
    (window-height . 0.4)))

;;; My Functions=======================================================================
(defun open-init-file()
  "Find and open the init.el."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun load-init-file()
  "Load init.el."
  (interactive)
  (load-file (concat user-emacs-directory "init.el"))
  )

;;; Rename the current file
(defun rename-this-file-and-buffer (new-name)
  "Rename both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
    (filename (buffer-file-name)))
  (unless filename
    (error "Buffer '%s' is not visiting a file! " name))
  (progn
    (when (file-exists-p filename)
      (rename-file filename new-name 1))
    (set-visited-file-name new-name)
    (rename-buffer new-name))
 ))

(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))

;;-----------------------------------------------------------------------

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(defun add-to-hook (fun hooks)
  "Add FUN to HOOKS."
  (dolist (hook hooks)
    (add-hook hook fun)))

(defun add-all-to-list (usr-list vars)
  "USR-LIST get th VARS in."
  (dolist (list vars)
    (add-to-list usr-list list))
  )

;;; On-demand installation of packages-----------------------------------
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (if (boundp 'package-selected-packages)
            ;; Record this as a package the user installed explicitly
            (package-install package nil)
          (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
      (require-package package min-version no-refresh)
    (error
     (message "Couldn't install optional package `%s': %S" package err)
     nil)))

(defun install-pack-list (pack-list &optional is-maybe)
  "Install all the package in PACK-LIST.
If IS-MAYBE is t then maybe install these packages."
  (dolist (pack pack-list)
    (if (eq is-maybe t)
	(maybe-require-package pack)
      (require-package pack))))

;;; Basic=============================================================================
(setq-default make-backup-files nil ;; Don't make a backup file which end with "~"
              visible-bell t ;; Flash the frame to represent a bell
              auto-image-file-mode t
              initial-scratch-message nil
              inhibit-splash-screen t
	      column-number-mode nil
	      line-number-mode nil
              initial-major-mode 'text-mode
              frame-title-format "%b"
	      )

;; Set font
(when *is-a-win*
  (set-frame-font "Consolas 11")) ;; set when the OS is windows
(when *is-a-win*
  (set-fontset-font "fontset-default" 'gb18030 '("DengXian" . "unicode-bmp")))

;; ===================================================================================

;;; Packages===============================================================================
(setq ;; package--init-file-ensured t
      package-enable-at-startup nil
      package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
                         ("melpa" . "http://elpa.emacs-china.org/melpa/")
                         ("org"   . "http://elpa.emacs-china.org/org/"))
      )

(if (< emacs-major-version 27.0)
    (package-initialize)
  )
;;---------------------------------------------------------------------------------------

;;; Install some basic packages.
(let ((basic-edit-pack-list
       '(dash swiper youdao-dictionary deft
              window-jump avy avy-menu counsel use-package undo-tree multi-term
              cnfonts powerline atom-one-dark-theme diminish list-utils
              company company-quickhelp cl-lib helm helm-describe-modes yasnippet
	      treemacs popwin pdf-tools projectile hl-todo
              )))
  (install-pack-list basic-edit-pack-list))
(require 'popwin)

;;; Completion=============================================================================
(let ((completion-pack-list
       '(company company-quickhelp)))
  (install-pack-list completion-pack-list t))

;; Company---------------------------------------------------------------------------------
(use-package company
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq tab-always-indent 'complete)
  (add-hook 'prog-mode-hook 'company-mode)
  ;; (setq-default company-backends (delete 'company-semantic company-backends))
  (push '(company-semantic :with company-yasnippet) company-backends)
  :diminish company-mode
  )
(use-package company-quickhelp
  :commands company-quickhelp-manual-begin
  :bind (("C-c d" . 'company-quickhelp-manual-begin)))

;; Use tab as complete
(setq tab-always-indent 'complete)

;; YASnippte
(use-package yasnippet
  :init
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  (add-hook 'prog-mode-hook 'yas-reload-all)
  :diminish yas-minor-mode
)

;; Helm------------------------------------------------------------------------------------
(let ((helm-pack-list
       '(helm helm-swoop helm-ebdb helm-xref helm-gtags helm-ls-git
	      helm-dash helm-projectile)))
  (install-pack-list helm-pack-list))

(require 'helm-xref)
(defun init-helm-dev ()
  "Init helm."
  (use-package
    helm
    :init (setq xref-show-xrefs-function
		'helm-xref-show-xrefs)
    ;; (setq helm-autoresize-max-height 40)
    ;; (setq helm-autoresize-min-height 10)
    :bind (("M-x" . #'helm-M-x)
	   ("C-x C-f" . #'helm-find-files)
	   ("C-c f f" . #'helm-find-files)
	   ("C-c f r" . 'helm-recentf)
	   ("C-c s s" . 'helm-swoop-without-pre-input)
	   ("C-c s r" . 'helm-swoop)
	   ("C-x b" . 'helm-mini)
	   ("C-c h d" . 'helm-dash)
	   ("C-c h i" . 'helm-semantic-or-imenu)
	   ("C-c h f" . 'helm-flycheck))
    :config (helm-mode 1)
    :diminish helm-mode))
(add-hook 'after-init-hook 'init-helm-dev)
;; Projectile------------------------------------------------------------------------
(defun init-project-dev ()
  "Init."

  (use-package projectile
    :config
    (projectile-mode +1)
    (helm-projectile-on)
    :diminish projectile-mode
    :bind (("C-c p f" . 'helm-projectile-find-file)
           ("C-c p h" . 'helm-projectile)
	   ("C-c p p" . 'helm-projectile-switch-project))
    )
  )
 (add-hook 'after-init-hook 'init-project-dev)

;; Version Control=========================================================================
(let ((vc-pack-list
       '(evil-magit gitconfig-mode gitconfig-mode git-commit magit magit-gitflow orgit)))
  (install-pack-list vc-pack-list))
(defun init-vc-mode ()
  "Used to initilize version control mode."
  (use-package magit
    :bind (("C-c g s" . 'magit-status)
	   ("C-c g d" . 'magit-diff-range)
	   )
    )
  )
(add-hook 'after-init-hook 'init-vc-mode)
;;=========================================================================================

;;; Interface=========================================================================
;; (setq solarized-use-variable-pitch nil)
;; (setq solarized-use-less-bold t)
;; (setq solarized-use-more-italic t)
;; (setq solarized-emphasize-indicators nil)
;; (setq solarized-scale-org-headlines nil)
;; (setq solarized-high-contrast-mode-line t)
(setq-default custom-enabled-themes '(spacemacs-dark))
(add-hook 'after-init-hook 'reapply-themes)
;;------------------------------------------------------------------------------------

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))
;;(when (fboundp 'menu-bar-mode)
;;  (menu-bar-mode -1))
(recentf-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(electric-pair-mode 1)
(popwin-mode 1)

(add-to-list 'popwin:special-display-config
	     '("*.*[Hh]elm.**" :regexp t :position bottom)
	     )

;; Set the mode line.---------------------------------------------------------------
(setq-default mode-line-format ;; set mode line
	            (list
	             "%e" ;; print error message
	             mode-line-front-space
	             '(:eval evil-mode-line-tag) ;; Show evil mode.
	             mode-line-mule-info mode-line-client mode-line-modified
	             mode-line-remote
	             mode-line-frame-identification mode-line-buffer-identification ;; buffer files
	             "   "
	             "["
	             mode-line-position ;; position of this buffer
	             "/%I] "
	             "  "
	             mode-line-modes ;; Major mode and some important minor modes.
	             '(vc-mode vc-mode) ;; version control messages.
	             mode-line-misc-info mode-line-end-spaces)) ;; end of mode line

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;;-------------------------------------------------------------------------------------
(defun hide-minor-mode ()
  "This function hide HIDED-LIST from the modeline to save the space of modeline."
  (let ((dim-list
         ;; minor modes list followed will not show in the mode line.
         '(abbrev-mode org-autolist-mode hs-minor-mode auto-revert-mode
		       hs-minor-mode image-mode iimage-mode visual-line-mode
		       eldoc-mode undo-tree-mode))
        )
    (dolist (list dim-list)
      (diminish list)))
  )
(add-hook 'after-init-hook 'hide-minor-mode)
(add-hook 'find-file-hook (lambda () (hide-minor-mode)))

;;; hl TODO mode ---------------------------------------------------------------------
(use-package hl-todo
  :init
  (global-hl-todo-mode)
  :bind (("C-c t p" . 'hl-todo-previous)
	 ("C-c t n" . 'hl-todo-next)
	 ("C-c t o" . 'hl-todo-occur)
	 ("C-c t i" . 'hl-todo-insert-keyword)))
(add-to-list 'popwin:special-display-config
	     '("Occur" :regexp t :position bottom))

;;; Deft==============================================================================
(setq-default deft-extensions '("org")
	      deft-directory "~/notebook"
	      deft-recursive t
	      )

;;; youdao-dict-----------------------------------------------------------------------
(use-package youdao-dictionary
  :bind (("C-c o y" . 'youdao-dictionary-search-at-point+))
  )

;;; Org mode==========================================================================
(let ((org-mode-pack-list
       '(org evil-org helm-org-rifle org-pomodoro gnuplot htmlize org-present
	     org-projectile org-autolist org2ctex)))
  (install-pack-list org-mode-pack-list t))
;;====================================================================================

;;; Program===========================================================================
;; Flycheck
(require-package 'flycheck)
(add-hook 'prog-mode-hook 'flycheck-mode)
;; hs-minor-mode
(add-hook 'prog-mode-hook 'hs-minor-mode)
;; semantic-mode
(add-hook 'prog-mode-hook 'semantic-mode)

;; cscope mode-----------------------------------------------------------------------
(let ((cscope-pack-list
       '(helm-cscope xcscope)))
  (install-pack-list cscope-pack-list))

;TODO: set tag system.
;; (defun init-cscope-mode ()
;;   "Init."
;;   (use-package xcscope
;;     :init
;;     (progn
;;       ;; for python projects, we don't want xcscope to rebuild the database
;;       ;; because it uses sccope instead of pycscope
;;       )))

;; c/cpp mode------------------------------------------------------------------------
(let ((c-cpp-packages
       '(cc-mode clang-format company company-c-headers company-ycmd disaster
		 flycheck semantic ycmd
		 )))
  (dolist (c-cpp-pkg c-cpp-packages)
    (require-package c-cpp-pkg))
  )

(defun init-c-cpp-dev ()
  "Init."
  (use-package company-c-headers
    :config
    (add-to-list 'company-backends 'company-c-headers)
    )
  (when *is-a-win*
    (let ((usr-include-path
	        '(
	          "C:\\msys64\\mingw64\\x86_64-w64-mingw32\\include"
	          "C:\\msys64\\mingw64\\include"
	          "C:\\msys64\\mingw64\\include\\c++\\8.2.0"
	          )))
      (dolist (list usr-include-path)
        (add-to-list 'company-c-headers-path-system list))
      ))
  (when *is-a-linux*
    (let ((usr-include-path
	   '(
	     "/usr/include/c++/7"
	     )))
      (dolist (list usr-include-path)
	(add-to-list 'company-c-headers-path-system list))
      ))

  (use-package cc-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist
                   `("\\.h\\'" . ,'c++-mode)))
    :config
    (progn
      (require 'compile)
      ;; Disable electric indentation
      ;; (setq-default c-electric-flag nil)
      (setq c-basic-offset 4)
      (setq c-default-style '((java-mode . "java")
                              (other . "linux")))

      ;;(add-to-list 'c-cleanup-list 'space-before-funcall)
      (add-to-list 'c-cleanup-list 'compact-empty-funcall)
      (add-to-list 'c-cleanup-list 'comment-close-slash)
      ))

  (use-package clang-format
    )

  (use-package disaster
    )
  )
(add-hook 'c-mode-hook 'init-c-cpp-dev)
(add-hook 'c++-mode-hook 'init-c-cpp-dev)

;; emacs-lisp------------------------------------------------------------------------
(let ((elisp-pack-dev
      '(lispy)))
  (install-pack-list elisp-pack-dev))

(defun init-emacs-lisp-dev ()
  "Init."
  (use-package eldoc
    :config
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))
  )
;; python----------------------------------------------------------------------------
(let ((python-dev-pack
       '(elpy anaconda-mode cython-mode eldoc live-py-mode pip-requirements py-isort
	      pyenv-mode pytest pyvenv helm-pydoc)))
  (install-pack-list python-dev-pack t))

(defun init-python-dev ()
  "Init."
  (setq python-indent-offset 4)
  (setq python-shell-interpreter "python3")

  (use-package elpy
    :defer t
    :config
    (elpy-enable)
    )

  (use-package live-py-mode
    :defer t
    :commands live-py-mode
    :init)

  (use-package pyvenv
    :defer t
    :init
    )

  (use-package pytest
    :commands(pytest-one
	      pytest-pdb-one
	      pytest-all
	      pytest-pdb-all
	      pytest-module
	      pytest-pdb-module))

  (defun python-shell-send-buffer-switch ()
    "Send buffer content to shell and switch to it in insert mode."
    (interactive)
    (python-shell-send-buffer)
    (python-shell-switch-to-shell)
    (evil-insert-state))

  (defun python-execute-file (arg)
    "Execute a python script in a shell."
    (interactive "P")
    ;; set compile command to buffer-file-name
    ;; universal argument put compile buffer in comint mode
    (let ((universal-argument t)
          (compile-command (format "python %s" (file-name-nondirectory
                                                buffer-file-name))))
      (if arg
          (call-interactively 'compile)
        (compile compile-command t)
        (with-current-buffer (get-buffer "*compilation*")
          (inferior-python-mode)))))
  
  )
(add-hook 'python-mode-hook 'init-python-dev)

;;====================================================================================


;;; Keybinding========================================================================
(let ((key-pack-list
       '(evil evil-anzu evil-args evil-cleverparens evil-escape evil-exchange
        evil-goggles evil-iedit-state evil-indent-plus evil-lion evil-lisp-state
        evil-mc evil-nerd-commenter evil-matchit evil-numbers evil-surround
        evil-tutor
	evil-leader
        ;; (evil-unimpaired :location (recipe :fetcher local))
        evil-visual-mark-mode
        evil-visualstar
        ;; (hs-minor-mode :location built-in)
        ;; (linum-relative :toggle (version< emacs-version "26"))
        )))
  (dolist (pack key-pack-list)
    (require-package pack)))

;;; Evil
(require 'evil)
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")
(evil-leader/set-key
  ;;helm minibuffers-------------------------
  "f f" 'helm-find-files
  "f r" 'helm-recentf
  "SPC" #'helm-M-x
  "s s" 'helm-swoop-without-pre-input
  "s r" 'helm-swoop
  "b b" 'helm-mini
  "h i" 'helm-semantic-or-imenu
  "h f" 'helm-flycheck
  "h d" 'helm-dash
  ;; magit-----------------------------------
  "g s" 'magit-status
  "g d" 'magit-diff-range
  ;; projectile------------------------------
  "p f" 'helm-projectile-find-file
  "p h" 'helm-projectile
  "p p" 'helm-projectile-switch-project
  ;; windows jump----------------------------
  "w l" 'window-jump-right
  "w h" 'window-jump-left
  "w k" 'window-jump-up
  "w j" 'window-jump-down
  ;;youdao dict------------------------------
  "o y" 'youdao-dictionary-search-at-point+
  ;; todo mode ------------------------------
  "t p" 'hl-todo-previous
  "t n" 'hl-todo-next
  "t o" 'hl-todo-occur
  "t i" 'hl-todo-insert-keyword
  )
(evil-mode 1)

;;; Org mode global keys
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cc" 'org-capture)
;; (global-set-key "\C-cb" 'org-iswitchb)


;;====================================================================================



;;TODO--------------------------------------------------------------------------------
(require 'smex)
(smex-initialize)

(provide 'init)
;;; init.el ends here
