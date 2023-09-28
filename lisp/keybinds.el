;;; keybindings.el --- other keybindings for Emacs.
;;; Commentary:
;;; evil + leader key + keymap + whichkey

;;; Code:
(require 'core)

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

;;; Whichkey
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
  (evil-set-leader '(insert replace emacs) (kbd keybinds/localleader-key))
  (keybinds/define-key evil-window-map
                       "m" #'delete-other-windows
                       "u" #'winner-undo
                       "d" #'evil-window-delete
                       "T" #'tear-off-window))

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

;;; Define key
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
                     "o" (cons "open" keybinds/open-map)
                     "s" (cons "searching" keybinds/search-map)
                     "w" (cons "window" evil-window-map))

(provide 'keybinds)
;;; keybindings.el ends here
