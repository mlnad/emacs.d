;;; keybindings.el --- other keybindings for Emacs.
;;; Commentary:
;;; evil + leader key + keymap + whichkey

;;; Code:
(defvar keybinds/leader-key "<SPC>"
  "The leader prefix key.")

(defvar keybinds/localleader-key "M-m"
  "The localleader prefix key.")

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
  (setq which-key-show-early-on-C-h t))

;;; Keybinding
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (evil-set-leader '(normal motion visual) (kbd keybinds/leader-key))
  (evil-set-leader '(insert replace emacs) (kbd keybinds/localleader-key)))

(use-package evil-anzu
    :ensure t
    :defer t
    :config
    (global-anzu-mode +1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))

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

(defvar keybinds/window-manage-map
  (let ((map (make-sparse-keymap)))
    (keybinds/define-key map
                         ;; Jump
                         "h" 'evil-window-left
                         "l" 'evil-window-right
                         "j" 'evil-window-down
                         "k" 'evil-window-up
                         ;; Split
                         "v" 'evil-window-vsplit
                         "-" 'evil-window-split
                         "d" 'evil-window-delete
                         "m" 'delete-other-windows
                         "0" 'delete-window)
    map)
  "Emacs window management commands.")

(defvar keybinds/file-manage-map
  (let ((map (make-sparse-keymap)))
    (keybinds/define-key map
                         "f" 'find-file
                         "s" 'save-buffer
                         "S" 'write-file
                         "r" 'recentf-open-files)
    map)
  "Emacs file management commands.")

(defvar keybinds/buffer-manage-map
  (let ((map (make-sparse-keymap)))
    (keybinds/define-key map
                         "b" 'consult-buffer
                         "d" 'kill-current-buffer
                         "]" 'next-buffer
                         "[" 'previous-buffer
                         "x" 'kill-buffer-and-window)
    map)
  "Emacs buffer management commands.")

(defvar keybinds/git-and-goto-map
  (let ((map (make-sparse-keymap)))
    (keybinds/define-key map
                         "g" 'magit-status
                         "l" 'goto-line)
    map)
  "Version control and goto line")

(defvar keybinds/notes-manage-map
  (let ((map (make-sparse-keymap)))
    (keybinds/define-key map
                         "a" 'org-agenda
                         "c" 'org-capture
                         "r" 'org-roam-node-find
                         "n" 'org-roam-capture)
    map))

;;; Define key
(keybinds/set-leader nil 'global
                     "<SPC>" '("Exec" . execute-extended-command)
                     "w" (cons "Window" keybinds/window-manage-map)
                     "f" (cons "File" keybinds/file-manage-map)
                     "b" (cons "Buffer" keybinds/buffer-manage-map)
                     "p" (cons "Projects" project-prefix-map)
                     ;; Searching
                     "s" (cons "Searching" completion/search-map)
                     "g" (cons "Git/Goto" keybinds/git-and-goto-map)
                     "n" (cons "Notes" keybinds/notes-manage-map))

(provide 'keybinds)
;;; keybindings.el ends here
