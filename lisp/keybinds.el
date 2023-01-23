;;; keybindings.el --- other keybindings for Emacs.
;;; Commentary:
;;; evil + leader key + keymap + whichkey

;;; Code:
(defvar keybinds/leader-key "SPC"
  "The leader prefix key.")

(defvar keybinds/localleader-key "SPC m"
  "The localleader prefix key.")

;;; Whichkey
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-show-early-on-C-h t)
;;  (which-key-show-major-mode)
  :diminish which-key-mode)

;;; Keybinding
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (evil-set-leader '(normal motion visual) (kbd "SPC"))
  (evil-set-leader '(insert replace emacs) (kbd "M-m"))
  (evil-set-initial-state 'ivy-occur-grep-mode 'normal))

(use-package evil-anzu
    :ensure t
    :defer t
    :config
    (global-anzu-mode +1))

(use-package evil-org
  :if configs/enable-org
  :ensure evil-org
  :defer t
  :hook (org-mode . evil-org-mode)
  :config
  (setq evil-org-use-additional-insert t
        evil-org-key-theme `(textobjects
                             navigation
                             additional))
  :diminish evil-org-mode)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))

(defun keybinds/define-key (keymap key op &rest key-ops)
  "Define KEY-OPs at KEYMAP."
  (while key
    (define-key keymap (kbd key) op)
    (setq key (pop key-ops)
          op  (pop key-ops))))

(defmacro keybinds/set-leader-key (states keymap key op)
  "Bind KEY to OP at STATES and KEYMAP."
  `(evil-define-key ,states ,keymap (kbd ,(concat "<leader>" key)) ,op))

(defun keybinds/set-leader-key* (states keymap key op &rest key-ops)
  "Bind KEY-OPS lists at states and KEYMAP."
  (while key
    (evil-define-key states keymap (kbd (concat "<leader>" key)) op)
    (setq key (pop key-ops)
          op (pop key-ops))))
(put 'keybinds/set-leader-key* 'lisp-indent-function 'defun)

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
                         "n" 'org-roam-capture)))

;;; Define key
(keybinds/set-leader-key* nil 'global
  "<SPC>" 'execute-extended-command
  "w" keybinds/window-manage-map
  "f" keybinds/file-manage-map
  "b" keybinds/buffer-manage-map
  "p" project-prefix-map
  ;; Searching
  "s" completion/search-map
  "e" flymake-mode-map
  "g" keybinds/git-and-goto-map
  "n" keybinds/notes-manage-map)

(provide 'keybinds)
;;; keybindings.el ends here
