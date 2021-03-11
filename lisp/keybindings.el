;;; keybindings.el --- other keybindings for Emacs.
;;; Commentary:

;;; Code:
(defvar emacs-default-map (make-sparse-keymap)
  "Base keymap for all Emacs leader key commands.")

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
  (evil-set-leader '(insert replace emacs) (kbd "C-c")))

(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-mode-list nil))

(defmacro user/set-leader-key (states keymap key op)
  "Bind KEY to OP at STATES and KEYMAP."
  `(evil-define-key ,states ,keymap (kbd ,(concat "<leader>" key)) ,op))

(defmacro user/set-global-leader-key (key op)
  "Bind KEY to OP globally for all evil states."
  `(user/set-leader-key nil 'global ,key ,op))

;;; General - for keybindings
(use-package general
  :ensure t
  :init
  (defalias 'define-key! #'general-def)
  (defalias 'undefine-key! #'general-unbind))

;;; Define key
(evil-define-key* nil 'global
  ;; windows jump
  (kbd "<leader>wh") 'evil-window-left
  (kbd "<leader>wl") 'evil-window-right
  (kbd "<leader>wj") 'evil-window-down
  (kbd "<leader>wk") 'evil-window-up
  ;; window split
  (kbd "<leader>wv") 'evil-window-vsplit
  (kbd "<leader>w-") 'evil-window-split
  (kbd "<leader>wd") 'evil-window-delete
  ;;
  (kbd "<leader><SPC>") 'execute-extended-command
  ;; Files
  (kbd "<leader>ff") 'find-file
  (kbd "<leader>fs") 'save-buffer
  (kbd "<leader>fS") 'evil-write-all
  ;; Buffers
  (kbd "<leader>bd") 'kill-buffer
  (kbd "<leader>bn") 'next-buffer
  (kbd "<leader>bp") 'previous-buffer
  (kbd "<leader>bx") 'kill-buffer-and-window)

(provide 'keybindings)
;;; keybindings.el ends here
