;;; keybindings.el --- other keybindings for Emacs.
;;; Commentary:

;;; Code:
(defvar emacs-default-map (make-sparse-keymap)
  "Base keymap for all Emacs leader key commands.")

;;; Whichkey
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-show-early-on-C-h t)
  (which-key-show-major-mode)
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

(defun user/set-leader-key* (states keymap key op &rest key-ops)
  "Bind KEY-OPS lists at states and KEYMAP."
  (while key
    (evil-define-key states keymap (kbd (concat "<leader>" key)) op)
    (setq key (pop key-ops)
          op (pop key-ops))))
(put 'user/set-leader-key* 'lisp-indent-function 'defun)

(defmacro user/set-global-leader-key (key op)
  "Bind KEY to OP globally for all evil states."
  `(user/set-leader-key nil 'global ,key ,op))

(defun user/set-global-leader-key* (key op &rest key-ops)
  "Bind KEY to OP."
  (while key
    (evil-define-key nil 'global (kbd (concat "<leader>" key)) op)
    (setq key (pop key-ops)
          op (pop key-ops))))
(put 'user/set-global-leader-key* 'lisp-indent-function 'defun)

;;; General - for keybindings
(use-package general
  :ensure t
  :init
  (defalias 'define-key! #'general-def)
  (defalias 'undefine-key! #'general-unbind))

;;; Define key
(user/set-global-leader-key*
 ;; windows jump
  "wh" 'evil-window-left
  "wl" 'evil-window-right
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  ;; window split
  "wv" 'evil-window-vsplit
  "w-" 'evil-window-split
  "wd" 'evil-window-delete
  ;;
  "<SPC>" 'execute-extended-command
  ;; Files
  "ff" 'find-file
  "fs" 'save-buffer
  "fS" 'evil-write-all
  ;; Buffers
  "bd" 'kill-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bx" 'kill-buffer-and-window)

(provide 'keybindings)
;;; keybindings.el ends here
