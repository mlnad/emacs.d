;;; keybindings.el --- other keybindings for Emacs.
;;; Commentary:

;;; Code:
(defvar emacs-default-map (make-sparse-keymap)
  "Base keymap for all Emacs leader key commands.")

;;; Define key
(evil-define-key* nil 'global
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
