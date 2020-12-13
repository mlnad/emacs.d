
;;; Code:
;;; Evil
(require 'evil)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")
(evil-leader/set-key
  ;;swiper minibuffers-------------------------
  "f f" 'counsel-find-file
  "f r" 'counsel-recentf
  "f s" 'save-buffer
  "SPC" 'counsel-M-x
  ;; "SPC" 'execute-extended-command
  "s s" 'swiper
  "s d" 'user/search-dir
  "s p" 'user/search-project
  "h i" 'counsel-imenu
  ;; buffer
  "b d" 'kill-current-buffer
  "b k" 'kill-buffer
  "b b" 'counsel-switch-buffer
  ;; magit-----------------------------------
  "g s" 'magit-status
  "g d" 'magit-diff-range
  "g p" 'magit-push-current
  "g P" 'magit-pull-branch
  "g c" 'magit-commit
  ;; projectile------------------------------
  "p f" 'counsel-projectile-find-file
  "p h" 'counsel-projectile
  "p p" 'counsel-projectile-switch-project
  "p b" 'counsel-projectile-switch-to-buffer
  "p k" 'projectile-kill-buffers
  ;; windows options-------------------------
  "w l" 'evil-window-right
  "w h" 'evil-window-left
  "w k" 'evil-window-up
  "w j" 'evil-window-down
  "w 2" 'split-window-right
  "w -" 'split-window-vertically
  "w 0" 'delete-window
  "w 1" 'delete-other-windows
  ;; youdao dict------------------------------
  "o y" 'youdao-dictionary-search-at-point+
  ;; flycheck mode
  "e n" 'flycheck-next-error
  "e p" 'flycheck-previous-error
  "e l" 'flycheck-list-errors
  ;; jump mode
  "j i" 'imenu
  ;; global
  "o c" 'open-userconfig-file
  "o i" 'open-init-file
  )
(evil-mode 1)

(provide 'keybindings)
;;; keybindings.el ends here
