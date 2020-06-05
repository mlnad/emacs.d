
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
  "s s" 'swiper
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
  ;; windows options-------------------------
  "w l" 'window-jump-right
  "w h" 'window-jump-left
  "w k" 'window-jump-up
  "w j" 'window-jump-down
  "w 2" 'split-window-right
  "w 0" 'delete-window
  "w 1" 'delete-other-windows
  ;;youdao dict------------------------------
  "o y" 'youdao-dictionary-search-at-point+
  )
(evil-mode 1)

(provide 'keybindings)
;;; keybindings.el ends here
