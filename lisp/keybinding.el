;;; keybinding.el --- This is a file about keyboard setting

;;; Commentary:
;; Here gets some keyboards that could be used when the
;; Emacs started.

;;; code:
(require 'user-fun)
(require 'basic)

(defun install-key-pack ()
  " "
  (let ((key-pack-list
        '(evil)))
    (dolist (pack key-pack-list)
      (require-package pack))
    ))
(install-key-pack)

;;; Evil
;;=====================================================================
(require 'evil)
(evil-mode 1)
;;=====================================================================

;;; helm
;; ==================================================================
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
;; ==================================================================

;;; Org mode global keys
;;==================================================================
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
;;==================================================================

;;; Youdao search
;;==================================================================
(global-set-key (kbd "M-s s") 'youdao-dictionary-search-from-input)
(global-set-key (kbd "M-s t") 'youdao-dictionary-search-at-point)
;;==================================================================

;;; Search
;;==================================================================
(global-set-key "\C-s" 'helm-swoop-without-pre-input)

;;; Recentf mode
;;------------------------------------------------------------------------
(global-set-key (kbd "\C-cr") 'recentf-open-files)
;;------------------------------------------------------------------------

;;; disaster
;; ===================================================================================
(global-set-key (kbd "\C-cd") 'disaster)
;; ===================================================================================

;;; windows jump
;;------------------------------------------------------------------------------------
(global-set-key (kbd "M-s r") 'window-jump-right)
(global-set-key (kbd "M-s l") 'window-jump-left)
(global-set-key (kbd "M-s u") 'window-jump-up)
(global-set-key (kbd "M-s d") 'window-jump-down)
;;------------------------------------------------------------------------------------



(provide 'keybinding)
;;; keyboard.el ends here
