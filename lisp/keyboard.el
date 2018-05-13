;;; init-keyboard.el --- This is a file about keyboard setting

;;; Commentary:
;; Here gets some keyboards that could be used when the
;; Emacs started.

;;; Code:
(require-package 'swiper)
(require-package 'youdao-dictionary)
(require-package 'window-jump)
(require-package 'avy-menu)
(require-package 'avy)
(require-package 'counsel)

;;; helm
;; ==================================================================
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
;; ==================================================================

;;; Different edit mode
;;===================================================================
(global-set-key (kbd "C-c M-d") 'day-editor-mode)
(global-set-key (kbd "C-c M-n") 'night-editor-mode)
(global-set-key (kbd "C-c M-c") 'console-editor-mode)
;;===================================================================

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

;;; Swiper
;;==================================================================
(global-set-key "\C-s" 'swiper)
;;(global-set-key (kbd "C-<f6>") 'ivy-resume)
;;(global-set-key (kbd "M-x") 'counsel-M-x)
;;(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;(global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;;(global-set-key (kbd "<f1> l") 'counsel-find-library)
;;(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;;(global-set-key (kbd "C-c g") 'counsel-git)
;;(global-set-key (kbd "C-c j") 'counsel-git-grep)
;;(global-set-key (kbd "C-c k") 'counsel-ag)
;;(global-set-key (kbd "C-x l") 'counsel-locate)
;;(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
;;(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
;;========================================================================

;;; Flymake
;;========================================================================
(global-set-key (kbd "\C-cf") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "\C-c\C-f") 'flymake-goto-next-error)
(global-set-key (kbd "\C-c\C-b") 'flymake-goto-prev-error)
;;========================================================================

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



(provide 'keyboard)
;;; keyboard.el ends here
