;;; init-org.el --- Org layer settings for emacs
;;
;; Author: mark Liu <liumiaogemini@gmail.com>
;;
;; Commemtary: I'd like to thanks Sylvain Benner and his
;; spacemacs. Most of this project's code are fork form
;; his project.
;;
;; License: See the root directory.

;;; Code:
(require 'usr-fun)
;;=================================================================================
(defun org-default ()
  "Default org settings."
  (use-package org
    :init
    (require-package 'org)
    (require-package 'org-ac)
    (require-package 'org2ctex)
    (require-package 'org-autolist)
    (require-package 'org-plus-contrib)

    (require 'org2ctex)
    (org2ctex-toggle t)
    
    (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
    (add-hook 'org-mode-hook 'turn-on-font-lock)
    (add-hook 'message-mode-hook 'turn-on-orgstruct)
    (add-hook 'message-mode-hook 'turn-on-orgstruct++)
    (add-hook 'org-mode-hook (lambda () (org-autolist-mode)));;enable org autolist
    ;;  (add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
    (add-hook 'org-mode-hook 'iimage-mode)
    (add-hook 'org-mode-hook 'visual-line-mode)
    )
  (require 'org-ac)
  )
;;==================================================================================

;;; Keybindings
;;==================================================================================
;;==================================================================================


(provide 'org)
;;; org.el ends here
