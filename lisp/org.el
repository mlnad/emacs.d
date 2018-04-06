;;; init-org.el --- Org layer settings for emacs
;;
;; Author: mark Liu <liumiaogemini@gmail.com>
;;
;; Commemtary: I'd like to thanks Sylvain Benner and his
;; spacemacs. Most of this project's code are fork form
;; his project.
;;
;; License: See the root directory.

(require 'org-ac)
;;(org-ac/config-default);; org auto complete

;;;
;;=================================================================================

(defun org-default ()
  ;;
  " "
  (use-package org
    :init
    (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
    (add-hook 'org-mode-hook 'turn-on-font-lock)
    (add-hook 'message-mode-hook 'turn-on-orgstruct)
    (add-hook 'message-mode-hook 'turn-on-orgstruct++)
    (add-hook 'org-mode-hook (lambda () (org-autolist-mode)));;enable org autolist
  )
  )
;;==================================================================================

;;; Default
;;==================================================================================
;;==================================================================================


(provide 'org)
;;; org.el ends here
