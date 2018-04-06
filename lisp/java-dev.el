;;; init-java.el --- init file about Emacs java development
;;; Commentary:

;;; Code:

;;; JDEE
;; ===================================================================================
(setq
 ;;jdee-jdk-registry
 ;;debug-on-error t
 )

(custom-set-variables
 '(jdee-server-dir "~/.emacs.d/jdee-server"))
;; ===================================================================================

;;; Java imports
;;------------------------------------------------------------------------------------
(require 'java-imports)

(setq java-imports-find-block-function 'java-imports-find-place-sorted-block)

(add-hook 'java-mode-hook 'java-imports-scan-file)
;;------------------------------------------------------------------------------------

(provide 'java-dev)
;;; java-dev.el ends here
