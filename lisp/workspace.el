;;; workspace.el --- Emacs workspace configs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Liu Miao;;
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(use-package persp-mode
  :ensure t
  :init
  (setq persp-add-buffer-on-after-change-major-mode 'free
        persp-auto-resume-time 1
        persp-is-ibc-as-f-supported nil
        persp-set-ido-hooks t
        persp-set-last-persp-for-new-frames nil
        persp-save-dir user/layouts-directory))


(provide 'workspace)
;;; workspace.el ends here
