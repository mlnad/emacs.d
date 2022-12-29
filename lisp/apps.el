;;; apps.el --- Emacs apps
;;
;;; Commentary:

;;; Code:
;;; Magit for git
(use-package magit
  :ensure t)

(use-package magit-gitflow
  :ensure t
  :hook (maigt-mode . turn-on-magit-gitflow))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

;;; Rime
(use-package rime
  :ensure t
  :if (not *sys/win32*)
  :custom
  (srime-show-candidate 'posframe)
  (default-input-method "rime")
  (rime-user-data-dir configs/rime-data-dir)
  :config
  (add-hook 'after-init-hook
            (lambda ()
              (unless (file-exists-p configs/rime-data-dir)
                (make-directory configs/rime-data-dir)
                (copy-file (concat user-emacs-directory "lisp/templates/user.yaml")
                           (concat configs/rime-data-dir "user.yaml"))))))

;;; Docker
(use-package docker
  :ensure t)

(use-package docker-tramp
  :ensure t)

(provide 'apps)
;;; apps.el ends here
