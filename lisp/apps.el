;;; apps.el --- Emacs apps
;;
;;; Commentary:

;;; Code:
;;; Deft
(use-package deft
  :ensure t
  :config
  (setq-default deft-extensions configs/notes-extensions
                deft-directory configs/notes-dir
                deft-recursive t))

;;; youdao-dict
(use-package youdao-dictionary
  :ensure t
  :config)

;;; Magit for git
(use-package magit
  :ensure t
  :init
  (use-package forge
    :ensure t)

  (use-package magit-gitflow
    :ensure t
    :hook (maigt-mode . turn-on-magit-gitflow))

  (use-package git-gutter
    :ensure t
    :custom
    (git-gutter:update-interval 2)
    :config
    (global-git-gutter-mode +1)))

;;; Rime
(use-package rime
  :ensure t
  :config
  (setq rime-show-candidate 'posframe
        default-input-method "rime"
        rime-user-data-dir configs/rime-data-dir)
  (add-hook 'after-init-hook
            (lambda ()
              (or (file-exists-p configs/rime-data-dir)
                  (progn
                    (make-directory configs/rime-data-dir)
                    (copy-file (concat user-emacs-directory "lisp/templates/user.yaml")
                               (concat configs/rime-data-dir "user.yaml")))))))

;;; Docker
(use-package docker
  :ensure t)

(use-package docker-tramp
  :ensure t)

(provide 'apps)
;;; apps.el ends here
