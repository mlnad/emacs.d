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

(use-package magit-todos
  :ensure t)

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

;;; EShell
(with-eval-after-load 'eshell
  (setq eshell-banner-message '(format "%s %s\n"
                                (propertize (format " %s " (string-trim (buffer-name)))
                                            'face 'mode-line-highlight)
                                (propertize (current-time-string)
                                            'face 'font-lock-keyword-face))
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-kill-processes-on-exit t
        eshell-hist-ignoredups t
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t))

(provide 'apps)
;;; apps.el ends here
