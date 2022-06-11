;; use-org.el --- the org mode file
;;; Commentary:

;;; Code:
(defvar roam/default-capture
  '("d" "default" plain "%?"
   	:if-new (file+head "${slug}.org"
                       "#+title: ${title}\n\n#+startup: indent\n")
    :unnarrowed t))

(use-package org
  :if user/enable-org
  :ensure org
  :commands (orgtbl-mode)
  :init
  (defvar org-face-font nil)
  :config
  (require 'org-tempo)

  (setq org-clock-persist-file (concat user/cache-directory
                                       "org-clock-save.el")
        org-id-locations-file (concat user/cache-directory
                                      "org-id-locations")
        org-publish-timestamp-directory (concat user/cache-directory
                                                "org-timestamps/")
        org-log-done 'time
        org-startup-with-inline-images t
        org-latex-prefer-user-labels t
        org-image-actual-width nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-imenu-depth 8)
  (setq org-src-block-faces
        `(("emacs-lisp" ,user/org-src-block-face)
          ("c" ,user/org-src-block-face)
          ("c++" ,user/org-src-block-face)
          ("shell" ,user/org-src-block-face)
          ("rust" ,user/org-src-block-face)
          ("python" ,user/org-src-block-face)
          ("haskell" ,user/org-src-block-face)))

  (with-eval-after-load 'org-agenda
    (add-to-list 'org-modules 'org-habit)))

(use-package ob
  :init
  (add-hook 'org-mode-hook
            (lambda ()
               (org-babel-do-load-languages 'org-babel-load-languages
                                            org-babel-load-languages))))

(use-package org-agenda
  :init
  (setq org-agenda-restore-windows-after-quit t))

(use-package org-roam
  :if (and user/enable-org-roam user/enable-org)
  :ensure org-roam
  :hook (after-init . org-roam-setup)
  :custom
  (org-roam-directory user/org-roam-dir)
  :commands (org-roam-buffer-toggle-display
             org-roam-tag-add
             org-roam-tag-delete)
  :config
  (add-to-list 'user/roam-templates roam/default-capture)
  (setq org-roam-capture-templates user/roam-templates))

(use-package gnuplot
  :ensure gnuplot
  :defer t)

(use-package valign
  :ensure t
  :config
  (add-hook 'org-mode-hook #'valign-mode)
  :diminish valign-mode)

(provide 'init-org)
;;; init-org.el ends here
