;; use-org.el --- the org mode file
;;; Commentary:

;;; Code:
;;;
;;; Org
(defvar org/default-roam-capture
  '("d" "default" plain "%?"
   	:if-new (file+head "${slug}.org"
                       "#+title: ${title}\n\n#+startup: indent\n")
    :unnarrowed t))

(defvar org/roam-templates nil)

(use-package org
  :init
  (defvar org-face-font nil)
  :config
  (require 'org-tempo)

  (setq org-id-locations-file (concat configs/cache-directory
                                      "org-id-locations")
        org-publish-timestamp-directory (concat configs/cache-directory
                                                "org-timestamps/")
        org-log-done 'time
        org-startup-with-inline-images t
        org-latex-prefer-user-labels t
        org-image-actual-width nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-imenu-depth 8
        org-agenda-restore-windows-after-quit t)

  (with-eval-after-load 'org-agenda
    (add-to-list 'org-modules 'org-habit)))

(use-package org-roam
  :ensure org-roam
  :hook (after-init . org-roam-db-autosync-enable)
  :custom
  (org-roam-directory configs/org-roam-dir)
  :commands (org-roam-buffer-toggle-display
             org-roam-tag-add
             org-roam-tag-delete)
  :config

  (cl-defmethod org-roam-node-org-hierarchy ((node org-roam-node))
    "Return hierarchy for NODE, constructed of its file title, OLP and direct title.
If some elements are missing, the will be stripped out."
    (let* ((title (org-roam-node-title node))
          (olp (org-roam-node-olp node))
          (level (org-roam-node-level node))
          (filetitle (or (if (= level 0)
                             title
                           (org-roam-node-file-title node))))
          (separator (propertize ":" 'face 'shadow)))
      (cl-case level
        (0 filetitle)
        (1 (concat (propertize filetitle 'face '(shadow italic))
                   separator title))
        (t (concat (propertize filetitle 'face '(shadow italic))
                   separator (propertize (string-join olp separator) 'face '(shadow italic))
                   separator title)))))

  (add-to-list 'org/roam-templates org/default-roam-capture)
  (setq org-roam-capture-templates org/roam-templates
        org-roam-node-display-template "${org-hierarchy}"))

(use-package gnuplot
  :ensure gnuplot
  :defer t)

(use-package valign
  :ensure t
  :config
  (add-hook 'org-mode-hook #'valign-mode)
  :diminish valign-mode)

(provide 'writting)
;;; init-org.el ends here
