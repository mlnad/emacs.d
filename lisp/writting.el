;; use-org.el --- the org mode file
;;; Commentary:
;;; Org Mode + Markdown
;;; Code:

;;; Org
(defvar org/default-roam-capture
  '("d" "default" plain "%?"
   	:if-new (file+head "${slug}.org"
                       "#+title: ${title}\n\n#+startup: indent\n")
    :unnarrowed t))

(defvar org/roam-templates nil)

(defvar org/roam-dailies-map (make-sparse-keymap))

(use-package org
  :preface
  ;; org files
  (setq-default org-directory configs/notes-dir)
  (setq org-id-locations-file (expand-file-name ".orgids" org-directory)
        org-preview-latex-image-directory (concat configs/cache-directory "org/latex/")
        org-list-allow-alphabetical t)
  ;; org faces
  (setq org-indirect-buffer-display 'current-window
        org-log-done 'time
        org-enforce-todo-dependencies t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-hide-leading-stars t
        org-image-actual-width nil
        org-startup-with-inline-images t
        org-imenu-depth 6
        org-startup-indented t
        org-tags-column 0
        org-startup-folded nil)
  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)")))
  ;; org agenda
  (setq-default org-agenda-files (list (concat "agendas/" configs/notes-dir))
                org-agenda-skip-unavailable-files t
                org-agenda-span 20
                org-agenda-start-on-weekday nil
                org-agenda-start-day "-5d"
                org-agenda-inhibit-startup t)
  ;; attachements
  (setq org-attach-store-link-p t
        org-attach-use-inheritance t))

(use-package org-roam
  :ensure org-roam
  :hook (after-init . org-roam-db-autosync-enable)
  :custom
  (org-roam-directory configs/org-roam-dir)
  :commands (org-roam-buffer-toggle-display
             org-roam-tag-add
             org-roam-tag-delete)
  :init
  (require 'org-roam-dailies)
  (setq org-roam-db-location configs/org-roam-db-location)
  :config
  (add-to-list 'org/roam-templates org/default-roam-capture)
  (setq org-roam-capture-templates org/roam-templates
        org-roam-node-display-template "${org-hierarchy}"))

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

;;;###autoload
(defun org/find-in-notes ()
  "Find file in notes directory."
  (interactive)
  (completion/find-file configs/notes-dir))

(use-package gnuplot
  :ensure gnuplot
  :defer t)

(use-package valign
  :ensure t
  :config
  (add-hook 'org-mode-hook #'valign-mode))


;;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("/README\\(?:\\.md\\)?\\'" . gfm-mode)
  :init
  (setq markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-asymmetric-header t
        markdown-make-gfm-checkboxes-buttons t
        markdown-fontify-whole-heading-line t))

;;; Latex
(setq TeX-parse-self t
      TeX-auto-save t
      TeX-auto-local ".auctex-auto"
      TeX-style-local ".auctex-style"
      TeX-source-correlate-mode t
      TeX-source-correlate-method 'synctex
      TeX-save-query nil)

(use-package auctex-latexmk
  :ensure t
  :after latex
  :init
  (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  :config
  (auctex-latexmk-setup))

(use-package tex
  :ensure auctex)

(with-eval-after-load 'bibtex
  (setq bibtex-align-at-equal-sign t
        bibtex-text-indentation 20))

(provide 'writting)
;;; init-org.el ends here
