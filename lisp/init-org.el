;; use-org.el --- the org mode file
;;; Commentary:

;;; Code:

(defvar org-ctexart-class
  '("ctexart"
    "\\documentclass[11pt]{ctexart}"
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(defvar org-complex-ctexbook-class
  '("cctexbook"
    "\\documentclass[11pt]{ctexbook}"
    ("\\part{%s}" . "\\part*{%s}")
    ("\\chapter{%s}" . "\\chapter*{%s}")
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(defvar org-simple-ctexbook-class
  '("sctexbook"
    "\\documentclass[11pt]{ctexbook}"
    ("\\chapter{%s}" . "\\chapter*{%s}")
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(defvar org-user-latex-class
  (list org-ctexart-class org-complex-ctexbook-class org-simple-ctexbook-class))

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
    (add-to-list 'org-modules 'org-habit))

  (evil-define-key 'normal org-mode-map (kbd "RET") 'org-open-at-point)

  (user/set-leader-key* 'normal org-mode-map
    ;; basic
    "oc" 'org-capture
    "Cc" 'org-clock-cancel
    "Cd" 'org-clock-display
    "Ci" 'org-clock-in

    "dd" 'org-deadline
    "ds" 'org-schedule
    "dt" 'org-time-stamp
    "dT" 'org-time-stamp-inactive
    "ee" 'org-export-dispatch

    ;; roam
    "mia" 'org-id-get-create

    ;; Subtree
    "msa" 'org-toggle-archive-tag
    "msA" 'org-archive-subtree-default
    "msd" 'org-cut-subtree
    "msj" 'org-move-subtree-down
    "msk" 'org-move-subtree-up
    "msn" 'org-narrow-to-subtree
    "msw" 'widen
    "msr" 'org-refile
    "mss" 'org-sparse-tree
    
    ;; Table
    "mta" 'org-table-align
    "mtb" 'org-table-blank-field
    "mtc" 'org-table-convert
    "mtdc" 'org-table-delete-column
    "mtdr" 'org-table-kill-row
    "mte" 'org-table-eval-formula
    "mtE" 'org-table-export
    "mtf" 'org-table-field-info
    "mth" 'org-table-previous-field
    "mtH" 'org-table-move-column-left
    "mtic" 'org-table-insert-column
    "mtih" 'org-table-insert-hline
    "mtiH" 'org-table-hline-and-move
    "mtir" 'org-table-insert-row
    "mtI" 'org-table-import
    "mtj" 'org-table-next-row
    "mtJ" 'org-table-move-row-down
    "mtK" 'org-table-move-row-up
    "mtl" 'org-table-next-field
    "mtL" 'org-table-move-column-right
    "mtn" 'org-table-create
    "mtN" 'org-table-create-with-table.el
    "mtr" 'org-table-recalculate
    "mtR" 'org-table-recalculate-buffer-tables
    "mts" 'org-table-sort-lines
    "mttf" 'org-table-toggle-formula-debugger
    "mtto" 'org-table-toggle-coordinate-overlays
    "tmw" 'org-table-wrap-region

    ;; Source blocks
    "mbp"     'org-babel-previous-src-block
    "mbn"     'org-babel-next-src-block
    "mbe"     'org-babel-execute-maybe
    "mbo"     'org-babel-open-src-block-result
    "mbv"     'org-babel-expand-src-block
    "mbu"     'org-babel-goto-src-block-head
    "mbg"     'org-babel-goto-named-src-block
    "mbr"     'org-babel-goto-named-result
    "mbb"     'org-babel-execute-buffer
    "mbs"     'org-babel-execute-subtree
    "mbd"     'org-babel-demarcate-block
    "mbt"     'org-babel-tangle
    "mbf"     'org-babel-tangle-file
    "mbc"     'org-babel-check-src-block
    "mbj"     'org-babel-insert-header-arg
    "mbl"     'org-babel-load-in-session
    "mbi"     'org-babel-lob-ingest
    "mbI"     'org-babel-view-src-block-info
    "mbz"     'org-babel-switch-to-session
    "mbZ"     'org-babel-switch-to-session-with-code
    "mba"     'org-babel-sha1-hash
    "mbx"     'org-babel-do-key-sequence-in-edit-buffer
    ))

(use-package ob
  :init
  (add-hook 'org-mode-hook
            (lambda ()
               (org-babel-do-load-languages 'org-babel-load-languages
                                            org-babel-load-languages))))

(use-package evil-org
  :if user/enable-org
  :ensure evil-org
  :defer t
  :hook (org-mode . evil-org-mode)
  :config
  (setq evil-org-use-additional-insert t
        evil-org-key-theme `(textobjects
                             navigation
                             additional))
  :diminish evil-org-mode)

(use-package org-agenda
  :init
  (setq org-agenda-restore-windows-after-quit t))

(use-package org-roam
  :if (and user/enable-org-roam user/enable-org)
  :ensure t
  :hook (after-init . org-roam-setup)
  :custom
  (org-roam-directory user/org-roam-dir)
  :commands (org-roam-buffer-toggle-display
             org-roam-tag-add
             org-roam-tag-delete)
  :config
  (add-to-list 'user/roam-templates roam/default-capture)
  (setq org-roam-capture-templates user/roam-templates)

  (user/set-leader-key* nil org-mode-map
    "mrg" 'org-roam-graph
    "mri" 'org-roam-node-insert
    "mrf" 'org-roam-node-find
    "mrta" 'org-roam-tag-add
    "mrtd" 'org-roam-tag-remove)
  (user/set-global-leader-key*
    "nrf" 'org-roam-node-find))

(use-package org-journal
  :if user/enable-org
  :ensure t
  :config
  (setq org-journal-file-type user/org-journal-type)
  (setq org-journal-dir (expand-file-name "journal" user/notes-dir)
        org-journal-file-format "%Y-%m-%d.org"))

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
