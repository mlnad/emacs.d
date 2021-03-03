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

(use-package org-mode
  :ensure org
  :defer t
  :commands (orgtbl-mode)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   `((emacs-lisp . t)
     (gnuplot . t)
     (latex . t)
     (python . t)))
  )

(use-package evil-org
  :ensure evil-org
  :defer t
  :hook (org-mode . evil-org-mode)
  :diminish evil-org-mode
  )

(use-package org-roam
  :ensure t
  :hook (after-init . org-roam-mode)
  :custom
  (org-roam-directory user/org-roam-dir)
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(use-package gnuplot
  :ensure gnuplot
  :defer t)

(use-package valign
  :ensure t
  :config
  (add-hook 'org-mode-hook #'valign-mode)
  :diminish valign-mode)

(with-eval-after-load 'org
  (add-hook 'org-mode-hook
            '(lambda ()
               (make-face 'width-font-face)
               (set-face-attribute 'width-font-face nil :font "Sarasa Mono SC 12") ;; 13, 14, 16等会出现不等宽
               (setq buffer-face-mode-face 'width-font-face)
               (buffer-face-mode))))

(provide 'init-org)
;;; init-org.el ends here
