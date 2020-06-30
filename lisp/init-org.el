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
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
  )

(defvar org-complex-ctexbook-class
  '("cctexbook"
    "\\documentclass[11pt]{ctexbook}"
    ("\\part{%s}" . "\\part*{%s}")
    ("\\chapter{%s}" . "\\chapter*{%s}")
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
  )

(defvar org-simple-ctexbook-class
  '("sctexbook"
    "\\documentclass[11pt]{ctexbook}"
    ("\\chapter{%s}" . "\\chapter*{%s}")
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
  )

(defvar org-user-latex-class
  (list org-ctexart-class org-complex-ctexbook-class org-simple-ctexbook-class))

(use-package org
  :ensure org
  :defer t
  :commands (orgtbl-mode)
  :init
  (progn
    (require 'org))
  )

(use-package evil-org
  :ensure evil-org
  :defer t
  :hook org-mode
  :init
  ;;  (progn
  ;;    (add-hook 'org-mode-hook `(lambda () ((evil-org-mode)))))
  )

(use-package gnuplot
  :ensure gnuplot
  :defer t)

(defun use-org ()
  "Manage useage of 'org-mode'."
  (interactive)
  (message "Welcome to the world of writting"))

(provide 'init-org)
;;; init-org.el ends here
