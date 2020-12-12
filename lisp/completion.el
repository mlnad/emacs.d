;;; init-ivy.el --- -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 John Doe
;;
;; Author: John Doe <http://github/L>
;; Maintainer: John Doe <john@doe.com>
;; Keywords:
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package ivy
  :hook (after-init . ivy-mode)
  :init
  (let ((standard-seaarch-fn #'ivy--regex-plus)
        (alt-search-fn #'ivy--regex-ignore-order))
    (setq ivy-re-builders-alist
          `((counsel-rg . ,standard-seaarch-fn)
            (swiper . ,standard-seaarch-fn)
            (swiper-isearch . ,standard-seaarch-fn)
            (t . ,alt-search-fn))
          ivy-more-chars-alist
          '((counsel-rg . 1)
            (counsel-search . 2)
            (t . 3))))
  :config
  (setq ivy-sort-max-size 7500)

  (require 'counsel nil t)

  (setq ivy-height 17
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
        ivy-use-virtual-buffers nil
        ivy-virtual-abbreviate 'full
        ivy-on-del-error-function #'ignore
        ivy-use-selectable-prompt t)
  )


(provide 'completion)
;;; completion.el ends here
