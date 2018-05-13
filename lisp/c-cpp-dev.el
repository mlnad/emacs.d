;;; init-c-c++-mode.el --- emacs c and c++ mode
;;; Commentary:

;;; Code:
;; package init
(defvar c-cpp-packages)
(defvar usr-include-path)
(defconst *is-a-linux* (eq system-type 'gnu/linux))
(defconst *is-a-win* (eq system-type 'windows-nt))

(setq c-cpp-packages
      '(cc-mode
        clang-format
        company
        company-c-headers
        company-ycmd
        disaster
        flycheck
        semantic
        ycmd
        ))

(dolist (c-cpp-pkg c-cpp-packages)
  (require-package c-cpp-pkg))

;;; End with package init and start code
(require 'semantic)
(require 'company)
(require 'company-c-headers)

(when *is-a-linux*
  (setq usr-include-path
	'("/usr/include/c++5.4.0")))
(when *is-a-win*
  (setq usr-include-path
	'(
	  "c:/msys64/mingw64/x86_64-w64-mingw32/include"
	  "c:/msys64/mingw64/x86_64-w64-mingw32/include/c++"
	)))


(defun c-cpp/cc-mode ()
  "Init cc mode for my Emacs."
  (use-package cc-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist
                   `("\\.h\\'" . ,'c++-mode)))
    :config
    (progn
      (require 'compile)
;;      (c-toggle-auto-newline 1)
      ;; Disable electric indentation
      (setq-default c-electric-flag nil)

      (setq c-basic-offset 4)
      
      (setq c-default-style '((java-mode . "java")
                              (other . "linux")))

;;      (add-to-list 'c-cleanup-list 'space-before-funcall)
      (add-to-list 'c-cleanup-list 'compact-empty-funcall)
      (add-to-list 'c-cleanup-list 'comment-close-slash)
      ))
  )

(defun c-cpp/clang-format ()
  "A clang-format package initilize."
  (use-package clang-format
    :init

    ))

(defun c-cpp/company ()
  "Company mode at c/cpp mode."

  (defun company-mode/more-than-prefix-gusser ()
    (c-cpp/load-clang-args)
    (company-clang-guess-prefix)
    )

  (add-to-list 'company-backends 'company-c-headers)
  (add-all-to-list 'company-c-headers-path-system usr-include-path)
  )

(defun c-cpp/disaster ()
  "Disaster package only can be used at *nix."
  (use-package disaster
    :defer t
    :commands (disaster)
    :init
    (progn
      (define-key c-mode-base-map (kbd "C-c d") 'disaster))
    ))

(defun c-cpp/flycheck ()
  "Check the syntax error in c/cpp mode."
  (dolist (mode '(c-mode cpp-mode))
    ))


(defun c-cpp/gdb ()
  "GNU Debuger initilize."
  (use-package gdb-mi
    :defer t
    :init
    (setq
     ;; use gdb-many-windows by default
     gdb-many-windows t
     gdb-show-main t
     ))
  )

(defun c-cpp/semantic()
  (add-to-hook 'semantic-mode '(c-mode-hook cpp-mode-hook))
  )

(defun c-cpp/ycmd ()
  "Initilize ycmd mode in c/cpp mode."
  (add-hook 'c++-mode-hook 'ycmd-mode)
  (add-hook 'c-mode-hook 'ycmd-mode)
  )

(defun active-c-cpp-mode ()
  "Active functions related to c-cpp mode."
  (c-cpp/cc-mode)
  (c-cpp/clang-format)
  (c-cpp/company)
  (c-cpp/disaster)
  (c-cpp/flycheck)
  (c-cpp/gdb)
  (c-cpp/semantic)
;;  (c-cpp/ycmd)
  )


(add-hook 'c-mode-hook 'active-c-cpp-mode)
(add-hook 'c++-mode-hook 'active-c-cpp-mode)
;;(add-hook 'c-mode-hook 'c-cpp/company)
;;(add-hook 'c-mode-hook 'c-cpp/cc-mode)
;;(add-hook 'c-mode-hook 'c-cpp/flycheck)
;;(add-hook 'c-mode-hook 'c-cpp/clang-format)
;;(add-hook 'c-mode-hook 'c-cpp/gdb)
;;(add-hook 'c-mode-hook 'c-cpp/semantic)
;;(add-hook 'c-mode-hook 'c-cpp/ycmd)


(provide 'c-cpp-dev)
;;; c-cpp-dev.el ends here
