;;; keybindings.el --- other keybindings for Emacs.
;;; Commentary:

;;; Code:
(defvar keybinds/leader-map (make-sparse-keymap)
  "Base keymap for all Emacs leader key commands.")

(defvar keybinds/leader-key "SPC"
  "The leader prefix key.")

(defvar keybinds/localleader-key "SPC m"
  "The localleader prefix key.")

;;; Whichkey
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-show-early-on-C-h t)
;;  (which-key-show-major-mode)
  :diminish which-key-mode)

;;; Keybinding
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (evil-set-leader '(normal motion visual) (kbd "SPC"))
  (evil-set-leader '(insert replace emacs) (kbd "M-m"))
  (evil-set-initial-state 'ivy-occur-grep-mode 'normal))

(use-package evil-anzu
    :ensure t
    :defer t
    :config
    (global-anzu-mode +1))

(use-package evil-org
  :if configs/enable-org
  :ensure evil-org
  :defer t
  :hook (org-mode . evil-org-mode)
  :config
  (setq evil-org-use-additional-insert t
        evil-org-key-theme `(textobjects
                             navigation
                             additional))
  :diminish evil-org-mode)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-init))

(defmacro keybinds/set-leader-key (states keymap key op)
  "Bind KEY to OP at STATES and KEYMAP."
  `(evil-define-key ,states ,keymap (kbd ,(concat "<leader>" key)) ,op))

(defun keybinds/set-leader-key* (states keymap key op &rest key-ops)
  "Bind KEY-OPS lists at states and KEYMAP."
  (while key
    (evil-define-key states keymap (kbd (concat "<leader>" key)) op)
    (setq key (pop key-ops)
          op (pop key-ops))))
(put 'keybinds/set-leader-key* 'lisp-indent-function 'defun)

(defmacro keybinds/set-global-leader-key (key op)
  "Bind KEY to OP globally for all evil states."
  `(keybinds/set-leader-key nil 'global ,key ,op))

(defun keybinds/set-global-leader-key* (key op &rest key-ops)
  "Bind KEY to OP."
  (while key
    (evil-define-key nil 'global (kbd (concat "<leader>" key)) op)
    (setq key (pop key-ops)
          op (pop key-ops))))
(put 'keybinds/set-global-leader-key* 'lisp-indent-function 'defun)

;;; Define key
(keybinds/set-global-leader-key*
 ;; windows jump
  "wh" 'evil-window-left
  "wl" 'evil-window-right
  "wj" 'evil-window-down
  "wk" 'evil-window-up
  "wc" 'writeroom-mode
  ;; window split
  "wv" 'evil-window-vsplit
  "w-" 'evil-window-split
  "wd" 'evil-window-delete
  "w1" 'delete-other-windows
  "w0" 'delete-window
  ;;
  "<SPC>" 'execute-extended-command
  ;; Files
  "ff" 'find-file
  "fs" 'save-buffer
  "fS" 'evil-write-all
  "fr" 'recentf-open-files
  ;; Buffers
  "bb" 'ivy-switch-buffer
  "bd" 'kill-this-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bx" 'kill-buffer-and-window
  ;; Projects
  "pk" 'projectile-kill-buffers
  "pf" 'projectile-find-file
  "pb" 'projectile-switch-to-buffer
  "pp" 'projectile-switch-project
  ;; Searching
  "si" 'imenu
  "sp" 'keybinds/counsel-search-project
  "sP" 'keybinds/counsel-search-project-at-point
  "sd" 'keybinds/counsel-search-dir
  "sD" 'keybinds/counsel-search-dir-at-point
  "ss" 'swiper
  "sS" 'swiper-thing-at-point
  "sb" 'swiper-all
  "sB" 'swiper-all-thing-at-point
  ;; Flycheck
  "en" 'flycheck-next-error
  "ep" 'flycheck-previous-error
  "eb" 'flycheck-buffer
  "ec" 'flycheck-clear
  "eh" 'flycheck-describe-checker
  "es" 'flycheck-select-checker
  "ex" 'flycheck-explain-error-at-point
  ;; Magit
  "gs" 'magit-status
  "gd" 'magit-diff-range
  ;; Aweshell
  "'" 'aweshell-dedicated-toggle
  "ts" 'aweshell-toggle
  ;; Notes
  "nrf" 'org-roam-node-find
  ;; Operations
  "oy" 'youdao-dictionary-search-at-point+);; global keybindings

(keybinds/set-leader-key* nil lsp-mode-map
  ;; format
  "=b" #'lsp-format-buffer
  "=r" #'lsp-format-region
  "=o" #'lsp-organize-imports
  ;; code
  "cr" #'lsp-rename)

(when configs/enable-org
  (keybinds/set-leader-key* 'normal org-mode-map
    ;; basic
    "RET" 'org-open-at-point
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

(when (and configs/enable-org-roam configs/enable-org)
  (keybinds/set-leader-key* nil org-mode-map
    "mrg" 'org-roam-graph
    "mri" 'org-roam-node-insert
    "mrf" 'org-roam-node-find
    "mrta" 'org-roam-tag-add
    "mrtd" 'org-roam-tag-remove))

(provide 'keybinds)
;;; keybindings.el ends here
