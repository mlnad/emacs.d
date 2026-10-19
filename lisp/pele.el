;;; pele.el --- preview Mermaid blocks in Markdown with pele -*- lexical-binding: t; -*-

;;; Commentary:

;; `pele' (https://github.com/obsidianmd/pele) renders Mermaid to SVG with no
;; browser and no runtime dependencies.  This file turns it into a buffer-local
;; minor mode for Markdown: every ```mermaid fenced block is replaced by an
;; image overlay, and the source is revealed again while point is inside the
;; block.
;;
;;   (add-hook 'markdown-mode-hook #'pele-mode)
;;
;; Rendering is done by the small Node wrapper `pele-render.mjs', which lives
;; next to this file.  The wrapper is needed because pele emits SVG that uses
;; CSS custom properties (`fill="var(--_fg)"'); librsvg, Emacs' SVG renderer,
;; does not implement custom-property declarations and would draw black boxes.
;; The wrapper resolves those variables to literal colors (following the current
;; Emacs theme) before Emacs ever sees the SVG.
;;
;; Where do the SVGs go?
;;
;;   The source of truth is a content-addressed file per diagram, stored under
;;   `pele-directory'/svg/<sha1>.svg.  The sha1 covers the Mermaid text, the
;;   theme colors and the pele version, so equal diagrams in different files
;;   share one file and a theme change never shows a stale picture.  Files are
;;   kept in memory as image objects in a buffer-local hash table; they survive
;;   redisplay and are reused while the buffer lives.  The previous file allows
;;   a new Emacs session to show cached diagrams without starting Node.
;;
;;   Putting all SVGs into one temporary buffer was considered and rejected:
;;   an image overlay needs an image object (or file), not a buffer region;
;;   extracting substrings from a buffer adds encoding/position bookkeeping,
;;   and the data disappears together with the buffer.  A temp buffer *is* a
;;   reasonable place for a debugging index (see `pele-list-diagrams'), just
;;   not for the images themselves.
;;
;; Toolchain:
;;
;;   `pele-mode' checks node (>= 20), the pele module and `pele-render.mjs'
;;   when it is enabled.  If pele is missing it can `npm install' it into
;;   `pele-directory' (`pele-auto-install'), or use an existing checkout found
;;   next to the cache directory (e.g. ~/.emacs.d/.cache/pele/dist/pele.js).
;;
;; Commands:
;;
;;   M-x pele-mode                  inline image overlays in the current buffer
;;   M-x pele-refresh               re-render now (also C-c C-v)
;;   M-x pele-preview-mode          side-by-side source/preview in a side window
;;   M-x pele-preview-region        one-shot preview of the active region
;;   M-x pele-check                 report node / pele / cache status
;;   M-x pele-install               npm install pele into `pele-directory'
;;   M-x pele-clear-cache           delete cached SVGs
;;   M-x pele-open-svg-at-point     open the SVG of the block at point
;;   M-x pele-list-diagrams         show a *pele* buffer listing this buffer's
;;                                  blocks, their type and their SVG file
;;
;; Org buffers: `pele-mode' and `pele-preview-mode' recognize
;; `#+begin_src mermaid' blocks too.  Add `(add-hook 'org-mode-hook
;; #'pele-preview-mode)' if you want side previews for every Org buffer.
;;
;; Standalone .mmd / .mermaid buffers open in `pele-mmd-mode' (a tiny
;; prog-mode derivative); with `pele-preview-auto' (the default) the
;; side-by-side preview opens automatically.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'json)
(require 'subr-x)

(defvar pele-mode)
(defvar pele-preview-mode)

(defgroup pele nil
  "Preview Mermaid diagrams in Markdown buffers with pele."
  :group 'text
  :prefix "pele-")

(defconst pele--version "0.1.0"
  "Version of this Emacs package.")

(defconst pele--cache-format "pele-el-1"
  "Bump to invalidate every cached SVG after a format change.")

(defconst pele--fence-start-regexp
  "^[ \t]*\\(`\\{3,\\}\\|~\\{3,\\}\\)[ \t]*mermaid\\b[^\r\n]*\r?$"
  "Regexp matching the opening fence of a Mermaid block.")

(defvar pele--package-directory
  (file-name-directory (or load-file-name (locate-library "pele")))
  "Directory that holds pele.el, its .elc and pele-render.mjs.")

(defcustom pele-directory
  (expand-file-name "pele-runtime/"
                    (if (boundp 'moyu/cache-directory)
                        moyu/cache-directory
                      (locate-user-emacs-file ".cache/")))
  "Directory for pele's runtime data (node_modules, SVG cache).
`npm install' runs here and rendered SVGs live in the svg/ subdirectory."
  :type 'directory)

(defcustom pele-node-program "node"
  "Node.js executable; pele needs Node 20 or newer."
  :type 'string)

(defcustom pele-npm-program "npm"
  "npm executable, used by `pele-install'."
  :type 'string)

(defcustom pele-dist-path nil
  "Explicit path to pele's ESM bundle (dist/pele.js).
When nil, look in `pele-directory'/node_modules and next to the cache
directory (a checkout at ~/.emacs.d/.cache/pele/dist/pele.js is found)."
  :type '(choice (const :tag "Auto-detect" nil) file))

(defcustom pele-auto-install nil
  "When non-nil, `pele-mode' tries `npm install pele' if it is missing."
  :type 'boolean)

(defcustom pele-font-family "Arial"
  "CSS font family used for diagram labels.
Pele measures text with an Arial advance table when no canvas is
available, so a metric-compatible family (Arial, Liberation Sans) keeps
labels inside their boxes.  Set this to your Emacs font only if you also
accept slightly different text widths."
  :type 'string)

(defcustom pele-mono-font-family "monospace"
  "CSS font family used for monospaced diagram labels."
  :type 'string)

(defcustom pele-auto-refresh-delay 0.3
  "Idle seconds to wait after a change before re-rendering."
  :type 'number)

(defcustom pele-persist-svg t
  "Whether rendered SVGs are cached as files on disk.
When non-nil, every diagram is written to `pele-directory'/svg/ and
reused across Emacs sessions.  When nil, diagrams live only in memory:
nothing is written to disk, `pele-open-svg-at-point' is unavailable,
and a new Emacs session renders everything again."
  :type 'boolean)

(defcustom pele-series-colors nil
  "Optional list of up to eight colors for chart series.
When nil, pele's built-in palette is used."
  :type '(repeat color))

(defcustom pele-theme-overrides nil
  "Alist of theme keys to override, e.g. ((surface . \"#222222\")).
Keys are symbols: bg fg muted line surface surface-alt border accent
radius font font-mono series-1 ... series-8."
  :type '(alist :key-type symbol :value-type string))

(defcustom pele-lighter " Pele"
  "Mode line lighter for `pele-mode'."
  :type 'string)

(defcustom pele-max-image-width nil
  "Maximum image width in pixels, or nil to use the window body width."
  :type '(choice (const :tag "Window width" nil) integer))

;;;; Toolchain detection

(defvar pele--node-version-cache 'unspecified)
(defvar pele--dist-cache nil)
(defvar pele--cache-seed-cache nil)

(defun pele--executable (program)
  "Return PROGRAM's path if it is executable, otherwise its `executable-find'."
  (and program
       (or (and (file-name-absolute-p program) (file-executable-p program) program)
           (executable-find program))))

(defun pele--node-program ()
  (pele--executable pele-node-program))

(defun pele--npm-program ()
  (pele--executable pele-npm-program))

(defun pele--node-version ()
  "Major version of node, cached, or nil if it cannot be determined."
  (when (eq pele--node-version-cache 'unspecified)
    (setq pele--node-version-cache
          (when-let* ((node (pele--node-program))
                      (output (ignore-errors
                                (string-trim (car (process-lines node "--version"))))))
            (when (string-match "v\\([0-9]+\\)" output)
              (string-to-number (match-string 1 output))))))
  pele--node-version-cache)

(defun pele--node-ok-p ()
  "Non-nil when a usable node (>= 20) is installed."
  (when (pele--node-program)
    (let ((version (pele--node-version)))
      (or (null version) (>= version 20)))))

(defun pele--cache-root ()
  "Directory that contains `pele-directory', e.g. ~/.emacs.d/.cache/."
  (file-name-directory (directory-file-name (file-name-as-directory pele-directory))))

(defun pele--dist ()
  "Return the path of pele's ESM bundle, or nil."
  (or pele--dist-cache
      (setq pele--dist-cache
            (or (and pele-dist-path (file-exists-p pele-dist-path) pele-dist-path)
                (cl-some (lambda (path) (and (file-exists-p path) path))
                         (list (expand-file-name "node_modules/pele/dist/pele.js"
                                                 pele-directory)
                               (expand-file-name "pele/dist/pele.js"
                                                 (pele--cache-root))))))))

(defun pele--renderer-script ()
  "Path of the Node wrapper that lives next to this file."
  (expand-file-name "pele-render.mjs" pele--package-directory))

(defun pele--ready-p ()
  "Non-nil when node, pele and the renderer script are available."
  (and (pele--node-ok-p)
       (pele--dist)
       (let ((script (pele--renderer-script)))
         (and (stringp script) (file-exists-p script)))))

(defun pele-install ()
  "Install pele with npm into `pele-directory'."
  (interactive)
  (unless (pele--node-ok-p)
    (user-error "pele needs Node.js >= 20 (%s not found or too old)"
                pele-node-program))
  (let ((npm (pele--npm-program)))
    (unless npm
      (user-error "npm not found; install Node.js (which ships npm) first"))
    (make-directory pele-directory t)
    (message "pele: npm install pele in %s ..." pele-directory)
    (let ((buffer (get-buffer-create "*pele-install*")))
      (with-current-buffer buffer
        (erase-buffer))
      (let ((status (call-process npm nil buffer nil
                                  "install" "--prefix" pele-directory
                                  "--no-audit" "--no-fund" "pele")))
        (unless (and (integerp status) (zerop status))
          (display-buffer buffer)
          (user-error "pele: npm install failed (status %s); see *pele-install*"
                      status))
        (setq pele--dist-cache nil
              pele--node-version-cache 'unspecified
              pele--cache-seed-cache nil)
        (if (pele--dist)
            (message "pele: installed %s" (pele--dist))
          (display-buffer buffer)
          (user-error "pele: npm finished but no dist/pele.js was found"))))))

(defun pele--ensure-ready (&optional auto)
  "Check the toolchain; install pele when AUTO and `pele-auto-install'."
  (cond
   ((not (pele--node-ok-p))
    (message "pele: Node.js >= 20 is required (found %S)"
             (or (pele--node-version) "nothing"))
    nil)
   ((pele--dist) t)
   ((and auto pele-auto-install)
    (condition-case err
        (progn (pele-install) (and (pele--dist) t))
      (error (message "pele: %s" (error-message-string err)) nil)))
   (t
    (message "pele: pele module not found; run M-x pele-install or set `pele-dist-path'")
    nil)))

(defun pele-check ()
  "Report the status of the pele toolchain."
  (interactive)
  (message "pele %s
  node:      %s (v%s)
  npm:       %s
  pele:      %s
  renderer:  %s
  cache:     %s (%s)"
           pele--version
           (or (pele--node-program) "MISSING")
           (or (pele--node-version) "?")
           (or (pele--npm-program) "MISSING")
           (or (pele--dist) "MISSING")
           (let ((script (pele--renderer-script)))
             (if (and script (file-exists-p script)) script "MISSING"))
           pele-directory
           (if pele-persist-svg "on disk" "memory only")))

;;;; Theme colors

(defun pele--face-color (face attribute &optional fallback)
  "Color of FACE's ATTRIBUTE, or FALLBACK when unspecified."
  (let ((value (ignore-errors (face-attribute face attribute))))
    (if (and (stringp value)
             (not (string-prefix-p "unspecified" value)))
        value
      fallback)))

(defun pele--css-color (color)
  "Return COLOR as a CSS-friendly #rrggbb string when possible.
Emacs' color functions commonly return 16-bit components, which librsvg
does not understand."
  (when (stringp color)
    (or (ignore-errors
          (let ((rgb (color-name-to-rgb color)))
            (when rgb
              (apply #'color-rgb-to-hex (append rgb '(2))))))
        color)))

(defun pele--dark-p (&optional background)
  "Non-nil when BACKGROUND (default: the default face background) is dark."
  (let ((background (or background (pele--face-color 'default :background))))
    (cond
     ((null background)
      (eq (frame-parameter nil 'background-mode) 'dark))
     ((ignore-errors
        (let ((rgb (color-name-to-rgb background)))
          (and rgb
               (< (+ (* 0.2126 (nth 0 rgb))
                     (* 0.7152 (nth 1 rgb))
                     (* 0.0722 (nth 2 rgb)))
                  0.5)))))
     (t (eq (frame-parameter nil 'background-mode) 'dark)))))

(defun pele--theme-alist ()
  "Return the CSS variables for the current Emacs theme as an alist."
  (let* ((background (pele--css-color (pele--face-color 'default :background "#ffffff")))
         (foreground (pele--css-color (pele--face-color 'default :foreground "#000000")))
         (dark (pele--dark-p background))
         (surface (pele--css-color
                   (or (ignore-errors
                         (color-lighten-name background (if dark 6 -6)))
                       background)))
         (surface-alt (pele--css-color
                       (or (ignore-errors
                             (color-lighten-name surface (if dark 4 -4)))
                           surface)))
         (muted (or (pele--css-color (pele--face-color 'shadow :foreground foreground))
                    foreground))
         (border (or (pele--css-color (pele--face-color 'vertical-border :foreground muted))
                     muted))
         (accent (or (pele--css-color (pele--face-color 'link :foreground "#5b7bd5"))
                     "#5b7bd5"))
         (alist (list (cons 'bg background)
                      (cons 'fg foreground)
                      (cons 'muted muted)
                      (cons 'line foreground)
                      (cons 'surface surface)
                      (cons 'surface-alt surface-alt)
                      (cons 'border border)
                      (cons 'accent accent)
                      (cons 'radius "4px")
                      (cons 'font pele-font-family)
                      (cons 'font-mono pele-mono-font-family))))
    (cl-loop for color in pele-series-colors
             for index from 1
             do (push (cons (intern (format "series-%d" index)) color) alist))
    (dolist (override pele-theme-overrides)
      (setf (alist-get (car override) alist) (cdr override)))
    ;; Normalize every color slot, including overrides and series colors.
    (dolist (cell alist)
      (when (or (memq (car cell) '(bg fg muted line surface surface-alt border accent))
                (string-prefix-p "series-" (symbol-name (car cell))))
        (setcdr cell (pele--css-color (cdr cell)))))
    alist))

(defun pele--theme-json ()
  "Serialize the current theme colors to the JSON the Node wrapper expects."
  (json-serialize (pele--theme-alist)))

;;;; Cache files

(defun pele--svg-directory ()
  (expand-file-name "svg/" pele-directory))

(defun pele--svg-file (key)
  (expand-file-name (concat key ".svg") (pele--svg-directory)))

(defun pele--cache-seed ()
  "String that changes when pele or the renderer changes."
  (or pele--cache-seed-cache
      (setq pele--cache-seed-cache
            (let ((dist (pele--dist)))
              (concat pele--version ":"
                      (if-let* ((attributes (ignore-errors (file-attributes dist))))
                          (format "%.0f"
                                  (float-time (file-attribute-modification-time attributes)))
                        "none"))))))

(defun pele--key (text theme-json)
  "Cache key for TEXT rendered with THEME-JSON."
  (secure-hash 'sha1
               (concat pele--cache-format "\0" (pele--cache-seed) "\0"
                       theme-json "\0" text)))

(defun pele--write-svg (key svg)
  "Write SVG to the cache file for KEY."
  (let ((file (pele--svg-file key)))
    (unless (file-directory-p (file-name-directory file))
      (make-directory (file-name-directory file) t))
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file file
        (insert svg)))))

(defun pele--read-svg-entry (key)
  "Read a cached SVG entry for KEY, or nil.
Width, height and diagram type are recovered from the root <svg> tag."
  (let ((file (pele--svg-file key)))
    (when (file-exists-p file)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix))
          (insert-file-contents file))
        (let* ((head (buffer-substring-no-properties
                      (point-min) (min (point-max) 1200)))
               (width (and (string-match "width=\"\\([0-9.]+\\)\"" head)
                           (string-to-number (match-string 1 head))))
               (height (and (string-match "height=\"\\([0-9.]+\\)\"" head)
                            (string-to-number (match-string 1 head))))
               (type (and (string-match "class=\"pele pele-\\([a-zA-Z0-9]+\\)\"" head)
                          (match-string 1 head))))
          (when (and width height (> width 0))
            (pele-entry-create :svg (buffer-string)
                               :width width
                               :height height
                               :type type)))))))

;;;; Data structures

(cl-defstruct (pele-entry (:constructor pele-entry-create))
  "One rendered diagram."
  svg width height type error image)

(cl-defstruct (pele-block (:constructor pele-block-create))
  "One ```mermaid block in a buffer."
  start end text key entry overlay)

(defvar-local pele--blocks nil
  "`pele-block' structs for the current buffer.")

(defvar-local pele--data nil
  "Hash table KEY -> `pele-entry' for the current buffer.")

(defvar-local pele--timer nil
  "Idle timer waiting to refresh the current buffer.")

(defvar-local pele--refreshing nil
  "Non-nil while a refresh runs, to avoid re-entrancy.")

(defvar-local pele--image-width nil
  "Pixel width the current images were built for.")

(defvar-local pele--last-point nil
  "Point before the current command, for detecting entry direction.")

(defvar pele-enter-block-motion-commands
  '(previous-line next-line scroll-down-command scroll-up-command
    evil-previous-line evil-next-line)
  "Commands treated as vertical movement when entering a hidden block.
The default covers stock line motion and Evil's j/k.  Add your own
motion commands here if you use another keybinding layer.")

;;;; Parsing blocks

(defun pele--scan-fenced-blocks ()
  "Return `pele-block' structs for every Mermaid fenced block in the buffer."
  (let (blocks)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward pele--fence-start-regexp nil t)
          (let* ((fence (match-string 1))
                 (start (match-beginning 0))
                 (body-start (line-beginning-position 2))
                 (closing (concat "^[ \t]*" (regexp-quote fence) "[ \t]*\r?$")))
            (when (and (<= body-start (point-max))
                       (re-search-forward closing nil t))
              (let ((body-end (match-beginning 0))
                    (end (match-end 0)))
                (push (pele-block-create
                       :start start
                       :end end
                       :text (buffer-substring-no-properties body-start body-end))
                      blocks)))))))
    (nreverse blocks)))

(defun pele--scan-org-blocks ()
  "Return `pele-block' structs for Mermaid `src' blocks in an Org buffer."
  (let (blocks)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward "^[ \t]*#\\+begin_src[ \t]+mermaid\\b[^\r\n]*\r?$" nil t)
          (let ((start (match-beginning 0))
                (body-start (line-beginning-position 2)))
            (when (and (<= body-start (point-max))
                       (re-search-forward "^[ \t]*#\\+end_src[ \t]*\r?$" nil t))
              (let ((body-end (match-beginning 0))
                    (end (match-end 0)))
                (push (pele-block-create
                       :start start
                       :end end
                       :text (buffer-substring-no-properties body-start body-end))
                      blocks)))))))
    (nreverse blocks)))

(defun pele--scan-blocks ()
  "Return `pele-block' structs for Mermaid blocks in the current buffer.
Org buffers use `#+begin_src mermaid' blocks; everything else uses
Markdown fenced code blocks."
  (if (derived-mode-p 'org-mode)
      (pele--scan-org-blocks)
    (pele--scan-fenced-blocks)))

(defun pele--block-at-point ()
  "Return the `pele-block' at point, or nil."
  (cl-find-if (lambda (block)
                (and (<= (pele-block-start block) (point))
                     (< (point) (pele-block-end block))))
              pele--blocks))

;;;; Rendering

(defun pele--render-pending (pairs theme-json)
  "Render PAIRS (alist of KEY . TEXT) in one node process.
Return a hash table KEY -> `pele-entry'."
  (let ((node (pele--node-program))
        (dist (pele--dist))
        (script (pele--renderer-script))
        (entries (make-hash-table :test #'equal)))
    (unless (and node (file-exists-p script) dist (file-exists-p dist))
      (error "pele toolchain is not ready; run M-x pele-check"))
    (let ((request (json-serialize
                    (vconcat (mapcar (lambda (pair)
                                       (list :id (car pair) :text (cdr pair)))
                                     pairs)))))
      ;; json-serialize returns a unibyte UTF-8 string; keep stdin/stdout binary
      ;; so Emacs does not re-encode the bytes as individual characters.
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert request)
        (let ((coding-system-for-read 'binary)
              (coding-system-for-write 'binary)
              (out (generate-new-buffer " *pele-out*")))
          (unwind-protect
              (progn
                (with-current-buffer out
                  (set-buffer-multibyte nil))
                (let ((status (apply #'call-process-region
                                     (point-min) (point-max) node nil out nil
                                     (list script dist theme-json))))
                  (unless (and (integerp status) (zerop status))
                    (error "pele: node exited with %s: %s"
                           status
                           (string-trim
                            (decode-coding-string
                             (with-current-buffer out (buffer-string))
                             'utf-8)))))
                (dolist (result (json-parse-string
                                 (decode-coding-string
                                  (with-current-buffer out (buffer-string))
                                  'utf-8)
                                 :object-type 'alist
                                 :array-type 'list))
                  (puthash (alist-get 'id result)
                           (if (eq (alist-get 'ok result) t)
                               (pele-entry-create
                                :svg (alist-get 'svg result)
                                :width (alist-get 'width result)
                                :height (alist-get 'height result)
                                :type (alist-get 'type result))
                             (pele-entry-create
                              :error (alist-get 'error result)))
                           entries)))
            (kill-buffer out)))))
    entries))

(defun pele--resolve (blocks theme-json)
  "Return KEY -> `pele-entry' for BLOCKS, rendering what is not cached."
  (let ((pending nil)
        (entries (make-hash-table :test #'equal)))
    (dolist (block blocks)
      (let* ((key (pele-block-key block))
             (known (or (gethash key pele--data)
                        (and pele-persist-svg (pele--read-svg-entry key)))))
        (if known
            (progn
              (puthash key known pele--data)
              (puthash key known entries))
          (push (cons key (pele-block-text block)) pending))))
    (when pending
      (let ((fresh (pele--render-pending (nreverse pending) theme-json)))
        (maphash (lambda (key entry)
                   (when (and pele-persist-svg
                              (pele-entry-svg entry))
                     (pele--write-svg key (pele-entry-svg entry)))
                   (puthash key entry pele--data)
                   (puthash key entry entries))
                 fresh)))
    entries))

(defun pele--ensure-data ()
  "Return the buffer-local render cache, creating it when needed."
  (or pele--data
      (setq-local pele--data (make-hash-table :test #'equal))))

(defun pele--entry (text)
  "Return a `pele-entry' for TEXT, using the cache or the renderer.
The entry is stored in the buffer-local cache and, when
`pele-persist-svg' is non-nil, written to the SVG cache directory."
  (let* ((theme (pele--theme-json))
         (key (pele--key text theme))
         (cache (pele--ensure-data))
         (known (or (gethash key cache)
                    (and pele-persist-svg (pele--read-svg-entry key)))))
    (unless known
      (let ((fresh (pele--render-pending (list (cons key text)) theme)))
        (setq known (or (gethash key fresh)
                        (pele-entry-create :error "pele returned no result")))
        (when (and pele-persist-svg (pele-entry-svg known))
          (pele--write-svg key (pele-entry-svg known)))))
    (puthash key known cache)
    known))

(defun pele--max-width ()
  "Pixel width available for images, or nil."
  (or pele-max-image-width
      (and (display-graphic-p)
           (ignore-errors (window-body-width nil t)))))

(defun pele--create-image (svg &optional max-width)
  "Create an Emacs image from SVG, scaled to MAX-WIDTH pixels when given.
MAX-WIDTH defaults to the selected window's body width."
  (let ((max-width (or max-width (pele--max-width))))
    (apply #'create-image svg 'svg t
           (append (list :scale 1)
                   (when (and max-width
                              (ignore-errors (image-transforms-p)))
                     (list :max-width max-width))))))

(defun pele--image (entry)
  "Return ENTRY's image, creating it on demand."
  (or (pele-entry-image entry)
      (when-let* ((svg (pele-entry-svg entry)))
        (setf (pele-entry-image entry) (pele--create-image svg)))))

;;;; Overlays

(defun pele--reveal-overlay (overlay &rest _)
  "Temporarily reveal OVERLAY, for isearch."
  (overlay-put overlay 'display nil))

(defun pele--make-overlay (block entry)
  "Create the image overlay for BLOCK using ENTRY."
  (let ((overlay (make-overlay (pele-block-start block)
                               (pele-block-end block))))
    (overlay-put overlay 'pele t)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'isearch-open-invisible #'pele--reveal-overlay)
    (when-let* ((error (pele-entry-error entry)))
      (overlay-put overlay 'after-string
                   (propertize (format "\n⚠ pele: %s" error) 'face 'error)))
    (when (pele-entry-svg entry)
      (pele--image entry)
      (overlay-put overlay 'help-echo
                   "Mermaid preview; move point into the block to edit the source"))
    overlay))

(defun pele--delete-overlays ()
  "Delete all pele overlays in the current buffer."
  (dolist (block pele--blocks)
    (when-let* ((overlay (pele-block-overlay block)))
      (delete-overlay overlay)))
  (setq pele--blocks nil))

(defun pele--enter-block-target (block previous)
  "Position to use when entering BLOCK from PREVIOUS.
Coming from below puts point on the block's last line; coming from
above on its first line, so that the image overlay collapsing the
hidden text does not make point jump to the beginning."
  (cond
   ((>= previous (pele-block-end block))
    (save-excursion
      (goto-char (max (pele-block-start block)
                      (1- (pele-block-end block))))
      (line-beginning-position)))
   ((<= previous (pele-block-start block))
    (pele-block-start block))))

(defun pele--update-visibility (&rest _)
  "Show the image everywhere except in the block containing point.
When vertical movement enters a hidden block, point is placed on the
side it came from instead of the first line of the block."
  (when (and pele-mode pele--blocks)
    (let ((position (point))
          (previous pele--last-point)
          (motion (memq this-command pele-enter-block-motion-commands))
          target)
      (dolist (block pele--blocks)
        (when-let* ((overlay (pele-block-overlay block)))
          (let* ((entry (pele-block-entry block))
                 (inside (and (<= (overlay-start overlay) position)
                              (< position (overlay-end overlay))))
                 (outside-before
                  (and previous
                       (or (< previous (pele-block-start block))
                           (>= previous (pele-block-end block)))))
                 (image (and (not inside) (pele--image entry))))
            (when (and inside motion outside-before)
              (setq target (pele--enter-block-target block previous)))
            (overlay-put overlay 'display image))))
      (when (and target (/= target position))
        (goto-char target)
        (setq position target))
      (setq pele--last-point position))))

;;;; Refresh

(defun pele--refresh-1 (buffer)
  "Re-render all Mermaid blocks in BUFFER."
  (with-current-buffer buffer
    (when (and pele-mode (pele--ready-p))
      (let ((pele--refreshing t))
        (unwind-protect
            (let* ((theme-json (pele--theme-json))
                   (blocks (pele--scan-blocks))
                   (width (pele--max-width)))
              ;; A changed window width invalidates the scaled image objects.
              (unless (equal width pele--image-width)
                (setq pele--image-width width)
                (when pele--data
                  (maphash (lambda (_key entry)
                             (setf (pele-entry-image entry) nil))
                           pele--data)))
              (dolist (block blocks)
                (setf (pele-block-key block)
                      (pele--key (pele-block-text block) theme-json)))
              (let ((entries (pele--resolve blocks theme-json)))
                (pele--delete-overlays)
                (setq pele--blocks
                      (mapcar (lambda (block)
                                (let ((entry (or (gethash (pele-block-key block) entries)
                                                 (pele-entry-create
                                                  :error "pele returned no result"))))
                                  (setf (pele-block-entry block) entry)
                                  (setf (pele-block-overlay block)
                                        (pele--make-overlay block entry))
                                  block))
                              blocks))
                (pele--update-visibility)))
          (setq pele--refreshing nil))))))

(defun pele--refresh (&optional buffer)
  "Re-render the Mermaid blocks in BUFFER (default: current buffer)."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (unless (and pele-mode pele--refreshing)
          (condition-case err
              (pele--refresh-1 buffer)
            (error
             (message "pele: %s" (error-message-string err)))))))))

(defun pele-refresh ()
  "Re-render all Mermaid blocks in the current buffer now."
  (interactive)
  (pele--refresh (current-buffer)))

(defun pele--schedule-refresh (&rest _)
  "Re-render after a short idle delay, coalescing rapid edits."
  (when pele-mode
    (when (timerp pele--timer)
      (cancel-timer pele--timer))
    (setq pele--timer
          (run-with-idle-timer pele-auto-refresh-delay nil
                               #'pele--refresh (current-buffer)))))

(defun pele--teardown ()
  "Remove hooks, timer and overlays from the current buffer."
  (when (timerp pele--timer)
    (cancel-timer pele--timer)
    (setq pele--timer nil))
  (pele--delete-overlays)
  (remove-hook 'after-change-functions #'pele--schedule-refresh t)
  (remove-hook 'after-save-hook #'pele--refresh t)
  (remove-hook 'after-revert-hook #'pele--refresh t)
  (remove-hook 'post-command-hook #'pele--update-visibility t)
  (remove-hook 'kill-buffer-hook #'pele--teardown t)
  (remove-hook 'change-major-mode-hook #'pele--teardown t)
  (setq pele--blocks nil
        pele--last-point nil))

;;;; Cache maintenance

(defun pele-clear-cache ()
  "Delete every cached SVG and forget in-memory render data."
  (interactive)
  (when (file-directory-p (pele--svg-directory))
    (delete-directory (pele--svg-directory) t))
  (setq pele--cache-seed-cache nil
        pele--dist-cache nil)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p pele--data)
        (clrhash pele--data))))
  (message "pele: SVG cache cleared"))

(defun pele-open-svg-at-point ()
  "Open the SVG file of the Mermaid block at point."
  (interactive)
  (let ((block (pele--block-at-point)))
    (cond
     ((null block) (user-error "Point is not inside a Mermaid block"))
     ((null (pele-block-key block)) (user-error "This block has not been rendered yet"))
     ((not pele-persist-svg) (user-error "SVGs are memory-only; set `pele-persist-svg' to open files"))
     (t (find-file (pele--svg-file (pele-block-key block)))))))

(defun pele-list-diagrams ()
  "List this buffer's Mermaid blocks in a `*pele*' buffer."
  (interactive)
  (let ((source (buffer-name))
        (lines (mapcar (lambda (block)
                         (let ((entry (pele-block-entry block)))
                           (format "%5d-%5d  %-14s %s"
                                   (pele-block-start block)
                                   (pele-block-end block)
                                   (or (and entry (pele-entry-type entry)) "-")
                                   (pele--svg-file (pele-block-key block)))))
                       pele--blocks)))
    (with-current-buffer (get-buffer-create "*pele*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Mermaid blocks in %s (%d)\n\n"
                        source
                        (length pele--blocks)))
        (dolist (line lines)
          (insert line "\n")))
      (special-mode)
      (display-buffer (current-buffer)))))

;;;; Side-by-side preview

(defcustom pele-preview-width 0.5
  "Width of the side preview window, as a fraction of the frame width."
  :type 'number)

(defcustom pele-preview-auto t
  "When non-nil, opening a .mmd/.mermaid file shows the side preview."
  :type 'boolean)

(defcustom pele-preview-refresh-delay 0.2
  "Idle seconds before the side preview follows an edit."
  :type 'number)

(defvar-local pele--preview-buffer nil
  "Preview buffer belonging to this source buffer.")

(defvar-local pele--preview-timer nil
  "Idle timer that updates the side preview.")

(defvar-local pele--preview-source nil
  "Source buffer of a preview buffer.")

(defun pele--preview-get-buffer (source &optional create)
  "Return SOURCE's preview buffer; create it when CREATE is non-nil."
  (let ((name (format "*pele preview: %s*" (buffer-name source))))
    (or (get-buffer name)
        (when create
          (with-current-buffer (get-buffer-create name)
            (special-mode)
            (setq-local pele--preview-source source)
            (setq-local truncate-lines t)
            (setq-local buffer-read-only t)
            (current-buffer))))))

(defun pele--preview-window (buffer)
  "Return a side window showing BUFFER, creating one if needed."
  (or (get-buffer-window buffer t)
      (display-buffer-in-side-window
       buffer
       `((side . right)
         (slot . 0)
         (window-width . ,pele-preview-width)
         (preserve-size . (t . t))))))

(defun pele--preview-entries ()
  "Return the `pele-entry' objects that belong to the current buffer."
  (if (derived-mode-p 'pele-mmd-mode)
      (list (pele--entry (buffer-string)))
    (let ((blocks (pele--scan-blocks)))
      (cond
       (blocks
        (mapcar (lambda (block) (pele--entry (pele-block-text block)))
                blocks))
       ((derived-mode-p 'markdown-mode 'markdown-ts-mode 'org-mode)
        nil)
       (t (list (pele--entry (buffer-string))))))))

(defun pele--preview-insert-entry (entry width)
  "Insert ENTRY as an image of at most WIDTH pixels."
  (if-let* ((error (pele-entry-error entry)))
      (insert (propertize (format "⚠ pele: %s" error) 'face 'error))
    (let ((image (pele--create-image (pele-entry-svg entry) width)))
      (if image
          (insert-image image)
        (insert (propertize "pele: cannot create image" 'face 'error))))))

(defun pele--preview-refresh (&optional source)
  "Rebuild the side preview of SOURCE (default: current buffer)."
  (let ((source (or source (current-buffer))))
    (when (buffer-live-p source)
      (let* ((preview (pele--preview-get-buffer source t))
             (window (pele--preview-window preview))
             (width (ignore-errors (window-body-width window t))))
        (with-current-buffer preview
          (let ((inhibit-read-only t)
                (entries (with-current-buffer source (pele--preview-entries))))
            (erase-buffer)
            (if (null entries)
                (insert (propertize "No Mermaid diagram in this buffer.\n"
                                    'face 'shadow))
              (dolist (entry entries)
                (pele--preview-insert-entry entry width)
                (insert "\n")))
            (goto-char (point-min))
            (set-buffer-modified-p nil)))
        (with-current-buffer source
          (setq-local pele--preview-buffer preview))))))

(defun pele--preview-schedule (&rest _)
  "Update the side preview after a short idle delay."
  (when pele-preview-mode
    (when (timerp pele--preview-timer)
      (cancel-timer pele--preview-timer))
    (setq pele--preview-timer
          (run-with-idle-timer pele-preview-refresh-delay nil
                               #'pele--preview-refresh (current-buffer)))))

(defun pele--preview-teardown ()
  "Remove the side preview and its hooks from the current buffer."
  (when (timerp pele--preview-timer)
    (cancel-timer pele--preview-timer)
    (setq pele--preview-timer nil))
  (remove-hook 'after-change-functions #'pele--preview-schedule t)
  (remove-hook 'after-save-hook #'pele--preview-refresh t)
  (remove-hook 'kill-buffer-hook #'pele--preview-teardown t)
  (remove-hook 'change-major-mode-hook #'pele--preview-teardown t)
  (when (and (buffer-live-p pele--preview-buffer)
             (get-buffer-window pele--preview-buffer t))
    (delete-window (get-buffer-window pele--preview-buffer t)))
  (when (buffer-live-p pele--preview-buffer)
    (kill-buffer pele--preview-buffer))
  (setq pele--preview-buffer nil))

(defun pele-preview-refresh ()
  "Update the side preview for the current buffer."
  (interactive)
  (pele--preview-refresh (current-buffer)))

(defun pele-preview-region (beg end)
  "Render the Mermaid text between BEG and END in a side window.
This is a one-shot preview; it does not follow edits."
  (interactive "r")
  (unless (use-region-p)
    (user-error "Select a Mermaid diagram first"))
  (unless (pele--images-supported-p)
    (user-error "This frame cannot display SVG images"))
  (unless (pele--ensure-ready t)
    (user-error "pele toolchain is not ready"))
  (let* ((text (buffer-substring-no-properties beg end))
         (entry (pele--entry text))
         (buffer (get-buffer-create "*pele preview: region*")))
    (with-current-buffer buffer
      (special-mode)
      (setq-local buffer-read-only t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let* ((window (pele--preview-window (current-buffer)))
               (width (ignore-errors (window-body-width window t))))
          (pele--preview-insert-entry entry width))
        (insert "\n")
        (goto-char (point-min))
        (set-buffer-modified-p nil)))
    (pele--preview-window buffer)))

(defun pele--preview-window-resized (frame)
  "Refresh previews in FRAME after its windows changed size."
  (dolist (window (window-list frame 'no-minibuf))
    (with-current-buffer (window-buffer window)
      (when (and (bound-and-true-p pele--preview-source)
                 (buffer-live-p pele--preview-source))
        (with-current-buffer pele--preview-source
          (when pele-preview-mode
            (pele--preview-refresh pele--preview-source)))))))

(add-hook 'window-size-change-functions #'pele--preview-window-resized)

(defvar pele-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-r") #'pele-preview-refresh)
    map)
  "Keymap for `pele-preview-mode'.")

;;;###autoload
(define-minor-mode pele-preview-mode
  "Show a live rendered preview of this buffer in a side window.
Markdown buffers preview every Mermaid fenced block, Org buffers
preview every `#+begin_src mermaid' block, and standalone Mermaid
buffers preview the whole buffer."
  :lighter " Pele-Pv"
  :keymap pele-preview-mode-map
  (if pele-preview-mode
      (cond
       ((not (pele--images-supported-p))
        (setq pele-preview-mode nil)
        (message "pele: this frame cannot display SVG images (GUI Emacs required)"))
       ((not (pele--ensure-ready t))
        (setq pele-preview-mode nil))
       (t
        (pele--ensure-data)
        (setq-local pele--preview-buffer nil)
        (setq-local pele--preview-timer nil)
        (add-hook 'after-change-functions #'pele--preview-schedule nil t)
        (add-hook 'after-save-hook #'pele--preview-refresh nil t)
        (add-hook 'kill-buffer-hook #'pele--preview-teardown nil t)
        (add-hook 'change-major-mode-hook #'pele--preview-teardown nil t)
        (pele--preview-refresh (current-buffer))))
    (pele--preview-teardown)))

;;;; Minor mode

(defvar pele-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-v") #'pele-refresh)
    map)
  "Keymap for `pele-mode'.")

;;;###autoload
(define-minor-mode pele-mode
  "Preview Mermaid fenced blocks as images using pele.
The source stays in the buffer; an image overlay covers the block.
When point moves into the block the image is removed and the source
is shown, so it remains editable.  Diagrams are re-rendered after an
idle delay when the buffer changes.

Requires a GUI frame (SVG image support) and Node.js >= 20."
  :lighter pele-lighter
  :keymap pele-mode-map
  (if pele-mode
      (cond
       ((not (pele--images-supported-p))
        (setq pele-mode nil)
        (message "pele: this frame cannot display SVG images (GUI Emacs required)"))
       ((not (pele--ensure-ready t))
        (setq pele-mode nil))
       (t
        (setq-local pele--data (make-hash-table :test #'equal))
        (setq-local pele--blocks nil)
        (setq-local pele--timer nil)
        (setq-local pele--image-width nil)
        (setq-local pele--last-point (point))
        (add-hook 'after-change-functions #'pele--schedule-refresh nil t)
        (add-hook 'after-save-hook #'pele--refresh nil t)
        (add-hook 'after-revert-hook #'pele--refresh nil t)
        (add-hook 'post-command-hook #'pele--update-visibility nil t)
        (add-hook 'kill-buffer-hook #'pele--teardown nil t)
        (add-hook 'change-major-mode-hook #'pele--teardown nil t)
        (pele--schedule-refresh)))
    (pele--teardown)))

(defun pele--images-supported-p ()
  "Non-nil when this frame can display SVG images."
  (and (display-images-p)
       (image-type-available-p 'svg)))

;;;###autoload
(defun pele-mode-maybe ()
  "Enable `pele-mode' when the current frame can display images.
Silently do nothing on a terminal frame, so adding it to
`markdown-mode-hook' is harmless for `emacs -nw'."
  (when (display-graphic-p)
    (pele-mode 1)))

;;;###autoload
(define-derived-mode pele-mmd-mode prog-mode "Mermaid"
  "Major mode for standalone Mermaid files (.mmd, .mermaid).
The whole buffer is one diagram.  When `pele-preview-auto' is
non-nil and this frame can display SVG images, the buffer is shown
in a side-by-side preview automatically; toggle it with
`M-x pele-preview-mode'."
  (setq-local comment-start "%% ")
  (setq-local comment-end "")
  (setq-local font-lock-defaults nil)
  (when (and pele-preview-auto
             (pele--images-supported-p))
    (pele-preview-mode 1)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(?:mmd\\|mermaid\\)\\'" . pele-mmd-mode))

;;;; Tests
;; Run: emacs -Q --batch -L lisp -l lisp/pele.el \
;;        --eval "(ert-run-tests-batch-and-exit)"

(with-eval-after-load 'ert

  (ert-deftest pele/scan-blocks ()
    "Both fence styles are recognized and the text is correct."
    (with-temp-buffer
      (insert "# title\n\n"
              "```mermaid\nflowchart LR\n  A-->B\n```\n\n"
              "text\n\n"
              "~~~mermaid\npie\n~~~\n")
      (let ((blocks (pele--scan-blocks)))
        (should (= 2 (length blocks)))
        (should (equal "flowchart LR\n  A-->B\n"
                       (pele-block-text (nth 0 blocks))))
        (should (equal "pie\n" (pele-block-text (nth 1 blocks)))))))

  (ert-deftest pele/scan-blocks-ignores-other-languages ()
    (with-temp-buffer
      (insert "```elisp\n(message \"hi\")\n```\n")
      (should-not (pele--scan-blocks))))

  (ert-deftest pele/org-blocks ()
    "Org src blocks are found in an Org buffer."
    (with-temp-buffer
      (setq major-mode 'org-mode)
      (insert "#+begin_src mermaid\nflowchart LR\n  A-->B\n#+end_src\n")
      (let ((blocks (pele--scan-blocks)))
        (should (= 1 (length blocks)))
        (should (equal "flowchart LR\n  A-->B\n"
                       (pele-block-text (car blocks)))))))

  (ert-deftest pele/theme-json-roundtrip ()
    (let ((json (pele--theme-json)))
      (should (stringp json))
      (let ((object (json-parse-string json :object-type 'alist)))
        (should (alist-get 'bg object))
        (should (alist-get 'fg object))
        (should (equal pele-font-family (alist-get 'font object))))))

  (ert-deftest pele/cache-key-is-stable ()
    (let ((text "flowchart LR\n  A-->B\n")
          (theme (pele--theme-json)))
      (should (equal (pele--key text theme) (pele--key text theme)))
      (should-not (equal (pele--key text theme)
                         (pele--key (concat text "  C-->D\n") theme)))))

  (ert-deftest pele/memory-only ()
    "With `pele-persist-svg' nil nothing is written, but rendering works."
    (skip-unless (and (pele--node-ok-p) (pele--dist)))
    (let ((pele-auto-install nil)
          (pele-persist-svg nil))
      (with-temp-buffer
        (insert "```mermaid\nflowchart LR\n  mem-only-test-->B\n```\n")
        (cl-letf (((symbol-function 'display-images-p)
                   (lambda (&optional _) t)))
          (pele-mode 1)
          (pele--refresh)
          (let ((block (car pele--blocks)))
            (should (string-prefix-p "<svg" (pele-entry-svg (pele-block-entry block))))
            (should-not (file-exists-p (pele--svg-file (pele-block-key block)))))
          (pele-mode -1)))))

  (ert-deftest pele/render-and-reveal ()
    "Render one block, then toggle between image and source."
    (skip-unless (and (pele--node-ok-p) (pele--dist)))
    (let ((pele-auto-install nil))
      (with-temp-buffer
        (insert "# title\n\n```mermaid\nflowchart LR\n  A-->B\n```\n\ntail\n")
        (cl-letf (((symbol-function 'display-images-p)
                   (lambda (&optional _) t)))
          (pele-mode 1)
          (should pele-mode)
          (pele--refresh)
          (should (= 1 (length pele--blocks)))
          (let* ((block (car pele--blocks))
                 (entry (pele-block-entry block)))
            (should (string-prefix-p "<svg" (pele-entry-svg entry)))
            (should (> (pele-entry-width entry) 0))
            (should (file-exists-p (pele--svg-file (pele-block-key block))))
            (goto-char (point-min))
            (pele--update-visibility)
            (should (overlay-get (pele-block-overlay block) 'display))
            (goto-char (pele-block-start block))
            (pele--update-visibility)
            (should-not (overlay-get (pele-block-overlay block) 'display)))
          (pele-mode -1)
          (should-not pele--blocks)))))

  (ert-deftest pele/preview-entries-standalone ()
    "A standalone Mermaid buffer previews its whole contents."
    (skip-unless (and (pele--node-ok-p) (pele--dist)))
    (with-temp-buffer
      (setq major-mode 'pele-mmd-mode)
      (insert "flowchart LR\n  standalone-->B\n")
      (let ((entries (pele--preview-entries)))
        (should (= 1 (length entries)))
        (should (string-prefix-p "<svg" (pele-entry-svg (car entries)))))))

  (ert-deftest pele/enter-block-from-below ()
    "Entering a hidden block from below lands on its last line."
    (skip-unless (and (pele--node-ok-p) (pele--dist)))
    (let ((pele-auto-install nil))
      (with-temp-buffer
        (insert "```mermaid\nflowchart LR\n  A-->B\n```\n\ntail\n")
        (cl-letf (((symbol-function 'display-images-p)
                   (lambda (&optional _) t)))
          (pele-mode 1)
          (pele--refresh)
          (let* ((block (car pele--blocks))
                 (start (pele-block-start block))
                 (end (pele-block-end block))
                 (last-line (save-excursion
                              (goto-char (1- end))
                              (line-beginning-position))))
            ;; moving up from below: line-move lands on START because the
            ;; display overlay collapses the block; we correct to the last line.
            (goto-char (point-max))
            (setq pele--last-point (point))
            (let ((this-command 'previous-line))
              (goto-char start)
              (pele--update-visibility))
            (should (= (point) last-line))
            ;; moving down from above stays on the first line.
            (goto-char (point-min))
            (setq pele--last-point (point))
            (let ((this-command 'next-line))
              (goto-char start)
              (pele--update-visibility))
            (should (= (point) start))
            ;; a mouse click inside is not a vertical motion: no correction.
            (goto-char (point-max))
            (setq pele--last-point (point))
            (let ((this-command 'mouse-set-point))
              (goto-char start)
              (pele--update-visibility))
            (should (= (point) start)))
          (pele-mode -1)))))

  (ert-deftest pele/preview-buffer ()
    "The side preview buffer is created and contains an image."
    (skip-unless (and (pele--node-ok-p) (pele--dist)))
    (let ((pele-auto-install nil)
          (pele-persist-svg nil))
      (with-temp-buffer
        (setq major-mode 'pele-mmd-mode)
        (insert "flowchart LR\n  preview-->B\n")
        (cl-letf (((symbol-function 'display-images-p)
                   (lambda (&optional _) t)))
          (pele-preview-mode 1)
          (should pele-preview-mode)
          (let ((preview pele--preview-buffer))
            (should (buffer-live-p preview))
            (with-current-buffer preview
              (should (get-text-property (point-min) 'display))))
          (pele-preview-mode -1)
          (should-not pele--preview-buffer))))))

(provide 'pele)
;;; pele.el ends here
