;;; treesit.el --- Pinned tree-sitter grammars for `moyue treesit' -*- lexical-binding: t; -*-

;;; Commentary:

;; Grammar libraries are pinned to exact upstream revisions and live below
;; `.cache/tree-sitter/', with the checkouts they were built from under
;; `.cache/tree-sitter/src/<lang>/'.  The cache is searched before
;; ~/.emacs.d/tree-sitter/ and before the system library directories, so a
;; grammar built there always wins, while one that is not built yet falls
;; back to whatever else provides it.
;;
;; Everything needed to *produce* those libraries lives here, because it is
;; only ever used from the command line:
;;
;;   moyue treesit status|fetch|build|sync|update|verify|reset
;;
;; init.el needs none of it.  A running editor only has to be told where the
;; libraries are, and that value is a pure function of `user-emacs-directory',
;; so init.org declares it in two lines of plain `setq' instead of loading a
;; library.  `moyue install --grammars' is what materialises and builds the
;; libraries in the first place.
;;
;; `moyue-treesit-manifest' is the single source of truth.  Nothing here ever
;; follows a default branch on its own: `fetch' moves a checkout to its pinned
;; revision and needs the network, `build' compiles whatever revision is on
;; disk and works offline.  That split is what makes the setup reproducible,
;; and it is what lets a grammar be patched by hand and rebuilt -- `build'
;; never resets a checkout, only `reset' does.
;;
;; Most pins point at the newest release tag.  A few deliberately do not, and
;; `moyue treesit update --write' would move all of them to the newest tag, so
;; read its diff before accepting it:
;;
;;   * the repository never cut a release tag (make, sql, zig);
;;   * the newest tag predates the committed generated parser, so only a
;;     later commit can be compiled at all (elisp);
;;   * the newest tag declares an ABI below the oldest Emacs can load (vue:
;;     v0.1.0 is ABI 10, while Emacs 31 wants 13-15);
;;   * the tag's directory layout differs from the one Emacs assumes (ocaml
;;     v0.26.0 puts its grammars under grammars/<lang>/src);
;;   * upstream changed its version numbering, so the numerically largest tag
;;     is not the newest code (lua: the abandoned Azganoth repository stops at
;;     v2.1.3 while the maintained fork is at v0.5.0, and Emacs 31 is written
;;     against the fork).
;;
;; doxygen, jsdoc, phpdoc and lua are additionally pinned to the exact commits
;; Emacs 31 names in its own recipes, because those are the revisions its
;; bundled queries were written against.  A grammar whose node types no longer
;; match those rules does not fail to load, it only loses font-lock features,
;; which is why `verify' checks the rules as well.
;;
;; A grammar is only useful if the mode consuming it can find its companions,
;; which is why the manifest also carries markdown-inline (markdown-ts-mode),
;; phpdoc (php-ts-mode), jsdoc (js-ts-mode) and doxygen (c/cpp/java ts-modes).

;;; Code:


(require 'cl-lib)
(require 'subr-x)

;; These two belong to `treesit.el', which this library only loads on demand
;; so that it keeps working on an Emacs built without tree-sitter support.
;; Declaring them here marks them special, so `let'-binding them -- which
;; `moyue-treesit-build' and the tests do -- binds them dynamically instead of
;; creating a lexical variable that `treesit.el' then refuses to redefine.
(defvar treesit-language-source-alist)
(defvar treesit-auto-install-grammar)
;; default directory, so the advice below has to rebind it dynamically.


(defvar moyue-treesit-directory-name "tree-sitter"
  "Name of the grammar directory inside the `.cache' directory.")


;;;; ── Pinned upstream revisions ────────────────────────────────────────────
;;
;; Each entry is (LANG URL :commit SHA :ref REF :source-dir DIR), where
;;   :commit     the exact revision that is checked out and compiled,
;;   :ref        the release tag the commit came from, for human eyes only,
;;   :source-dir the subdirectory of URL that holds the generated parser.c.
;;
;; This is the only place a revision is decided.  Regenerate with
;; `moyue treesit update --write', and re-check with `moyue treesit verify'.

(defconst moyue-treesit-manifest
  '(
    (bash "https://github.com/tree-sitter/tree-sitter-bash"
     :commit "a06c2e4415e9bc0346c6b86d401879ffb44058f7"
     :ref "v0.25.1"
     :source-dir "src")
    (c "https://github.com/tree-sitter/tree-sitter-c"
     :commit "b780e47fc780ddc8da13afa35a3f4ed5c157823d"
     :ref "v0.24.2"
     :source-dir "src")
    (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp"
     :commit "cac6d5fb595f5811a076336682d5d595ac1c9e85"
     :ref "v0.23.5"
     :source-dir "src")
    (cmake "https://github.com/uyha/tree-sitter-cmake"
     :commit "e997bd0b275ca525ce9befecedf5299031183661"
     :ref "v0.7.5"
     :source-dir "src")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp"
     :commit "f41e1a044c8a84ea9fa8577fdd2eab92ec96de02"
     :ref "v0.23.4"
     :source-dir "src")
    (css "https://github.com/tree-sitter/tree-sitter-css"
     :commit "dda5cfc5722c429eaba1c910ca32c2c0c5bb1a3f"
     :ref "v0.25.0"
     :source-dir "src")
    (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile"
     :commit "868e44ce378deb68aac902a9db68ff82d2299dd0"
     :ref "v0.2.0"
     :source-dir "src")
    (doxygen "https://github.com/tree-sitter-grammars/tree-sitter-doxygen"
     :commit "1e28054cb5be80d5febac082706225e42eff14e6"
     :ref "1e28054cb5be80d5febac082706225e42eff14e6"
     :source-dir "src")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp"
     :commit "0cbf0906d9ee707c8c109422fba9cdd17ae13dcf"
     :ref "main"
     :source-dir "src")
    (go "https://github.com/tree-sitter/tree-sitter-go"
     :commit "1547678a9da59885853f5f5cc8a99cc203fa2e2c"
     :ref "v0.25.0"
     :source-dir "src")
    (gomod "https://github.com/camdencheek/tree-sitter-go-mod"
     :commit "3b01edce2b9ea6766ca19328d1850e456fde3103"
     :ref "v1.1.0"
     :source-dir "src")
    (html "https://github.com/tree-sitter/tree-sitter-html"
     :commit "5a5ca8551a179998360b4a4ca2c0f366a35acc03"
     :ref "v0.23.2"
     :source-dir "src")
    (java "https://github.com/tree-sitter/tree-sitter-java"
     :commit "94703d5a6bed02b98e438d7cad1136c01a60ba2c"
     :ref "v0.23.5"
     :source-dir "src")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
     :commit "44c892e0be055ac465d5eeddae6d3e194424e7de"
     :ref "v0.25.0"
     :source-dir "src")
    (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc"
     :commit "b253abf68a73217b7a52c0ec254f4b6a7bb86665"
     :ref "b253abf68a73217b7a52c0ec254f4b6a7bb86665"
     :source-dir "src")
    (json "https://github.com/tree-sitter/tree-sitter-json"
     :commit "ee35a6ebefcef0c5c416c0d1ccec7370cfca5a24"
     :ref "v0.24.8"
     :source-dir "src")
    ;; The maintained fork, and the revision Emacs 31's `lua-ts-mode' names
    ;; in its own recipe (v0.3.0-1-gdb16e76).  The old Azganoth repository
    ;; still carries v2.1.3, whose number sorts higher but whose content is
    ;; far older (ABI 13) and whose node types no longer match the bundled
    ;; font-lock rules.
    (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua"
     :commit "db16e76558122e834ee214c8dc755b4a3edc82a9"
     :ref "v0.3.0-1-gdb16e76"
     :source-dir "src")
    (make "https://github.com/alemuller/tree-sitter-make"
     :commit "a4b9187417d6be349ee5fd4b6e77b4172c6827dd"
     :ref "a4b9187417d6be349ee5fd4b6e77b4172c6827dd"
     :source-dir "src")
    (markdown "https://github.com/MDeiml/tree-sitter-markdown"
     :commit "f969cd3ae3f9fbd4e43205431d0ae286014c05b5"
     :ref "v0.5.3"
     :source-dir "tree-sitter-markdown/src")
    (markdown-inline "https://github.com/MDeiml/tree-sitter-markdown"
     :commit "f969cd3ae3f9fbd4e43205431d0ae286014c05b5"
     :ref "v0.5.3"
     :source-dir "tree-sitter-markdown-inline/src")
    (ocaml "https://github.com/tree-sitter/tree-sitter-ocaml"
     :commit "e3c9cf368f68bffd2f81188229aefa7b434eda65"
     :ref "v0.26.0"
     :source-dir "grammars/ocaml/src")
    (org "https://github.com/milisims/tree-sitter-org"
     :commit "698bb1a34331e68f83fc24bdd1b6f97016bb30de"
     :ref "v1.3.1"
     :source-dir "src")
    (php "https://github.com/tree-sitter/tree-sitter-php"
     :commit "92b5271b60bec77fb65b5e5bc41561e8dac81299"
     :ref "v0.25.0"
     :source-dir "php/src")
    (phpdoc "https://github.com/claytonrcarter/tree-sitter-phpdoc"
     :commit "03bb10330704b0b371b044e937d5cc7cd40b4999"
     :ref "03bb10330704b0b371b044e937d5cc7cd40b4999"
     :source-dir "src")
    (python "https://github.com/tree-sitter/tree-sitter-python"
     :commit "293fdc02038ee2bf0e2e206711b69c90ac0d413f"
     :ref "v0.25.0"
     :source-dir "src")
    (ruby "https://github.com/tree-sitter/tree-sitter-ruby"
     :commit "71bd32fb7607035768799732addba884a37a6210"
     :ref "v0.23.1"
     :source-dir "src")
    (rust "https://github.com/tree-sitter/tree-sitter-rust"
     :commit "77a3747266f4d621d0757825e6b11edcbf991ca5"
     :ref "v0.24.2"
     :source-dir "src")
    (sql "https://github.com/m-novikov/tree-sitter-sql"
     :commit "587f30d184b058450be2a2330878210c5f33b3f9"
     :ref "587f30d184b058450be2a2330878210c5f33b3f9"
     :source-dir "src")
    (toml "https://github.com/tree-sitter/tree-sitter-toml"
     :commit "474fbbec27e27d76b45aeaf9191e8acb13a699e2"
     :ref "v0.5.1"
     :source-dir "src")
    (tsx "https://github.com/tree-sitter/tree-sitter-typescript"
     :commit "f975a621f4e7f532fe322e13c4f79495e0a7b2e7"
     :ref "v0.23.2"
     :source-dir "tsx/src")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
     :commit "f975a621f4e7f532fe322e13c4f79495e0a7b2e7"
     :ref "v0.23.2"
     :source-dir "typescript/src")
    (vue "https://github.com/merico-dev/tree-sitter-vue"
     :commit "ebb1980eb45e7bbec4d00ef0bb7223091f148f49"
     :ref "master"
     :source-dir "src")
    (yaml "https://github.com/ikatyang/tree-sitter-yaml"
     :commit "6129a83eeec7d6070b1c0567ec7ce3509ead607c"
     :ref "v0.5.0"
     :source-dir "src")
    (zig "https://github.com/GrayJack/tree-sitter-zig"
     :commit "8e970cb01c22cf89b8f70231ed4eca8d6fad1d56"
     :ref "8e970cb01c22cf89b8f70231ed4eca8d6fad1d56"
     :source-dir "src")))

;;;; ── Locations ────────────────────────────────────────────────────────────

(defun moyue-treesit-cache-directory ()
  "Return the directory holding the compiled grammar libraries.
The path is derived from `user-emacs-directory' at call time so that
batch commands, which bind that variable, resolve the same directory as
the running Emacs."
  (file-name-as-directory
   (expand-file-name moyue-treesit-directory-name
                     (expand-file-name ".cache/" user-emacs-directory))))

(defun moyue-treesit-source-directory (&optional lang)
  "Return the root directory of the pinned upstream checkouts.
With LANG, return the checkout directory for that language instead."
  (let ((base (file-name-as-directory
               (expand-file-name "src" (moyue-treesit-cache-directory)))))
    (if lang
        (file-name-as-directory (expand-file-name (symbol-name lang) base))
      base)))

(defun moyue-treesit-grammar-file (lang &optional directory)
  "Return the path of the compiled library for LANG.
DIRECTORY overrides the default grammar cache directory."
  (expand-file-name
   (concat "libtree-sitter-" (symbol-name lang)
           (or (car dynamic-library-suffixes) ".so"))
   (file-name-as-directory (or directory (moyue-treesit-cache-directory)))))

;;;; ── Manifest access ──────────────────────────────────────────────────────

(defun moyue-treesit-entry (lang)
  "Return the manifest entry for LANG.
Signals an error when LANG was never pinned."
  (or (assq lang moyue-treesit-manifest)
      (error "No pinned tree-sitter grammar for `%s'" lang)))

(defun moyue-treesit-languages ()
  "Return the pinned language symbols, in manifest order."
  (mapcar #'car moyue-treesit-manifest))

(defun moyue-treesit--field (lang field)
  "Return FIELD of the manifest entry pinned for LANG."
  (plist-get (cddr (moyue-treesit-entry lang)) field))

(defun moyue-treesit--parser-file (lang)
  "Return the path of LANG's generated parser.c inside its checkout."
  (expand-file-name
   "parser.c"
   (expand-file-name (or (moyue-treesit--field lang :source-dir) "src")
                     (moyue-treesit-source-directory lang))))

(defun moyue-treesit-sourced-p (lang)
  "Return non-nil when LANG's pinned checkout holds a generated parser."
  (file-exists-p (moyue-treesit--parser-file lang)))

(defun moyue-treesit-installed-p (lang)
  "Return non-nil when LANG's compiled library is present in the cache."
  (file-exists-p (moyue-treesit-grammar-file lang)))

(defun moyue-treesit-declared-abi (lang)
  "Return the ABI version declared by LANG's pinned generated parser.
This is the LANGUAGE_VERSION the parser was generated against, read from
the checked-out source rather than from a loaded library, so comparing it
with `treesit-language-abi-version' tells whether the grammar Emacs
actually loaded came from the cache or from a fallback location.  Returns
nil when LANG has no source checkout."
  (let ((parser (moyue-treesit--parser-file lang)))
    (when (file-readable-p parser)
      (with-temp-buffer
        ;; LANGUAGE_VERSION sits in the header block, so reading a slice
        ;; keeps this cheap even for multi-megabyte generated parsers.
        (insert-file-contents parser nil 0 65536)
        (goto-char (point-min))
        (when (re-search-forward "^#define LANGUAGE_VERSION[ \t]+\\([0-9]+\\)" nil t)
          (string-to-number (match-string 1)))))))

(defun moyue-treesit-installed-languages ()
  "Return the pinned languages whose library is present in the cache."
  (seq-filter #'moyue-treesit-installed-p (moyue-treesit-languages)))

(defun moyue-treesit-missing-languages ()
  "Return the pinned languages whose library is missing from the cache."
  (seq-remove #'moyue-treesit-installed-p (moyue-treesit-languages)))

;;;; ── Runtime wiring ───────────────────────────────────────────────────────

(defun moyue-treesit-recipe (lang)
  "Return the `treesit-language-source-alist' recipe that builds LANG.
The recipe points at the local checkout, so Emacs compiles the revision
on disk and never clones, checks out or deletes anything."
  (list lang (moyue-treesit-source-directory lang)
        :source-dir (or (moyue-treesit--field lang :source-dir) "src")))

(defun moyue-treesit-language-source-alist ()
  "Return recipes for every pinned grammar, all rooted in the cache."
  (mapcar #'moyue-treesit-recipe (moyue-treesit-languages)))



;;;; ── Git plumbing ─────────────────────────────────────────────────────────

(defun moyue-treesit--git (&rest args)
  "Run git with ARGS, signalling an error on a non-zero exit status."
  (unless (executable-find "git")
    (error "git is not available in `exec-path'"))
  (with-temp-buffer
    (let ((code (apply #'call-process "git" nil t nil args)))
      (unless (eq code 0)
        (error "git %s failed:\n%s"
               (string-join args " ")
               (string-trim (buffer-string)))))))

(defun moyue-treesit--git-output (&rest args)
  "Return the non-empty standard output lines of git ARGS.
Standard error is discarded on purpose.  `call-process' mixes it into the
destination by default, and git does write warnings to it -- a CRLF
conversion notice, for instance -- which would then be parsed as data: a
pathspec would grow a bogus entry, and `git status --porcelain' would look
non-empty and make a clean checkout count as hand-edited."
  (with-temp-buffer
    (apply #'call-process "git" nil (list t nil) nil args)
    (split-string (string-trim (buffer-string)) "\n" t)))

(defun moyue-treesit--dirty-p (dir)
  "Return non-nil when DIR has staged or unstaged changes to tracked files.
Untracked files are ignored, so the object files left behind by a build
never count as local edits."
  (and (moyue-treesit--git-output "-C" dir "status" "--porcelain"
                                  "--untracked-files=no")
       t))

(defun moyue-treesit-fetch (lang &optional force)
  "Materialise the upstream revision pinned for LANG below the cache.
A language without a checkout is cloned blobless and detached at its pin.
An existing checkout carrying local edits is left untouched unless FORCE
is non-nil, so hand edits survive a re-fetch.  Returns the checkout
directory."
  (let* ((entry (moyue-treesit-entry lang))
         (url (nth 1 entry))
         (commit (moyue-treesit--field lang :commit))
         (dir (moyue-treesit-source-directory lang)))
    (unless (and (stringp commit) (not (string-empty-p commit)))
      (error "Manifest entry for `%s' has no :commit" lang))
    (cond
     ((not (file-directory-p (expand-file-name ".git" dir)))
      (when (file-exists-p dir) (delete-directory dir t))
      (make-directory (moyue-treesit-source-directory) t)
      (moyue-treesit--git "clone" "--filter=blob:none" "--no-checkout"
                         "--quiet" url (directory-file-name dir))
      (moyue-treesit--git "-C" (directory-file-name dir)
                         "checkout" "--quiet" "--detach" commit))
     ((and (moyue-treesit--dirty-p dir) (not force))
      (error "`%s' has local changes in %s; commit them, or use \
`moyue treesit reset %s' to discard them" lang dir lang))
     (t
      (moyue-treesit--git "-C" (directory-file-name dir) "fetch" "--quiet" "origin")
      (moyue-treesit--git "-C" (directory-file-name dir)
                         "checkout" "--quiet" "--detach" commit)))
    dir))

(defun moyue-treesit-reset (lang)
  "Discard local edits in LANG's checkout and restore its pinned commit."
  (let ((dir (moyue-treesit-source-directory lang)))
    (unless (file-directory-p (expand-file-name ".git" dir))
      (error "`%s' has no checkout in %s; run fetch first" lang dir))
    (moyue-treesit--git "-C" (directory-file-name dir)
                       "checkout" "--force" "--detach"
                       (moyue-treesit--field lang :commit))
    dir))

;;;; ── Building ─────────────────────────────────────────────────────────────

(defun moyue-treesit--restore-build-artifacts (lang)
  "Restore tracked build artifacts that compiling LANG overwrote.
A build writes `*.o' beside the sources.  Those are untracked in almost every
grammar, but one upstream (tree-sitter-zig) commits a generated
`src/parser.o', so compiling in place would leave that checkout looking
hand-edited for ever and would later stop it from moving to another revision.
Putting the tracked artifact back keeps `git status' honest about whether
there are edits worth preserving."
  (let* ((dir (directory-file-name (moyue-treesit-source-directory lang)))
         (paths (moyue-treesit--git-output "-C" dir "diff" "--name-only"
                                           "--" "*.o" "libtree-sitter-*.so")))
    (when paths
      (apply #'moyue-treesit--git "-C" dir "checkout" "--" paths)
      (message "treesit: restored %d tracked build artifact(s) in %s"
               (length paths) lang))))

(defun moyue-treesit-build (lang &optional out-dir)
  "Compile LANG from its local checkout into the grammar cache.
Signals an error when the checkout has no generated parser, which means
`moyue-treesit-fetch' has not run for LANG yet.  OUT-DIR overrides the
cache directory.  Returns the path of the compiled library."
  (require 'treesit)
  (let* ((source (moyue-treesit-source-directory lang))
         (parser (moyue-treesit--parser-file lang))
         (out (file-name-as-directory
               (expand-file-name (or out-dir (moyue-treesit-cache-directory)))))
         ;; Let the installer's own post-build check find the library we
         ;; are about to write, rather than an older copy somewhere else.
         (treesit-extra-load-path (cons out treesit-extra-load-path))
         (treesit-language-source-alist (moyue-treesit-language-source-alist)))
    (unless (file-exists-p parser)
      (error "No parser.c for `%s' below %s; run fetch first" lang source))
    (unless (file-directory-p out) (make-directory out t))
    (treesit-install-language-grammar lang out)
    (let ((lib (moyue-treesit-grammar-file lang out)))
      (unless (file-exists-p lib)
        (error "Building `%s' reported success but %s is missing" lang lib))
      (ignore-errors (moyue-treesit--restore-build-artifacts lang))
      lib)))

;;;; ── Reporting ────────────────────────────────────────────────────────────

(defun moyue-treesit-status (&optional langs)
  "Return one plist per language in LANGS describing its pinned state.
LANGS defaults to every pinned language."
  (mapcar (lambda (lang)
            (list :lang lang
                  :ref (moyue-treesit--field lang :ref)
                  :commit (moyue-treesit--field lang :commit)
                  :source (moyue-treesit-sourced-p lang)
                  :library (moyue-treesit-installed-p lang)))
          (or langs (moyue-treesit-languages))))

(defun moyue-treesit--short-ref (ref)
  "Return REF for display, shortened when it is a raw commit hash.
Repositories pinned to their default branch carry the commit itself as
the ref, which is too wide for the status table."
  (cond ((null ref) "-")
        ((string-match-p "\\`[0-9a-f]\\{40\\}\\'" ref) (substring ref 0 9))
        (t ref)))

(defun moyue-treesit-status-string (&optional langs)
  "Return a human readable report of the pinned grammars in LANGS."
  (mapconcat
   (lambda (row)
     (let ((commit (plist-get row :commit)))
       (format "%-15s %-18s %-10s %s"
               (plist-get row :lang)
               (moyue-treesit--short-ref (plist-get row :ref))
               (if (stringp commit) (substring commit 0 9) "-")
               (cond ((and (plist-get row :source) (plist-get row :library)) "built")
                     ((plist-get row :source) "source only (run build)")
                     ((plist-get row :library) "library only (no source)")
                     (t "missing (run fetch + build)")))))
   (moyue-treesit-status langs)
   "\n"))

;;;; ── Deliberate upgrades ───────────────────────────────────────────────────

(defun moyue-treesit--ls-remote (url &rest options)
  "Return the non-empty lines of `git ls-remote OPTIONS... URL'.
OPTIONS must come before URL: `ls-remote' reads everything after the
repository as a ref pattern, so `git ls-remote URL --tags' asks for a ref
literally named \"--tags\", succeeds, and prints nothing -- a silent
downgrade to a default-branch lookup rather than an error."
  (apply #'moyue-treesit--git-output
         (append (list "ls-remote") options (list url))))

(defun moyue-treesit--ls-remote-ref (url ref)
  "Return the object id of REF in URL, or nil when REF does not exist.
Here the ref goes after the repository, which is where `ls-remote' expects
a ref pattern."
  (car (moyue-treesit--git-output "ls-remote" url ref)))

(defun moyue-treesit-resolve-upstream (url)
  "Return (REF . COMMIT) for the newest release tag published at URL.
Annotated tags are peeled to the commit they point at, so COMMIT is always
a commit and never a tag object.  Repositories without a plain
semantic-version tag fall back to their default branch head, in which case
REF is the commit itself."
  (let ((tags (make-hash-table :test #'equal))
        (peeled (make-hash-table :test #'equal))
        best best-version)
    (dolist (line (moyue-treesit--ls-remote url "--tags"))
      (when (string-match "\\([0-9a-f]+\\)\trefs/tags/\\(.*\\)\\'" line)
        (let ((sha (match-string 1 line))
              (name (match-string 2 line)))
          (if (string-suffix-p "^{}" name)
              (puthash (substring name 0 -3) sha peeled)
            (puthash name sha tags)))))
    (maphash (lambda (name _sha)
               (when (string-match "\\`v?\\([0-9]+\\(?:\\.[0-9]+\\)*\\)\\'" name)
                 (let ((version (match-string 1 name)))
                   (when (or (null best-version) (version< best-version version))
                     (setq best name best-version version)))))
             tags)
    (if best
        (cons best (or (gethash best peeled) (gethash best tags)))
      (let ((head (moyue-treesit--ls-remote-ref url "HEAD")))
        (unless head
          (error "Neither a release tag nor HEAD could be resolved for %s" url))
        (cons head head)))))

(defun moyue-treesit-update (&optional langs)
  "Resolve the newest upstream revision for LANGS.
Returns an alist of (LANG OLD-COMMIT NEW-REF . NEW-COMMIT).  Nothing is
written and no checkout is touched; use `moyue-treesit-write-manifest'
with the result to make the move permanent."
  (mapcar
   (lambda (lang)
     (let* ((entry (moyue-treesit-entry lang))
            (resolved (moyue-treesit-resolve-upstream (nth 1 entry))))
       (list lang
             (moyue-treesit--field lang :commit)
             (car resolved)
             (cdr resolved))))
   (or langs (moyue-treesit-languages))))


(defun moyue-treesit--manifest-file-path ()
  "Return the file holding the pinned manifest, which is this one."
  (expand-file-name "commands/treesit.el" moyue--bin-dir))


(defun moyue-treesit--manifest-region ()
  "Return (START . END) of the `moyue-treesit-manifest' form in the library.
The file is scanned form by form rather than with a text search, so that
the `defconst' is found even though the same text also occurs inside the
string literal built by `moyue-treesit--manifest-form'."
  (let ((file (moyue-treesit--manifest-file-path)))
    (unless (file-readable-p file)
      (error "Cannot read the manifest file %s" file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let (region)
        (condition-case nil
            (while (not region)
              ;; Move past whitespace and comments first, otherwise the
              ;; region starts on the previous form's line ending and
              ;; replacing it would join two top-level forms.
              (forward-comment (point-max))
              (let ((beg (point))
                    (form (read (current-buffer))))
                (when (and (eq (car-safe form) 'defconst)
                           (eq (cadr form) 'moyue-treesit-manifest))
                  (setq region (cons beg (point))))))
          (end-of-file nil)
          (invalid-read-syntax nil))
        region))))

(defun moyue-treesit--manifest-form (manifest)
  "Return the `defconst' form holding MANIFEST as a string."
  (with-temp-buffer
    (insert "(defconst moyue-treesit-manifest\n  '(")
    (dolist (entry manifest)
      (insert (format "\n    (%s %S" (car entry) (nth 1 entry)))
      (let ((plist (cddr entry)))
        (while plist
          (insert (format "\n     %s %S" (car plist) (cadr plist)))
          (setq plist (cddr plist))))
      (insert ")"))
    (insert "))\n")
    (buffer-string)))

(defun moyue-treesit-write-manifest (manifest)
  "Replace the pinned manifest in the library with MANIFEST.
MANIFEST uses the same shape as `moyue-treesit-manifest'.  The file is
only rewritten when the replacement form reads back as valid Lisp."
  (let* ((file (moyue-treesit--manifest-file-path))
         (region (moyue-treesit--manifest-region))
         (form (moyue-treesit--manifest-form manifest)))
    (unless region
      (error "No `moyue-treesit-manifest' definition found in %s" file))
    ;; Refuse to write anything that does not read back as a single form.
    (with-temp-buffer
      (insert form)
      (goto-char (point-min))
      (read (current-buffer))
      (skip-chars-forward " \t\r\n")
      (unless (eobp)
        (error "Refusing to write a manifest that does not read as one form")))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (cdr region))
      (delete-region (car region) (cdr region))
      (insert (string-trim-right form))
      (write-region (point-min) (point-max) file nil 'silent))
    file))


;;;; ── Command line ───────────────────────────────────────────────────────

(defun moyue-treesit--select (args)
  "Split ARGS into the languages and flags it names.
Returns (LANGS . FLAGS).  An empty selection, or the word \"all\", means
every pinned language."
  (let (langs flags)
    (dolist (arg args)
      (cond ((string-prefix-p "-" arg) (push arg flags))
            ((member arg '("all" "")) nil)
            (t (push (intern arg) langs))))
    (setq langs (nreverse langs))
    (dolist (lang langs)
      (moyue-treesit-entry lang))
    (cons (or langs (moyue-treesit-languages)) (nreverse flags))))

(defun moyue-treesit--report (lang status)
  "Print one line for LANG described by STATUS."
  (message "  %-11s %s" lang status))

(defun moyue-treesit--status (args)
  "Implement `moyue treesit status' with ARGS."
  (pcase-let* ((`(,langs . ,_flags) (moyue-treesit--select args)))
    (message "tree-sitter grammars in %s" (moyue-treesit-cache-directory))
    (message "%s" (moyue-treesit-status-string langs))
    (let ((missing (seq-remove #'moyue-treesit-installed-p langs)))
      (message "%d/%d built%s"
               (- (length langs) (length missing))
               (length langs)
               (if missing
                   (format "; run `moyue treesit sync %s'"
                           (mapconcat #'symbol-name missing " "))
                 "")))))

(defun moyue-treesit--fetch (args &optional build)
  "Implement `moyue treesit fetch' (and, with BUILD, `sync') for ARGS."
  (pcase-let* ((`(,langs . ,flags) (moyue-treesit--select args))
               (force (member "--force" flags)))
    (dolist (lang langs)
      (condition-case err
          (progn
            (message "fetch %s ..." lang)
            (moyue-treesit-fetch lang force)
            (when build
              (message "build %s ..." lang)
              (moyue-treesit-build lang))
            (moyue-treesit--report lang (if build "synced" "fetched")))
        (error (moyue-treesit--report lang (format "FAILED: %s"
                                                   (error-message-string err))))))))

(defun moyue-treesit--build (args)
  "Implement `moyue treesit build' with ARGS."
  (pcase-let* ((`(,langs . ,_flags) (moyue-treesit--select args)))
    (dolist (lang langs)
      (condition-case err
          (progn
            (message "build %s ..." lang)
            (moyue-treesit-build lang)
            (moyue-treesit--report lang (moyue-treesit-grammar-file lang)))
        (error (moyue-treesit--report lang (format "FAILED: %s"
                                                   (error-message-string err))))))))

(defun moyue-treesit--reset (args)
  "Implement `moyue treesit reset' with ARGS."
  (pcase-let* ((`(,langs . ,_flags) (moyue-treesit--select args)))
    (dolist (lang langs)
      (condition-case err
          (progn
            (moyue-treesit-reset lang)
            (moyue-treesit--report lang "restored the pinned revision"))
        (error (moyue-treesit--report lang (format "FAILED: %s"
                                                   (error-message-string err))))))))

(defun moyue-treesit--update (args)
  "Implement `moyue treesit update' with ARGS.
Resolution is read-only unless ARGS carries --write, so a new upstream
release never becomes the compiled revision by accident."
  (pcase-let* ((`(,langs . ,flags) (moyue-treesit--select args))
               (write (member "--write" flags))
               (resolved (moyue-treesit-update langs))
               (manifest (copy-tree moyue-treesit-manifest))
               (changed 0))
    (dolist (row resolved)
      (pcase-let* ((`(,lang ,old ,ref ,new) row))
        (if (equal old new)
            (moyue-treesit--report lang (format "up to date (%s)" ref))
          (setq changed (1+ changed))
          (moyue-treesit--report
           lang (format "%s -> %s  %s" (substring old 0 9) (substring new 0 9) ref))
          (let ((entry (assq lang manifest)))
            ;; Replace the whole property list: `(cdr entry)' is the
            ;; (URL . PLIST) cons, so setting its cdr swaps the plist out.
            (setcdr (cdr entry)
                    (list :commit new :ref ref
                          :source-dir (plist-get (cddr entry) :source-dir)))))))
    (if (zerop changed)
        (message "nothing to update")
      (if write
          (progn
            (moyue-treesit-write-manifest manifest)
            (message "updated %d pin%s in %s; run `moyue treesit sync' to rebuild"
                     changed (if (= changed 1) "" "s")
                     (moyue-treesit--manifest-file-path)))
        (message "%d pin%s would change; re-run with --write to apply"
                 changed (if (= changed 1) "" "s"))))))

(defun moyue-treesit-on-pin-p (lang)
  "Return non-nil when LANG's checkout is clean and on its pin."
  (let* ((dir (directory-file-name (moyue-treesit-source-directory lang)))
         (pin (moyue-treesit--field lang :commit)))
    (and (file-directory-p (expand-file-name ".git" dir))
         (not (moyue-treesit--dirty-p dir))
         (equal pin (car (moyue-treesit--git-output
                          "-C" dir "rev-parse" "HEAD"))))))

(defun moyue-treesit-install-grammars ()
  "Materialise and build every pinned grammar for `moyue install'.
A checkout that already sits cleanly on its pin is left where it is, and one
that carries local changes is skipped rather than reset: being able to patch
a grammar and rebuild it is the reason the sources are kept at all, and an
install must not throw that away.  Returns an alist of (LANG . MESSAGE) for
the grammars that failed, which the caller reports."
  (let (failed)
    (dolist (lang (moyue-treesit-languages))
      (condition-case err
          (progn
            (cond
             ((moyue-treesit-on-pin-p lang) nil)
             ((moyue-treesit--dirty-p (moyue-treesit-source-directory lang))
              (message "treesit: %s has local changes, building it as-is" lang))
             (t (moyue-treesit-fetch lang)))
            (moyue-treesit-build lang))
        (error (push (cons lang (error-message-string err)) failed))))
    (nreverse failed)))
(defconst moyue-treesit--mode-check
  '((c                . (c-ts-mode            . "int main(void) { return 0; }\n"))
    (cpp              . (c++-ts-mode          . "int main() { return 0; }\n"))
    (python           . (python-ts-mode       . "def f(x):\n    return x\n"))
    (bash             . (bash-ts-mode         . "echo hi\n"))
    (json             . (json-ts-mode         . "{\"a\": 1}\n"))
    (yaml             . (yaml-ts-mode         . "a: 1\n"))
    (css              . (css-ts-mode          . "a { color: red; }\n"))
    (cmake            . (cmake-ts-mode        . "project(x)\n"))
    (dockerfile       . (dockerfile-ts-mode   . "FROM alpine\n"))
    (rust             . (rust-ts-mode         . "fn main() {}\n"))
    (go               . (go-ts-mode           . "package main\n"))
    (gomod            . (go-mod-ts-mode       . "module x\n"))
    (java             . (java-ts-mode         . "class A {}\n"))
    (ruby             . (ruby-ts-mode         . "def f; end\n"))
    (php              . (php-ts-mode          . "<?php\n"))
    (toml             . (toml-ts-mode         . "k = 1\n"))
    (lua              . (lua-ts-mode          . "local x = 1\n"))
    (html             . (html-ts-mode         . "<p>hi</p>\n"))
    (javascript       . (js-ts-mode           . "var x = 1;\n"))
    (typescript       . (typescript-ts-mode   . "var x: number = 1;\n"))
    (tsx              . (tsx-ts-mode          . "const A = () => <p/>;\n"))
    (markdown         . (markdown-ts-mode     . "# T\n"))
    (markdown-inline  . (markdown-ts-mode     . "# T\n")))
  "Grammar to (MODE . SAMPLE) for the ts-modes bundled with Emacs.
Used by `moyue treesit verify' to prove that the queries Emacs ships still
compile against the pinned revision of each grammar.")

(defun moyue-treesit--grammar-problems (lang)
  "Return a list of health problems for LANG, empty when it looks good.
Covers provenance (the checkout is on its pin), presence, loadability and
the loaded ABI agreeing with the ABI the pinned source declares -- the last
one is what detects a silent fallback to a distribution package."
  (let* ((dir (directory-file-name (moyue-treesit-source-directory lang)))
         (pin (moyue-treesit--field lang :commit))
         (head (or (car (moyue-treesit--git-output "-C" dir "rev-parse" "HEAD"))
                   ""))
         (declared (moyue-treesit-declared-abi lang))
         (loaded (treesit-language-available-p lang t))
         (abi (and (car loaded) (ignore-errors (treesit-language-abi-version lang))))
         problems)
    (unless (equal pin head)
      (push (format "checkout is %s, not the pinned %s"
                    (if (string-empty-p head) "(none)" (substring head 0 9))
                    (substring pin 0 9))
            problems))
    (unless (moyue-treesit-installed-p lang)
      (push "no compiled library in the cache" problems))
    (unless declared
      (push "cannot read LANGUAGE_VERSION from the pinned source" problems))
    (unless (car loaded)
      (push (format "Emacs cannot load it (%s)" (cdr loaded)) problems))
    (when (and declared abi (/= abi declared))
      (push (format "loaded abi=%s but the pin declares %s" abi declared) problems))
    (nreverse problems)))

(defun moyue-treesit--mode-problems (lang)
  "Return font-lock rule problems for LANG, empty when the queries compile.
A grammar whose node types no longer match the rules bundled with Emacs
still loads, it only loses font-lock features behind a warning, so the
warning is collected and reported instead of being ignored."
  (when-let* ((entry (cdr (assq lang moyue-treesit--mode-check))))
    (let ((mode (car entry))
          (sample (cdr entry)))
      (unless (fboundp mode) (require mode nil t))
      (if (not (fboundp mode))
          (list (format "%s is not available" mode))
        (with-temp-buffer
          (insert sample)
          (let ((warnings nil))
            (cl-letf (((symbol-function 'display-warning)
                       (lambda (type message &optional _level _buffer)
                         (push (format "%s: %s" type message) warnings)
                         nil)))
              (funcall mode)
              (condition-case err
                  (font-lock-ensure)
                (error (push (format "font-lock signalled: %s"
                                     (error-message-string err))
                             warnings))))
            (mapcar (lambda (w) (car (split-string w "\n")))
                    (seq-filter (lambda (w)
                                  (string-match-p "rules-mismatch\\|mismatch between" w))
                                warnings))))))))

(defun moyue-treesit--verify (args)
  "Implement `moyue treesit verify' with ARGS.
Exits non-zero when any pinned grammar is unhealthy, so it can gate an
upgrade or a rebuild."
  (require 'treesit)
  (setq treesit-extra-load-path
        (list (moyue-treesit-cache-directory))
        treesit-auto-install-grammar nil)
  (pcase-let* ((`(,langs . ,_flags) (moyue-treesit--select args)))
    (let ((ok 0) (bad 0))
      (dolist (lang langs)
        (let ((problems (append (moyue-treesit--grammar-problems lang)
                                (moyue-treesit--mode-problems lang))))
          (if problems
              (progn
                (setq bad (1+ bad))
                (message "FAIL  %-16s" lang)
                (dolist (p problems) (message "        %s" p)))
            (setq ok (1+ ok))
            (message "ok    %-16s abi=%s %s" lang
                     (moyue-treesit-declared-abi lang)
                     (moyue-treesit--short-ref (moyue-treesit--field lang :ref))))))
      (message "\n%d healthy, %d unhealthy" ok bad)
      (when (> bad 0) (kill-emacs 1)))))

(defun moyue-treesit--help ()
  "Print the usage and documentation of the `treesit' command."
  (let ((cmd (gethash "treesit" moyue--commands)))
    (message "Usage: moyue treesit %s\n\n%s"
             (moyue-command-usage cmd) (moyue-command-doc cmd))))

(moyue-defcommand "treesit" "SUBCOMMAND [ARGS...]"
  "Maintain the pinned tree-sitter grammar libraries.

Grammar sources and compiled libraries live below .cache/tree-sitter/, and
every grammar is pinned to an exact upstream commit, so nothing here ever
follows an upstream default branch by itself.

Subcommands:
  status [LANG...]              pin, source and library state
  fetch  [LANG...|all] [--force]
                                check out the pinned revisions; --force
                                discards local edits in the checkout
  build  [LANG...|all]          compile from the local checkouts (offline)
  sync   [LANG...|all] [--force]
                                fetch, then build
  update [LANG...|all] [--write]
                                resolve newer upstream releases, and with
                                --write record them in the manifest
  verify [LANG...|all]          check pins against checkouts, loadability,
                                ABI agreement and the bundled font-lock
                                rules; exits non-zero when unhealthy
  reset  LANG...                discard local edits, restore the pin"
  (let ((sub (car args))
        (rest (cdr args))
        ;; Resolve the cache against the checkout this command was run
        ;; from, not against whatever HOME happens to point at.
        (user-emacs-directory (moyue--config-root)))
    (pcase sub
      ("status" (moyue-treesit--status rest))
      ("fetch"  (moyue-treesit--fetch rest))
      ("sync"   (moyue-treesit--fetch rest 'build))
      ("build"  (moyue-treesit--build rest))
      ("reset"  (moyue-treesit--reset rest))
      ("update" (moyue-treesit--update rest))
      ("verify" (moyue-treesit--verify rest))
      ((or "help" "--help" "-h" 'nil) (moyue-treesit--help))
      (_ (error "Unknown `treesit' subcommand '%s'; try `moyue help treesit'" sub)))))


(defun moyue-treesit--test-file-contents (file)
  "Return the whole contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun moyue-treesit--test-pinned-entry (file lang)
  "Return the manifest entry for LANG as written in FILE."
  (let* ((forms (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (let (acc done)
                    (while (not done)
                      (condition-case nil
                          (push (read (current-buffer)) acc)
                        (end-of-file (setq done t))))
                    (nreverse acc))))
         (const (seq-find (lambda (form)
                            (and (eq (car-safe form) 'defconst)
                                 (eq (cadr form) 'moyue-treesit-manifest)))
                          forms))
         ;; The form carries its value quoted: (defconst V '(ENTRY ...)).
         (value (caddr const))
         (entries (if (eq (car-safe value) 'quote) (cadr value) value)))
    (assq lang entries)))

(defmacro moyue-treesit--test-with-stub (file &rest body)
  "Run BODY against a scratch manifest in FILE.
The manifest path is redirected to FILE and every upstream lookup reports
NEW-COMMIT as the newest release, so no test touches the network or the
real manifest."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'moyue-treesit--manifest-file-path)
              (lambda () ,file))
             ((symbol-function 'moyue-treesit--ls-remote)
              (lambda (_url &rest _args)
                (list (concat moyue-treesit--test-new-commit
                              "\trefs/tags/v99.0.0"))))
             ((symbol-function 'moyue-treesit-languages)
              (lambda () '(c))))
     ,@body))

(defconst moyue-treesit--test-new-commit
  "2222222222222222222222222222222222222222"
  "Release commit the stubbed upstream lookup reports.")

(defun moyue-treesit--test-with-manifest (body)
  "Call BODY with a scratch copy of the manifest.
BODY receives the scratch file name; the file is removed afterwards."
  (let ((file (make-temp-file "treesit-manifest-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert (moyue-treesit--manifest-form moyue-treesit-manifest)))
          (funcall body file))
      (ignore-errors (delete-file file)))))


;;; treesit.el ends here


;;;; ── Tests ─────────────────────────────────────────────────────────────
;; Run by `moyue test', which loads this file through bin/moyue.el.

(with-eval-after-load 'ert

  (ert-deftest treesit/manifest-is-well-formed ()
    "Every pinned entry must carry a URL, a commit and a source directory."
    (should (consp moyue-treesit-manifest))
    (dolist (entry moyue-treesit-manifest)
      (let ((lang (car entry))
            (url (nth 1 entry)))
        (should (symbolp lang))
        (should (stringp url))
        (should (string-match-p "\\`https://" url))
        (should (string-match-p "\\`[0-9a-f]\\{40\\}\\'"
                                (or (plist-get (cddr entry) :commit) "")))
        (should (stringp (plist-get (cddr entry) :source-dir)))
        (should-not (string-prefix-p "/" (plist-get (cddr entry) :source-dir))))))

  (ert-deftest treesit/manifest-has-no-duplicates ()
    "A language must be pinned exactly once."
    (let ((langs (moyue-treesit-languages)))
      (should (= (length langs) (length (delete-dups (copy-sequence langs)))))))

  (ert-deftest treesit/cache-directory-lives-in-cache ()
    "Compiled libraries belong below `.cache/tree-sitter/'."
    (let ((user-emacs-directory "/tmp/moyu-treesit-test/"))
      (should (equal (moyue-treesit-cache-directory)
                     "/tmp/moyu-treesit-test/.cache/tree-sitter/"))
      (should (equal (moyue-treesit-source-directory)
                     "/tmp/moyu-treesit-test/.cache/tree-sitter/src/"))
      (should (equal (moyue-treesit-source-directory 'c)
                     "/tmp/moyu-treesit-test/.cache/tree-sitter/src/c/"))))

  (ert-deftest treesit/grammar-file-uses-emacs-suffix ()
    "The library name must match what Emacs itself looks for."
    (let* ((user-emacs-directory "/tmp/moyu-treesit-test/")
           (expected (concat "libtree-sitter-python"
                             (or (car dynamic-library-suffixes) ".so"))))
      (should (equal (file-name-nondirectory (moyue-treesit-grammar-file 'python))
                     expected))
      (should (equal (file-name-directory (moyue-treesit-grammar-file 'python))
                     (moyue-treesit-cache-directory)))))

  (ert-deftest treesit/entry-errors-for-unknown-language ()
    "Asking for a language that was never pinned is an error."
    (should-error (moyue-treesit-entry 'no-such-grammar)))

  (ert-deftest treesit/recipe-targets-local-checkout ()
    "A recipe must build from the cache checkout, never from a URL."
    (let* ((user-emacs-directory "/tmp/moyu-treesit-test/")
           (lang (car (moyue-treesit-languages)))
           (recipe (moyue-treesit-recipe lang)))
      (should (eq (car recipe) lang))
      (should (file-name-absolute-p (nth 1 recipe)))
      (should (string-prefix-p (moyue-treesit-source-directory) (nth 1 recipe)))
      (should (string-suffix-p (format "%s/" (symbol-name lang)) (nth 1 recipe)))
      (should (stringp (plist-get (cddr recipe) :source-dir)))))

  (ert-deftest treesit/source-alist-covers-manifest ()
    "Every pinned language must appear in the generated source alist."
    (let ((user-emacs-directory "/tmp/moyu-treesit-test/"))
      (should (equal (mapcar #'car (moyue-treesit-language-source-alist))
                     (moyue-treesit-languages)))))

  (ert-deftest treesit/missing-languages-complement-installed ()
    "Installed and missing languages must partition the manifest."
    (let ((user-emacs-directory "/tmp/moyu-treesit-test/"))
      (should (equal (sort (append (moyue-treesit-installed-languages)
                                   (moyue-treesit-missing-languages))
                           #'string<)
                     (sort (copy-sequence (moyue-treesit-languages)) #'string<)))))

  
  
  (ert-deftest treesit/manifest-form-round-trips ()
    "A generated manifest form must read back to the same manifest."
    (let* ((sample '((c "https://example.com/c"
                        :commit "0123456789abcdef0123456789abcdef01234567"
                        :ref "v1.2.3"
                        :source-dir "src")))
           (form (moyue-treesit--manifest-form sample))
           (read-back (with-temp-buffer
                        (insert form)
                        (goto-char (point-min))
                        (read (current-buffer)))))
      (should (equal read-back
                     `(defconst moyue-treesit-manifest ',sample)))))

  (ert-deftest treesit/resolve-upstream-prefers-newest-tag ()
    "Version ordering must be numeric, not lexicographic."
    (cl-letf (((symbol-function 'moyue-treesit--ls-remote)
               (lambda (_url &rest _args)
                 '("aaaa\trefs/tags/v0.9.0"
                   "bbbb\trefs/tags/v0.10.0"
                   "cccc\trefs/tags/v0.10.1"))))
      (should (equal (moyue-treesit-resolve-upstream "https://example.com/x")
                     '("v0.10.1" . "cccc")))))

  (ert-deftest treesit/resolve-upstream-peels-annotated-tags ()
    "An annotated tag must resolve to its commit, not to the tag object.
`git ls-remote --tags' lists both `refs/tags/X' and the peeled
`refs/tags/X^{}'; picking the former would pin a tag object."
    (cl-letf (((symbol-function 'moyue-treesit--ls-remote)
               (lambda (_url &rest _args)
                 '("1111111111111111111111111111111111111111\trefs/tags/v1.0.0"
                   "2222222222222222222222222222222222222222\trefs/tags/v1.0.0^{}"
                   "3333333333333333333333333333333333333333\trefs/tags/v2.0.0"
                   "4444444444444444444444444444444444444444\trefs/tags/v2.0.0^{}"))))
      (should (equal (moyue-treesit-resolve-upstream "https://example.com/x")
                     '("v2.0.0" . "4444444444444444444444444444444444444444")))))

  (ert-deftest treesit/resolve-upstream-keeps-lightweight-tags ()
    "A lightweight tag has no peeled entry and must be used as it is."
    (cl-letf (((symbol-function 'moyue-treesit--ls-remote)
               (lambda (_url &rest _args)
                 '("aaaa\trefs/tags/v1.0.0"))))
      (should (equal (moyue-treesit-resolve-upstream "https://example.com/x")
                     '("v1.0.0" . "aaaa")))))

  (ert-deftest treesit/resolve-upstream-falls-back-to-head ()
    "A repository without release tags pins its default branch head."
    (cl-letf (((symbol-function 'moyue-treesit--ls-remote)
               (lambda (_url &rest _args) '()))
              ((symbol-function 'moyue-treesit--ls-remote-ref)
               (lambda (_url _ref) "deadbeef")))
      (should (equal (moyue-treesit-resolve-upstream "https://example.com/x")
                     '("deadbeef" . "deadbeef")))))

  (ert-deftest treesit/git-output-discards-stderr ()
    "Standard error must not be parsed as git output.
`call-process' mixes stderr into its destination by default, and git does
write warnings there -- a CRLF conversion notice on this very checkout, for
instance.  Mixed in, that text becomes a bogus path in a pathspec, and it
makes `git status --porcelain' look non-empty so a clean checkout counts as
hand-edited.  The destination has to be the two-element form that sends
stderr elsewhere."
    (let (captured)
      (cl-letf (((symbol-function 'call-process)
                 (lambda (_prog _in out _display &rest args)
                   (setq captured (cons out args))
                   0)))
        (moyue-treesit--git-output "status" "--porcelain"))
      (should (equal (car captured) (list t nil)))
      (should (equal (cdr captured) '("status" "--porcelain")))))

  (ert-deftest treesit/ls-remote-puts-options-before-the-url ()
    "Regression: `git ls-remote URL --tags' silently lists nothing.
Options must precede the repository, otherwise git reads the option as a
ref pattern and every tag lookup degrades to a HEAD lookup without any
error, which is invisible unless the argument order itself is asserted."
    (let (captured)
      (cl-letf (((symbol-function 'call-process)
                 (lambda (_prog _in _out _display &rest args)
                   (setq captured args)
                   0)))
        (moyue-treesit--ls-remote "https://example.com/x" "--tags"))
      (should (equal captured
                     '("ls-remote" "--tags" "https://example.com/x")))))

  (ert-deftest treesit/ls-remote-ref-puts-the-ref-after-the-url ()
    "A ref pattern belongs after the repository, not before it."
    (let (captured)
      (cl-letf (((symbol-function 'call-process)
                 (lambda (_prog _in _out _display &rest args)
                   (setq captured args)
                   0)))
        (moyue-treesit--ls-remote-ref "https://example.com/x" "HEAD"))
      (should (equal captured
                     '("ls-remote" "https://example.com/x" "HEAD"))))))
(with-eval-after-load 'ert

  (ert-deftest treesit/update-without-write-leaves-the-file-alone ()
    "Resolving newer releases must not touch the manifest on its own."
    (moyue-treesit--test-with-manifest
     (lambda (file)
       (let ((before (moyue-treesit--test-file-contents file)))
         (moyue-treesit--test-with-stub file
           (moyue-treesit--update '("c")))
         (should (equal before (moyue-treesit--test-file-contents file)))))))

  (ert-deftest treesit/update-write-replaces-the-whole-plist ()
    "`update --write' must swap the plist out, not append to it.
Replacing the cdr of the plist itself would leave the old :commit as the
first element and corrupt every entry, which is what this guards against."
    (moyue-treesit--test-with-manifest
     (lambda (file)
       (moyue-treesit--test-with-stub file
         (moyue-treesit--update '("c" "--write")))
       (let ((entry (moyue-treesit--test-pinned-entry file 'c))
             (other (moyue-treesit--test-pinned-entry file 'bash)))
         (should (equal (plist-get (cddr entry) :commit)
                        moyue-treesit--test-new-commit))
         (should (equal (plist-get (cddr entry) :ref) "v99.0.0"))
         (should (equal (plist-get (cddr entry) :source-dir) "src"))
         ;; Three keys, so six elements: a corrupting append would leave the
         ;; previous :commit element behind and make this longer.
         (should (= (length (cddr entry)) 6))
         ;; Untouched entries must survive the rewrite intact.
         (should (= (length (cddr other)) 6)))))))
;;; treesit.el ends here
