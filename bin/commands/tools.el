;;; tools.el --- External tool installation for `moyue install' -*- lexical-binding: t; -*-

;;; Commentary:

;; `moyue install' sets up the Emacs configuration itself; this library is
;; what sets up the *programs* the configuration delegates work to.  Each one
;; is described by an entry of `moyue-tool-recipes' -- where to download it
;; from, which file to expose on PATH, and where it belongs -- and the
;; machinery below turns that entry into a download, a verification and an
;; install:
;;
;;   moyue install clangd clang-format ruff ty   # named tools
;;   moyue install --list                        # what can be installed
;;   moyue install --all                         # install every recipe
;;   moyue tools status                          # what is installed where
;;   moyue tools remove clangd --force           # take one back out
;;
;; Three shapes cover every tool that matters:
;;
;;   :strategy single    one self-contained executable.  It is downloaded
;;                       into `.cache/pkgs/<id>/<version>/' and copied into
;;                       `.emacs.d/bin/', which `init.el' always puts first
;;                       on PATH, so nothing else has to be configured.
;;
;;   :strategy archive   a tarball or zip with more than one file.  clangd is
;;                       why this exists: its executable needs the clang
;;                       headers beside it under `lib/clang/<major>/', so the
;;                       whole tree is unpacked into `.cache/pkgs/' and only a
;;                       *relative symlink* is left in `bin/'.  clangd derives
;;                       its resource directory from the path of its own
;;                       executable and a symlink still resolves there, so the
;;                       bundled headers are found with no wrapper script.
;;
;;   :strategy installer the vendor ships an install script (ruff, ty,
;;                       rustup).  That script owns the layout -- astral
;;                       installs into `$HOME/.local/bin', rustup into
;;                       `$HOME/.cargo/bin' -- and both directories are already
;;                       on the PATH that `moyue install' captures into
;;                       `.cache/env.el'.  The script is downloaded into the
;;                       package cache first and run from there, so what it
;;                       will do is inspectable and the script is never piped
;;                       straight from the network into a shell.  The
;;                       variables it reads are set per recipe:
;;                       `*_NO_MODIFY_PATH=1' keeps it from editing shell
;;                       startup files and `*_DISABLE_UPDATE=1' keeps its
;;                       self-updater from changing the version behind our
;;                       back.
;;
;; Everything is reproducible: `:version' pins a fallback, the newest release
;; is used only when it can actually be resolved, and `--version VERSION'
;; overrides both.  Downloads land in `.cache/pkgs/', never in a temporary
;; directory that is gone by the time something fails.
;;
;; `moyue doctor' reads the same table through `moyue-tool-recipes->requirements',
;; so a tool that can be installed here is exactly a tool whose absence doctor
;; reports -- and the hint it prints is this command.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; ── The recipe table ─────────────────────────────────────────────────────


(defconst moyue-tool-recipes
  '((clangd
     :kind tool
     :commands ("clangd")
     :also ((:id cpp :kind lsp :commands ("clangd" "ccls")
                 :modes (c-ts-mode c++-ts-mode c-mode c++-mode)))
     :title "clangd (clangd/clangd releases)"
     :version "23.1.0"
     :latest (:github "clangd/clangd")
     :strategy archive
     :download ((:url "https://github.com/clangd/clangd/releases/download/%v/clangd-%a-%v.zip"
                     :file "clangd-%a-%v.zip"
                     :format zip
                     :platforms
                     ((linux-x86_64 . (:a "linux"))
                      (linux-aarch64 . (:a "linux"))
                      (darwin-x86_64 . (:a "mac"))
                      (darwin-arm64 . (:a "mac"))
                      (windows-x86_64 . (:a "windows"))))
                    (:bind-unix ((:version "clangd_%v/bin/clangd" :as "clangd")))
                    (:bind-windows ((:version "clangd_%v/bin/clangd.exe" :as "clangd.exe"))))
     :hint "clangd for C and C++ (`moyue install clangd`)."
)

    (clang-format
     :kind tool
     :commands ("clang-format")
     :title "clang-format (cpp-linter/clang-tools-static-binaries)"
     :version "2026.09.01-5fb8802d"
     :latest (:github "cpp-linter/clang-tools-static-binaries")
     :strategy single
     :download ((:asset "clang-format-22_linux-amd64"
                     :file "%k"
                     :format single
                     :platforms
                     ((linux-x86_64 . (:asset "clang-format-22_linux-amd64"))
                     ;; The cpp-linter builds are static, so one asset serves both libcs.
                     (linux-x86_64-musl . (:asset "clang-format-22_linux-amd64"))
                      (linux-aarch64 . (:asset "clang-format-22_linux-arm64"))
                      (linux-aarch64-musl . (:asset "clang-format-22_linux-arm64"))
                      (darwin-x86_64 . (:asset "clang-format-22_macos-amd64"))
                      (darwin-arm64 . (:asset "clang-format-22_macos-arm64"))
                      (windows-x86_64 . (:asset "clang-format-22_windows-amd64.exe"))))
                    (:bind-unix ((:name "clang-format" :as "clang-format")))
                    (:bind-windows ((:name "clang-format.exe" :as "clang-format.exe"))))
     :hint "Formatting for C and C++ (`moyue install clang-format`)."
)

    (ruff
     :kind tool
     :commands ("ruff")
     :title "ruff (astral-sh/ruff official installer)"
     :version "0.16.10"
     :latest (:github "astral-sh/ruff")
     :strategy installer
     :download ((:url "https://astral.sh/ruff/install.sh"
                     :file "install.sh"
                     :shell "sh"
                     :env (("RUFF_INSTALL_DIR" . "~/.local/bin")
                           ("RUFF_NO_MODIFY_PATH" . "1")
                           ("RUFF_DISABLE_UPDATE" . "1")))
                    (:verify ("ruff" "--version")))
     :hint "Linting and formatting for Python (`moyue install ruff`)."
)

    (ty
     :kind tool
     :commands ("ty")
     :title "ty (astral-sh/ty official installer)"
     :version "0.0.84"
     :latest (:github "astral-sh/ty")
     :strategy installer
     :download ((:url "https://astral.sh/ty/install.sh"
                     :file "install.sh"
                     :shell "sh"
                     :env (("TY_INSTALL_DIR" . "~/.local/bin")
                           ("TY_NO_MODIFY_PATH" . "1")
                           ("TY_DISABLE_UPDATE" . "1")))
                    (:verify ("ty" "--version")))
     :hint "Type checking for Python (`moyue install ty`)."
)

    (rust
     :kind tool
     :commands ("cargo" "rustc")
     :also ((:id rust-analyzer :kind lsp :commands ("rust-analyzer")
                 :modes (rust-ts-mode rust-mode rustic-mode)
                 :hint "rust-analyzer for Rust (`moyue install rust`)."))
     :title "Rust toolchain and rust-analyzer (rustup)"
     :version "stable"
     :latest nil
     :strategy installer
     :download ((:url "https://sh.rustup.rs"
                     :file "rustup-init.sh"
                     :shell "sh"
                     :args ("-y" "--no-modify-path" "--profile" "default")
                     :env (("CARGO_HOME" . "~/.cargo")
                           ("RUSTUP_HOME" . "~/.rustup")
                           ("RUSTUP_INIT_SKIP_PATH_CHECK" . "yes")))
                    (:post (("rustup" "component" "add" "rust-analyzer")
                            ("cargo" "--version")
                            ("rustc" "--version")))
                    (:verify ("rust-analyzer" "--version")))
     :hint "The Rust toolchain and rust-analyzer (`moyue install rust`)."
)
)
  "Everything `moyue install TOOL' knows how to put on this machine.

This table is the single place a tool is described: `moyue doctor' derives
its checks from it and the install machinery derives its downloads from it,
so the two can never disagree.

A `:download' entry is a list of plists that `moyue-tool-recipe-download'
merges into one.  The first describes the release:

  :url        URL template; %v is the version and any other %LETTER comes
              from the selected `:platforms' entry
  :asset      alternatively the release asset name, which a `:platforms'
              entry may override when the upstream names it after its own
              version (`clang-format-22_...')
  :file       the name the download is stored under; %k stands for the
              asset name
  :format     `zip', `tar.gz', `tar.xz', `tar.zst' or `single'
  :platforms  alist of (PLATFORM . PLIST); an absent entry means the tool
              is not published for this machine
  :checksum   optional URL of a sha256 file to check the download against
  :bind       the files to expose in `bin/'; `:version' names a path in
              the archive, `:name' one in the payload and `:as' the name
              to expose.  A recipe whose Windows release carries an extra
              `.exe' spells `:bind-unix' and `:bind-windows' instead.

The installer strategy adds:

  :shell   the interpreter to run the script with, \"sh\" by default
  :args    extra arguments, e.g. rustup's \"-y --no-modify-path\"
  :env     ((\"VAR\" . \"VALUE\")), with a leading ~ expanded
  :post    command lists to run after the script
  :verify  a command list that proves the install worked")

;;;; ── Lookup ───────────────────────────────────────────────────────────────

(defun moyue-tool-ids ()
  "Return every installable tool id, in recipe order."
  (mapcar #'car moyue-tool-recipes))

(defun moyue-tool-recipe (id)
  "Return the recipe of ID, or signal an error naming the alternatives.
ID is a string as typed on the command line, or a symbol; it is matched by
name, because interning the same characters twice need not give `eq'
symbols and the recipe names are the table's keys."
  (let ((name (if (symbolp id) (symbol-name id) id))
        (found nil))
    (dolist (entry moyue-tool-recipes)
      (when (and (null found) (equal (symbol-name (car entry)) name))
        (setq found (cdr entry))))
    (or found
        (error "Unknown tool `%s'.  Available: %s"
               id (string-join (mapcar #'symbol-name (moyue-tool-ids)) ", ")))))

(defun moyue-tool-ids->recipes (ids)
  "Return the recipes named by the list of strings IDS.
Without IDS the recipes of every tool are returned."
  (if ids (mapcar #'moyue-tool-recipe ids) (mapcar #'cdr moyue-tool-recipes)))

(defun moyue-tool-recipe-id (recipe)
  "Return the id symbol of RECIPE.
An id is returned unchanged; a plist is matched against the table by
name, because interning the same characters twice need not give `eq'
symbols and `rassq' would compare the wrong element of the cons cell."
  (cond
   ((symbolp recipe) recipe)
   ((stringp recipe) (intern recipe))
   (t (let ((entry (cl-find recipe moyue-tool-recipes
                            :test (lambda (r e) (equal r (cdr e))))))
        (car entry)))))

(defun moyue-tool-recipe-plist (recipe)
  "Return the plist body of RECIPE.
`moyue-tool-recipes' is an alist whose values are already plists -- the
table is written as (ID :key VALUE ...) -- and `moyue-tool-recipe' hands
back that value, so RECIPE is the plist itself.  Dropping its first
element here would silently turn `:kind' into the id."
  recipe)

(defun moyue-tool-recipe-field (recipe key)
  "Return RECIPE's value for KEY."
  (plist-get (moyue-tool-recipe-plist recipe) key))

(defun moyue-tool-recipe-download (recipe)
  "Return RECIPE's network directive as one plist.
The table spells `:download' as a list so that the direct plist and the
shared `:bind'/' :verify' part read as separate entries.  `plist-get' only
looks at alternate elements, so the parts are merged here: a second plist in
the list would otherwise be invisible to every lookup, which is exactly the
bug that made the bind list come back nil."
  (let ((parts (moyue-tool-recipe-field recipe :download)))
    (cond ((null parts) nil)
          ((keywordp (car parts)) parts)
          (t (apply #'append parts)))))

(defun moyue-tool-recipe-title (recipe)
  "Return a one line description of RECIPE for --list."
  (or (moyue-tool-recipe-field recipe :title)
      (symbol-name (moyue-tool-recipe-id recipe))))

(defun moyue-tool-recipe-hint (recipe)
  "Return RECIPE's doctor hint."
  (or (moyue-tool-recipe-field recipe :hint)
      (format "Install it with `moyue install %s'." (moyue-tool-recipe-id recipe))))

(defun moyue-tool-recipes->requirements ()
  "Turn the recipes into `moyue-tool-requirements' entries.
Only the parts doctor reads are carried over, so the two tables cannot
disagree about which executable satisfies a tool or how to install it.

A recipe normally yields one entry.  `:also' adds more, which is what a
recipe that provides two different things needs: `moyue install clangd'
satisfies both the `config/tool-clangd' check and the eglot check
`config/lsp-cpp', and `moyue install rust' satisfies `config/tool-rust'
and `config/lsp-rust'."
  (let (requirements)
    ;; Walk the table rather than its values: the id is the alist key, and
    ;; finding it back from a value would mean comparing by identity.
    (dolist (entry moyue-tool-recipes)
      (let* ((recipe (cdr entry))
             (id (car entry))
             (hint (moyue-tool-recipe-hint recipe))
             (base (list :id id
                         :kind (or (moyue-tool-recipe-field recipe :kind) 'tool)
                         :commands (moyue-tool-recipe-field recipe :commands)
                         :required (moyue-tool-recipe-field recipe :required)
                         :hint hint)))
        (push base requirements)
        (dolist (extra (moyue-tool-recipe-field recipe :also))
          (let ((extra-id (plist-get extra :id))
                (extra-kind (plist-get extra :kind)))
            (unless (cl-find extra-id requirements
                             :key (lambda (r) (plist-get r :id)))
              (push (append (list :id extra-id
                                  :kind (if (memq extra-kind '(tool lsp))
                                            extra-kind
                                          'lsp)
                                  :commands (plist-get extra :commands)
                                  :required (plist-get extra :required)
                                  :hint (or (plist-get extra :hint) hint))
                            (when (plist-get extra :modes)
                              (list :modes (plist-get extra :modes))))
                    requirements))))))
    (nreverse requirements)))

;;;; ── Where things live ────────────────────────────────────────────────────

(defun moyue-tools-cache-directory ()
  "Return the directory downloads are unpacked into: .cache/pkgs.
The package is kept next to `bin/' on purpose: the configuration root is
what this command was run from, and a symlink placed in `bin/' has to stay
valid no matter where that tree is moved to."
  (file-name-as-directory
   (expand-file-name ".cache/pkgs" (moyue--config-root))))

(defun moyue-tools-bin-directory ()
  "Return the directory executables are exposed in: <config>/bin.
It is the directory `bin/moyue' lives in, and `init.el' puts it first on
PATH, so a tool installed here needs no further configuration."
  (file-name-as-directory (expand-file-name "bin" (moyue--config-root))))

(defun moyue-tools--ensure-on-exec-path ()
  "Put `bin/' on `exec-path' for this run.
The directory is already on the PATH that a normal Emacs startup installs,
but `moyue' itself is started from a shell whose PATH may not have it yet
on a first run, and a tool installed a moment ago should be found by the
check that follows."
  (let ((dir (directory-file-name (moyue-tools-bin-directory))))
    (unless (member dir exec-path)
      (setq exec-path (cons dir exec-path)))))

(defun moyue-tool-id-name (id)
  "Return the tool ID as a string, whether given a symbol or a string."
  (if (symbolp id) (symbol-name id) (format "%s" id)))

(defun moyue-tools-package-directory (id &optional version)
  "Return the cache directory of tool ID at VERSION.
Without VERSION the tool's own directory is returned, which is what
`moyue tools remove' deletes."
  (let ((base (expand-file-name (moyue-tool-id-name id)
                                (moyue-tools-cache-directory))))
    (if version (file-name-as-directory (expand-file-name version base)) base)))

(defun moyue-tools-installed-version (id)
  "Return the version directory of tool ID, or nil when it is absent.
A package directory holding more than one version reports the newest by
name, which is what `moyue tools status' shows."
  (let* ((dir (moyue-tools-package-directory id))
         ;; Only subdirectories: `installed.el' lives beside them, and
         ;; treating it as "the version" pointed later lookups at a file.
         (versions (and (file-directory-p dir)
                        (seq-filter #'file-directory-p
                                    (directory-files dir t "\\`[^.]" t)))))
    (car (last (sort (copy-sequence versions) #'string<)))))

;;;; ── Platform detection ────────────────────────────────────────────────────

(defun moyue-tools--musl-p ()
  "Return non-nil when this machine runs musl rather than glibc.
Alpine ships /etc/alpine-release; elsewhere `ldd --version' says so in its
first line."
  (or (file-exists-p "/etc/alpine-release")
      (ignore-errors
        (with-temp-buffer
          (and (eq 0 (call-process "ldd" nil t nil "--version"))
               (string-match-p "musl" (buffer-string)))))))

(defun moyue-tools-platform ()
  "Return the platform key recipes are matched against.
One of linux-x86_64, linux-x86_64-musl, darwin-arm64, windows-x86_64, ...
The value comes from `system-type' and `system-configuration', so it does
not depend on which Emacs happens to run the command."
  (let* ((arch (cond ((string-match-p "\\`\\(aarch64\\|arm64\\)" system-configuration) "aarch64")
                     ((string-match-p "\\`\\(x86_64\\|amd64\\)" system-configuration) "x86_64")
                     ((string-match-p "\\`i[3-6]86" system-configuration) "i686")
                     ((string-match-p "\\`armv7" system-configuration) "armv7")
                     ((string-match-p "\\`riscv64" system-configuration) "riscv64")
                     (t (or (car (split-string system-configuration "-"))
                            (symbol-name system-type)))))
         (os (cond ((eq system-type 'darwin) "darwin")
                   ((memq system-type '(windows-nt ms-dos)) "windows")
                   (t "linux")))
         (libc (if (and (equal os "linux") (moyue-tools--musl-p)) "-musl" "")))
    (intern (concat os "-" arch libc))))

(defun moyue-tools--platform-table (recipe platform)
  "Return the substitution table RECIPE provides for PLATFORM.
Signals an error when the tool is not published for this machine."
  (let* ((download (moyue-tool-recipe-download recipe))
         (entry (cdr (assq platform (plist-get download :platforms)))))
    (or entry
        (error "`%s' publishes no build for %s%s"
               (moyue-tool-recipe-id recipe) platform
               ;; musl needs a word of explanation: the release exists, it
               ;; just is not built for this libc.
               (if (string-suffix-p "-musl" (symbol-name platform))
                   " (the upstream Linux release is glibc; on Alpine the \
default build can be run after `apk add gcompat', or use the distribution \
package)"
                 "")))))

;;;; ── Talking to the network ────────────────────────────────────────────────

(defvar moyue-tools--release-cache (make-hash-table :test #'equal)
  "Memoises the GitHub API responses for the life of one command.")

(defun moyue-tools--raw-get (url)
  "Return the body of URL as a string, or signal an error.
The built-in URL library is used rather than curl so a small API response
never has to be written to a file first."
  (let ((buffer (url-retrieve-synchronously url t t 30)))
    (unless buffer
      (error "No response from %s" url))
    (with-current-buffer buffer
      (unwind-protect
          (progn
            (goto-char (point-min))
            (unless (re-search-forward "^HTTP/[0-9.]+ 200" nil t)
              (error "HTTP request for %s was not successful" url))
            (goto-char (point-min))
            (unless (re-search-forward "\r?\n\r?\n" nil t)
              (error "Malformed response from %s" url))
            (buffer-substring-no-properties (point) (point-max)))
        (kill-buffer buffer)))))

(defun moyue-tools--release (owner-repo)
  "Return the parsed `releases/latest' payload of OWNER-REPO, or nil.
A failed lookup is not an error: the recipe's pin is the offline fallback
and a machine without a network must still be able to install it.  The
response is memoised so a release is fetched at most once per command."
  (let ((cached (gethash owner-repo moyue-tools--release-cache 'missing)))
    (if (not (eq cached 'missing))
        cached
      (let ((value (condition-case err
                       (let ((json (json-parse-string
                                    (moyue-tools--raw-get
                                     (format "https://api.github.com/repos/%s/releases/latest"
                                             owner-repo))
                                    :object-type 'alist :array-type 'list
                                    :null-object nil)))
                         (unless (alist-get 'tag_name json)
                           (error "no tag_name in the response"))
                         json)
                     (error
                      (message "  (cannot resolve the latest %s: %s; using the pinned version)"
                               owner-repo (error-message-string err))
                      nil))))
        (puthash owner-repo value moyue-tools--release-cache)
        value))))

(defun moyue-tools--release-assets (owner-repo)
  "Return the asset names of OWNER-REPO's newest release."
  (mapcar (lambda (asset) (alist-get 'name asset))
          (alist-get 'assets (moyue-tools--release owner-repo))))

(defun moyue-tools-resolve-version (recipe &optional requested)
  "Return the version of RECIPE to install.
REQUESTED is what `--version' asked for and wins outright.  Otherwise the
newest release is looked up and RECIPE's :version is the offline fallback.
A recipe without :latest is pinned and never looked up."
  (or requested
      (let ((latest (moyue-tool-recipe-field recipe :latest)))
        (and latest
             (alist-get 'tag_name (moyue-tools--release (plist-get latest :github)))))
      (moyue-tool-recipe-field recipe :version)
      (error "Recipe `%s' has neither a version nor a release to look up"
             (moyue-tool-recipe-id recipe))))

(defun moyue-tools-substitute (template version table)
  "Expand TEMPLATE with VERSION and TABLE.
%s and %v both stand for the version; any other %LETTER is looked up in
TABLE as the keyword `:LETTER', so a value a platform does not define is
reported rather than silently producing a broken URL."
  (replace-regexp-in-string
   "%\\([A-Za-z]\\)"
   (lambda (match)
     (let ((letter (aref match 1)))
       (cond ((memq letter '(?s ?v)) version)
             (t (let ((value (plist-get table (intern (format ":%c" letter)))))
                  (unless value
                    (error "Platform value missing for %%%c in `%s'" letter template))
                  (format "%s" value))))))
   template t t))

(defun moyue-tools--expand-bind (bind version table)
  "Return BIND with its path templates expanded.
BIND is one entry of a recipe's `:bind', a plist naming the file to expose
either by its path in the archive (`:version') or by its name in the
payload (`:name').  Each template uses %v for VERSION and the platform
values in TABLE."
  (let ((out nil))
    (while bind
      (let ((key (pop bind))
            (value (pop bind)))
        (setq out (append out
                          (list key
                                (if (and (memq key '(:version :name :as))
                                         (stringp value))
                                    (moyue-tools-substitute value version table)
                                  value))))))
    out))

(defun moyue-tools--bind-raw (recipe platform)
  "Return the bind list RECIPE wants on PLATFORM.
The list usually lives in the directive's second plist.  A recipe whose
Windows release has different file names -- clangd and clang-format both
ship an extra `.exe' there -- may instead spell out `:bind-unix' and
`:bind-windows', and the one that matches PLATFORM wins."
  (let* ((windows (equal "windows" (car (split-string (symbol-name platform) "-"))))
         (directive (moyue-tool-recipe-download recipe)))
    (or (and windows (plist-get directive :bind-windows))
        (and (not windows) (plist-get directive :bind-unix))
        (plist-get directive :bind))))

(defun moyue-tools--download-directive (recipe version platform)
  "Return RECIPE's `:download' plist with its templates expanded.
VERSION and PLATFORM have already been resolved.  The result always carries
:url (or :asset) and :file, plus :format and the expanded :bind list for
the single and archive strategies."
  (let* ((raw (moyue-tool-recipe-download recipe))
         (has-platforms (plist-get raw :platforms))
         (table (and has-platforms (moyue-tools--platform-table recipe platform)))
         (asset (or (plist-get table :asset) (plist-get raw :asset)))
         ;; The bind list is the directive's second element in the table,
         ;; because it is shared by every platform.
         (bind-raw (or (plist-get raw :bind)
                       (moyue-tools--bind-raw recipe platform)))
         (url (cond ((plist-get raw :url)
                     (if table
                         (moyue-tools-substitute (plist-get raw :url) version table)
                       (plist-get raw :url)))
                    (asset
                     (format "https://github.com/%s/releases/download/%s/%s"
                             (plist-get (moyue-tool-recipe-field recipe :latest) :github)
                             version asset))))
         (file (cond ((null (plist-get raw :file)) asset)
                     (asset
                      (replace-regexp-in-string "%k" asset (plist-get raw :file) t t))
                     (table
                      (moyue-tools-substitute (plist-get raw :file) version table))
                     (t (plist-get raw :file))))
         (checksum (and (plist-get raw :checksum)
                        table
                        (moyue-tools-substitute (plist-get raw :checksum)
                                                version table)))
         (bind (when (and bind-raw table)
                 (mapcar (lambda (spec)
                           (moyue-tools--expand-bind spec version table))
                         bind-raw))))
    (when (and has-platforms (null url))
      (error "Recipe `%s' has neither :url nor :asset for %s"
             (moyue-tool-recipe-id recipe) platform))
    (let ((expanded (copy-sequence raw)))
      (setq expanded (plist-put expanded :url url))
      (setq expanded (plist-put expanded :file file))
      (when checksum (setq expanded (plist-put expanded :checksum checksum)))
      (when bind (setq expanded (plist-put expanded :bind bind)))
      expanded)))



;;;; ── Downloading ───────────────────────────────────────────────────────────

(defun moyue-tools--gnu-wget-p ()
  "Return non-nil when the wget on PATH understands GNU long options.
Alpine ships busybox wget, which rejects them: handing it `--show-progress'
fails every download there, so the flags are chosen from what the program
actually advertises."
  (ignore-errors
    (with-temp-buffer
      (and (eq 0 (call-process "wget" nil (list t nil) nil "--help"))
           (string-match-p "--show-progress" (buffer-string))))))

(defun moyue-tools--http-tool ()
  "Return (PROGRAM . ARGS-PREFIX) for the downloader to use.
curl is preferred because it fails loudly on an HTTP error, follows
redirects and creates the destination directory in one call; wget is the
fallback and only gets the options this wget understands."
  (cond ((executable-find "curl")
         (cons "curl" '("-fSL" "--retry" "3" "--create-dirs")))
        ((executable-find "wget")
         (cons "wget" (if (moyue-tools--gnu-wget-p)
                          '("-q" "--show-progress" "-O")
                        '("-q" "-O"))))
        ((executable-find "powershell") (cons "powershell" nil))
        (t (error "No downloader available: install curl or wget"))))

(defun moyue-tools-download (url file &optional label)
  "Download URL to FILE, creating its directory.  LABEL is printed instead.
Signal an error when the transfer fails or leaves an empty file behind, so
a proxy error page is never mistaken for a package."
  (make-directory (file-name-directory file) t)
  (message "  downloading %s" (or label url))
  (let* ((tool (moyue-tools--http-tool))
         (program (car tool))
         (prefix (cdr tool))
         (args (cond ((equal program "curl")
                      (append prefix (list "-o" file url)))
                     ((equal program "wget")
                      (append prefix (list file url)))
                     (t (list "-NoProfile" "-Command"
                              (format "Invoke-WebRequest -Uri '%s' -OutFile '%s'"
                                      url file))))))
    (let ((status (apply #'call-process program nil nil nil args)))
      (unless (and (integerp status) (zerop status))
        (error "Downloading %s failed (exit %s)" url status)))
    (unless (and (file-exists-p file)
                 (> (file-attribute-size (file-attributes file)) 0))
      (error "Downloading %s produced an empty file" url))
    file))

(defun moyue-tools--sha256-file (file)
  "Return the sha256 of FILE as a lowercase hex string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun moyue-tools--checksum-expectations (checksum-file)
  "Return the sha256 hashes listed in CHECKSUM-FILE.
A `.sha256' beside a release asset holds one hash, a `sha256.sum' many
lines of `HASH  NAME'; only the hashes are read, because which file the
recipe downloaded is already known."
  (with-temp-buffer
    (insert-file-contents checksum-file)
    (let (hashes)
      (goto-char (point-min))
      (while (re-search-forward "\\b\\([0-9a-fA-F]\\{64\\}\\)\\b" nil t)
        (push (downcase (match-string 1)) hashes))
      (nreverse hashes))))

(defun moyue-tools--verify-checksum (file checksum-file)
  "Check FILE against CHECKSUM-FILE and signal an error on a mismatch."
  (let* ((expected (moyue-tools--checksum-expectations checksum-file))
         (actual (moyue-tools--sha256-file file)))
    (unless expected
      (error "No sha256 hash found in %s" checksum-file))
    (unless (member actual expected)
      (error "Checksum mismatch for %s:\n  expected %s\n  actual   %s"
             (file-name-nondirectory file)
             (string-join expected ", ") actual))
    (message "  sha256 ok (%s)" (substring actual 0 12))))

(defun moyue-tools--fetch-checksum (download dir)
  "Download DOWNLOAD's `:checksum' file into DIR and return its path.
A recipe without `:checksum' returns nil, and the download is then only
checked for structural sanity rather than by hash."
  (let ((url (plist-get download :checksum)))
    (when url
      (let ((file (expand-file-name (concat (plist-get download :file) ".sha256") dir)))
        (moyue-tools-download url file (concat "checksum for " (plist-get download :file)))
        file))))

;;;; ── Unpacking ──────────────────────────────────────────────────────────────

(defun moyue-tools--extract-tar (archive dir format)
  "Extract ARCHIVE into DIR, choosing the flags from FORMAT."
  (unless (executable-find "tar")
    (error "tar is not available"))
  ;; -p lets tar create the destination: the caller may not have been able
  ;; to, and tar is the one that needs it to exist in the first place.
  (ignore-errors (make-directory dir t))
  (let ((flags (pcase format
                 ((or 'tar.gz 'tgz) '("-xzf"))
                 ('tar.xz '("-xJf"))
                 (_ '("-xf")))))
    (with-temp-buffer
      (let ((status (apply #'call-process "tar" nil (list t t) nil
                           (append flags (list archive) (list "-C" dir)))))
        (unless (zerop status)
          (error "Unpacking %s failed: %s" archive (string-trim (buffer-string))))))))

(defun moyue-tools--extract-zip (archive dir)
  "Extract ARCHIVE into DIR with unzip, refusing escaping members.
A release archive is data from the network: unpacking it must not be able
to write outside DIR, so an absolute path or a `..' component is an error
before the extraction happens."
  (unless (executable-find "unzip")
    (error "unzip is not available and is needed for %s" archive))
  (make-directory dir t)
  (let ((listing (with-temp-buffer
                   (unless (zerop (call-process "unzip" nil t nil "-Z1" archive))
                     (error "Cannot list %s" archive))
                   (split-string (buffer-string) "\n" t))))
    (dolist (member listing)
      (when (or (string-prefix-p "/" member)
                (string-match-p "\\`[A-Za-z]:" member)
                (member ".." (split-string member "/")))
        (error "Refusing %s: member `%s' escapes the archive" archive member))))
  (unless (zerop (call-process "unzip" nil nil nil "-q" "-o" archive "-d" dir))
    (error "Unpacking %s failed" archive)))

(defun moyue-tools--extract (archive dir format)
  "Unpack ARCHIVE into DIR according to FORMAT."
  (pcase format
    ('zip (moyue-tools--extract-zip archive dir))
    (_ (moyue-tools--extract-tar archive dir format))))

;;;; ── Installing and exposing ────────────────────────────────────────────────

(defun moyue-tools--chmod-x (file)
  "Make FILE executable."
  (set-file-modes file (logior (file-modes file) #o111)))

(defun moyue-tools--chmod-tree (dir)
  "Make every regular file below DIR executable.
A downloaded release is not always faithful about the mode bit -- and the
binding step has to link something the shell can run -- so the mode is set
here rather than assumed.  Files that are not programs are left alone."
  (when (file-directory-p dir)
    (dolist (entry (directory-files dir t "\\`[^.]" t))
      (if (file-directory-p entry)
          (moyue-tools--chmod-tree entry)
        (when (file-regular-p entry)
          (moyue-tools--chmod-x entry))))))

(defun moyue-tools--relative-target (link target)
  "Return TARGET as a path relative to the directory of LINK.
A relative symlink keeps the whole configuration tree relocatable, which a
tool that looks for its data beside its own executable (clangd) needs."
  (file-relative-name target (file-name-directory link)))

(defun moyue-tools--bind-files (recipe download version dir payload)
  "Return a list of (SOURCE . NAME) for RECIPE's `:bind' entries.
SOURCE is the file to expose in `bin/' under NAME.  VERSION substitutes the
paths, DIR is the unpacked archive and PAYLOAD the download itself for a
`single' payload."
  (mapcar
   (lambda (spec)
     (let ((source (cond ((plist-get spec :version)
                          (expand-file-name (plist-get spec :version) dir))
                         ((plist-get spec :name)
                          (expand-file-name (plist-get spec :name) payload))
                         (t payload))))
       (list source
             (or (plist-get spec :as)
                 (file-name-nondirectory (or (plist-get spec :version)
                                             (plist-get spec :name)
                                             ""))))))
   (plist-get download :bind)))

(defun moyue-tools--find-source (candidates dir payload)
  "Return the first path among CANDIDATES that exists.
Fall back to PAYLOAD when nothing matches and the download is a plain file,
which makes a recipe work whether the release is a bare binary or a program
inside an archive.  DIR is accepted for symmetry with the callers."
  (ignore dir)
  (or (cl-find-if #'file-exists-p candidates)
      (and (file-regular-p payload) payload)
      (error "None of %s exists after unpacking" (string-join candidates ", "))))

(defun moyue-tools--expose (source name copy)
  "Put SOURCE into `bin/' as NAME, by COPY or by a relative symlink."
  (let ((link (expand-file-name name (moyue-tools-bin-directory))))
    (when (file-exists-p link) (delete-file link))
    (make-directory (file-name-directory link) t)
    (if copy
        (progn (copy-file source link t)
               (moyue-tools--chmod-x link)
               (message "  %s (copy of %s)" link source))
      (make-symbolic-link (moyue-tools--relative-target link source) link)
      (message "  %s -> %s" link (file-relative-name source (file-name-directory link))))
    link))

(defun moyue-tools--manifest-file (recipe version)
  "Return the manifest path recording RECIPE at VERSION."
  (expand-file-name "installed.el"
                    (moyue-tools-package-directory (moyue-tool-recipe-id recipe)
                                                   version)))

(defun moyue-tools--manifest-write (recipe version)
  "Record RECIPE as installed at VERSION under its package directory.
The manifest is small, human readable, and ignored by git along with the
rest of `.cache'; `moyue tools status' reads the version back from it when
the payload itself carries no version in its name."
  (let ((file (moyue-tools--manifest-file recipe version)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (let ((print-length nil) (print-level nil))
        (insert ";; Written by `moyue install'.\n")
        (prin1 (list :id (moyue-tool-recipe-id recipe)
                     :version version
                     :platform (moyue-tools-platform)
                     :installed (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
               (current-buffer))
        (insert "\n")))
    file))

(defun moyue-tools--read-property (file key)
  "Return KEY from the plist in FILE, or nil when FILE cannot be read."
  (when (and (stringp file) (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (ignore-errors (plist-get (read (current-buffer)) key)))))

(defun moyue-tools--installed-manifest (id)
  "Return the manifest file of the installed ID, or nil."
  (let ((version (moyue-tools-installed-version id)))
    (and version (moyue-tools--manifest-file (moyue-tool-recipe id) version))))

(defun moyue-tools-installed-p (id)
  "Return the installed version of ID, or nil.
A tool counts as installed only when one of its declared commands resolves
on PATH: a manifest on its own is not trusted, because a link can be
deleted by hand."
  (let* ((recipe (moyue-tool-recipe id))
         (found (cl-find-if #'executable-find (moyue-tool-recipe-field recipe :commands))))
    (when found
      (or (moyue-tools--read-property (moyue-tools--installed-manifest id) :version)
          (moyue-tools-installed-version id)
          ;; A tool the vendor's script installed keeps its own books, so the
          ;; only thing left to report is what the program says about itself.
          (moyue-tools--version-from-command found)
          "installed"))))

(defun moyue-tools--version-from-command (program)
  "Return the version PROGRAM prints for `--version', or nil.
`moyue tools status' prints a version for a tool that this command did not
install, and the only source left is the program itself.  A banner such as
`cargo 1.94.1 (29ea6fb6a 2026-03-24)' is reduced to its number."
  (ignore-errors
    (with-temp-buffer
      (when (and (executable-find program)
                 (eq 0 (call-process program nil (list t nil) nil "--version")))
        (let* ((first (car (split-string (string-trim (buffer-string)) "\n" t)))
               (number (and first (string-match "[0-9]+\.[0-9]+[0-9.]*" first))))
          (cond (number (match-string 0 first))
                ((and first (not (string-empty-p first))) first)))))))

(defun moyue-tools--run (command &optional label)
  "Run COMMAND, a list of strings, and print its output under LABEL.
Used for the vendor script and for the verification commands.  A non-zero
status is reported but not raised here, because some tools answer
`--version' with a non-zero status and the caller decides what that means."
  (message "  %s" (or label (string-join command " ")))
  (let ((code (apply #'call-process (car command) nil (list t t) nil (cdr command))))
    (when (and (integerp code) (/= code 0))
      (message "  (exit %s)" code))
    code))

(defun moyue-tools--verify (recipe download)
  "Prove that RECIPE's tool runs and return the version string it prints.
The recipe's `:verify' command is tried first, then each `:commands' entry
with `--version'.  Nothing is fatal: the install is what matters, and the
caller reports what was printed."
  (let* ((explicit (plist-get download :verify))
         (attempts (if explicit
                       (list explicit)
                     (mapcar (lambda (command) (list command "--version"))
                             (moyue-tool-recipe-field recipe :commands))))
         (output nil))
    (while (and attempts (null output))
      (let ((command (car attempts)))
        (setq attempts (cdr attempts))
        (when (executable-find (car command))
          (with-temp-buffer
            (let ((code (apply #'call-process (car command) nil (list t nil) nil
                               (cdr command))))
              (when (and (integerp code) (zerop code))
                (setq output (string-trim (buffer-string)))))))))
    (and output (not (string-empty-p output)) output)))

;;;; ── The three strategies ──────────────────────────────────────────────────

(defun moyue-tools--install-single (recipe version download dir)
  "Install RECIPE at VERSION, whose DOWNLOAD is one executable in DIR.
The executable is copied into `bin/', not symlinked: it is self-contained,
and a copy keeps working when the cache is cleaned."
  (let ((payload (expand-file-name (plist-get download :file) dir)))
    (moyue-tools--chmod-x payload)
    (let ((binds (moyue-tools--bind-files recipe download version dir payload)))
      (if binds
          (mapcar (lambda (bind)
                    (moyue-tools--expose (moyue-tools--find-source (list (car bind))
                                                                   dir payload)
                                         (cadr bind) t))
                  binds)
        (list (moyue-tools--expose payload (plist-get download :file) t))))))

(defun moyue-tools--install-archive (recipe version download dir)
  "Install RECIPE at VERSION by unpacking DOWNLOAD into DIR.
The archive is expanded where it lies, so the files a binary looks for
beside itself -- clangd's `lib/clang/<major>/' -- are still there
afterwards, and only a relative symlink is left in `bin/'."
  (let ((archive (plist-get download :archive))
        (format (plist-get download :format)))
    (make-directory dir t)
    (moyue-tools--extract archive dir format)
    (moyue-tools--chmod-tree dir)
    (let ((binds (moyue-tools--bind-files recipe download version dir nil)))
      (unless binds
        (error "Recipe `%s' has no :bind, so nothing would appear on PATH"
               (moyue-tool-recipe-id recipe)))
      (mapcar (lambda (bind)
                (moyue-tools--expose (moyue-tools--find-source (list (car bind)) dir nil)
                                     (cadr bind) nil))
              binds))))

(defun moyue-tools--env-directories (env)
  "Return the directories a recipe's ENV makes programs live in.
CARGO_HOME is where rustup puts rustup and cargo, so its `bin' has to be
searched for the `:post' steps: the shell that started `moyue' has not
learned about that directory yet."
  (let (dirs)
    (dolist (entry env)
      (let ((name (car entry))
            (value (moyue-tools--env-value (cdr entry))))
        (when (equal name "CARGO_HOME")
          (push (expand-file-name "bin" value) dirs))
        (when (file-directory-p value)
          (push value dirs))))
    (nreverse dirs)))

(defun moyue-tools--resolved-command (command &optional dirs)
  "Return COMMAND with its program resolved as far as possible.
DIRS are the directories the recipe's environment created; a tool the
script installed a moment ago -- rustup in `~/.cargo/bin' -- is found there
because `exec-path' has not been refreshed yet."
  (let ((program (or (executable-find (car command))
                     (cl-find-if #'file-executable-p
                                 (mapcar (lambda (dir)
                                           (expand-file-name (car command) dir))
                                         dirs)))))
    (cons (or program (car command)) (cdr command))))

(defun moyue-tools--env-value (value)
  "Return VALUE as a script environment variable value.
Only a leading ~ is expanded: the other values are flags such as "1", and
running every one through `expand-file-name' turned them into paths like
`/home/user/.emacs.d/1'."
  (if (string-prefix-p "~" value) (expand-file-name value) value))

(defun moyue-tools-vendor-bin-directory ()
  "Return the directory a vendor install script should put programs in.
That is `~/.local/bin', the place the vendor scripts default to and a
directory this configuration already has on PATH.  It is *created* rather
than merely tested: the scripts install into it happily and would `mkdir'
it themselves, so a container whose `$HOME' starts empty must not be read
as unwritable and quietly redirected somewhere else -- that would stop the
tools from living where the user asked for them.

The configuration's own `bin/' is the fallback only when `$HOME' really
cannot be written (a sandboxed or read-only home), and MOYUE_TOOLS_INSTALL_DIR
overrides the choice for a machine that wants a different prefix."
  (let* ((preferred (or (getenv "MOYUE_TOOLS_INSTALL_DIR") "~/.local/bin"))
         (dir (file-name-as-directory (expand-file-name preferred)))
         (fallback (moyue-tools-bin-directory)))
    (if (condition-case nil (progn (make-directory dir t) t) (error nil))
        dir
      (message "  %s cannot be created; installing into %s"
               (directory-file-name dir) fallback)
      fallback)))

(defun moyue-tools--install-installer (recipe version download dir)
  "Install RECIPE at VERSION by running the vendor's script from DIR.
The recipe's environment variables are bound around the script and around
the `:post' steps, so a toolchain that installs a second program -- the way
rustup installs rust-analyzer -- keeps pointing at the same home."
  (let* ((script (expand-file-name (plist-get download :file) dir))
         (shell (or (plist-get download :shell) "sh"))
         (args (append (list shell script) (plist-get download :args)))
         (env (mapcar (lambda (pair)
                        (format "%s=%s" (car pair)
                                (moyue-tools--env-value (cdr pair))))
                      (plist-get download :env))))
    (unless (file-exists-p script)
      (error "Install script for `%s' is missing: %s"
             (moyue-tool-recipe-id recipe) script))
    (let* ((process-environment (append env process-environment))
           (dirs (moyue-tools--env-directories (plist-get download :env))))
      (moyue-tools--run args (format "%s %s" shell (file-name-nondirectory script)))
      (dolist (step (plist-get download :post))
        (moyue-tools--run (moyue-tools--resolved-command step dirs)))
      (moyue-tools--manifest-write recipe version)
      ;; The script owns the layout, so what it put on PATH is what gets
      ;; reported; the cache only records that it ran.
      (list (or (cl-find-if #'executable-find
                            (moyue-tool-recipe-field recipe :commands))
                (format "%s (installed by its own script)"
                        (string-join (moyue-tool-recipe-field recipe :commands) "/")))))))

;;;; ── The public entry points ────────────────────────────────────────────────

(cl-defstruct moyue-tools-result
  "What one `moyue install TOOL' run did."
  (id nil) (version nil) (url nil) (files nil) (verified nil) (up-to-date nil))

(defun moyue-tools-install (recipe &optional version force dry-run)
  "Install RECIPE, at VERSION when given.
Returns a `moyue-tools-result'.  An already installed tool is left alone
unless FORCE is non-nil; DRY-RUN prints the download and stops before it."
  (let* ((id (moyue-tool-recipe-id recipe))
         (current (moyue-tools-installed-p id))
         (resolved (moyue-tools-resolve-version recipe version))
         (platform (moyue-tools-platform))
         (download (moyue-tools--download-directive recipe resolved platform))
         (dir (moyue-tools-package-directory id resolved))
         (file (expand-file-name (or (plist-get download :file) "payload") dir))
         (result (make-moyue-tools-result :id id :version resolved
                                          :url (plist-get download :url))))
    (message "%s %s (%s)" id resolved platform)
    (cond
     (dry-run
      (message "  would download %s" (plist-get download :url))
      (message "  into %s" dir))
     ((and current (not force)
           (or (equal (format "%s" current) resolved)
               (null (moyue-tool-recipe-field recipe :latest))))
      (message "  already installed (%s); use --force to reinstall" current)
      (setf (moyue-tools-result-up-to-date result) t))
     (t
      (make-directory dir t)
      (moyue-tools-download (plist-get download :url) file)
      (let ((checksum (moyue-tools--fetch-checksum download dir)))
        (when checksum (moyue-tools--verify-checksum file checksum)))
      (let* ((strategy (moyue-tool-recipe-field recipe :strategy))
             (download (if (eq strategy 'single)
                           (plist-put download :source file)
                         (plist-put download :archive file)))
             (files (pcase strategy
                      ('single    (moyue-tools--install-single recipe resolved download dir))
                      ('archive   (moyue-tools--install-archive recipe resolved download dir))
                      ('installer (moyue-tools--install-installer recipe resolved download dir))
                      (_ (error "Recipe `%s' has an unknown :strategy %S" id strategy))))
             (verified (moyue-tools--verify recipe download)))
        (moyue-tools--manifest-write recipe resolved)
        (setf (moyue-tools-result-files result) files
              (moyue-tools-result-verified result) verified)
        (when verified
          (message "  %s: %s" id (car (split-string verified "\n")))))))
    result))

(defun moyue-tools-install-many (recipes &optional version force dry-run)
  "Install RECIPES one after another, returning the failures.
The run never stops at the first failure: one broken upstream is not a
reason to skip every other tool, and the summary is what the exit status
is built from."
  (let ((ok 0) (present 0) (failed '()))
    (dolist (recipe recipes)
      (condition-case err
          (if (moyue-tools-result-up-to-date
               (moyue-tools-install recipe version force dry-run))
              (setq present (1+ present))
            (setq ok (1+ ok)))
        (error
         (push (format "%s: %s"
                       (moyue-tool-recipe-id recipe) (error-message-string err))
               failed))))
    (if failed
        (message "\n%s%d, %d already present, %d failed:\n%s"
                 (if dry-run "would install " "")
                 ok present (length failed)
                 (mapconcat (lambda (line) (concat "  " line)) (nreverse failed) "\n"))
      (message "\n%s%d, %d already present."
               (if dry-run "would install " "") ok present))
    (nreverse failed)))

(defun moyue-tools-status-string (&optional ids)
  "Return a table describing the installation state of IDS.
Every tool is listed, installed or not, because the question worth
answering is usually `what is missing' rather than `what is there'."
  (mapconcat
   (lambda (recipe)
     (let* ((id (moyue-tool-recipe-id recipe))
            (installed (moyue-tools-installed-p id))
            (found (cl-find-if #'executable-find (moyue-tool-recipe-field recipe :commands))))
       (format "%-14s %-22s %-12s %s"
               id
               (or (moyue-tool-recipe-field recipe :version) "-")
               (if installed (format "%s" installed) "-")
               (cond (found (format "%s" found))
                     (installed (format "recorded (%s) but not on PATH" installed))
                     (t (format "missing (moyue install %s)" id))))))
   (moyue-tool-ids->recipes ids)
   "\n"))

(defun moyue-tools-remove (recipe force)
  "Delete RECIPE's downloaded package.  With FORCE also take it off PATH.
Without FORCE an entry in `bin/' that points into the package is reported
instead, because deleting the package would otherwise leave a dangling
symlink behind."
  (let* ((id (moyue-tool-recipe-id recipe))
         (dir (moyue-tools-package-directory id))
         (entries (cl-remove-if-not
                   #'file-exists-p
                   (mapcar (lambda (command)
                             (expand-file-name command (moyue-tools-bin-directory)))
                           (moyue-tool-recipe-field recipe :commands)))))
    (if (not (file-directory-p dir))
        (message "%s: nothing to remove (%s does not exist)" id dir)
      (when (and entries (not force))
        (error "%s is still on PATH (%s); use --force to remove both"
               id (string-join entries ", ")))
      (delete-directory dir t)
      (message "%s: removed %s" id dir)
      (dolist (entry entries)
        (delete-file entry)
        (message "%s: removed %s" id entry)))))

;;;; ── Argument handling ──────────────────────────────────────────────────────

(defun moyue-tools--parse-args (args)
  "Split ARGS into (OPTIONS . NAMES).
OPTIONS is an alist of the flags seen and NAMES the tools asked for.
`--version' consumes its value, so a version is never mistaken for a tool."
  (let ((options nil) (names nil) (rest args))
    (while rest
      (let ((arg (car rest)))
        (cond
         ((member arg '("--force" "-f")) (push (cons :force t) options))
         ((member arg '("--dry-run" "-n")) (push (cons :dry-run t) options))
         ((member arg '("--all" "-a")) (push (cons :all t) options))
         ((member arg '("--list" "-l")) (push (cons :list t) options))
         ((equal arg "--version")
          (unless (cadr rest) (error "--version needs a value"))
          (push (cons :version (cadr rest)) options)
          (setq rest (cdr rest)))
         ((string-prefix-p "--version=" arg)
          (push (cons :version (substring arg (length "--version="))) options))
         ((string-prefix-p "-" arg)
          (error "Unknown option `%s'" arg))
         (t (push arg names))))
      (setq rest (cdr rest)))
    (cons (nreverse options) (nreverse names))))

(defun moyue-tools-list-string ()
  "Return the table `moyue install --list' prints."
  (let ((lines (list (format "%-14s %-50s %s" "TOOL" "UPSTREAM" "STRATEGY"))))
    (dolist (recipe (mapcar #'cdr moyue-tool-recipes))
      (push (format "%-14s %-50s %s"
                    (moyue-tool-recipe-id recipe)
                    (moyue-tool-recipe-title recipe)
                    (moyue-tool-recipe-field recipe :strategy))
            lines))
    (push (concat "\nInstall with `moyue install TOOL ...', or `moyue install --all'.\n"
                  "Add --version VERSION to pin a release, --dry-run to only print the URL.")
          lines)
    (string-join (nreverse lines) "\n")))

(defun moyue-tools--install-named (names options)
  "Install the tools NAMES, honouring OPTIONS.
Without NAMES every recipe is installed, which is what --all amounts to."
  (moyue-tools--ensure-on-exec-path)
  (let ((failed (moyue-tools-install-many (moyue-tool-ids->recipes names)
                                          (cdr (assq :version options))
                                          (and (assq :force options) t)
                                          (and (assq :dry-run options) t))))
    (when failed (kill-emacs 1))))

(defun moyue-tools-command (args)
  "Run `moyue tools ARGS'."
  (moyue-tools--ensure-on-exec-path)
  (let* ((parsed (moyue-tools--parse-args args))
         (options (car parsed))
         (names (cdr parsed))
         (sub (car names)))
    (cond
     ;; Without a subcommand, status is what is wanted.
     ((member sub '(nil "status"))
      (when (cdr names)
        (error "`moyue tools status' takes no arguments"))
      (message "tool           pinned                 installed    on PATH\n%s"
               (moyue-tools-status-string)))
     ((equal sub "list")
      (message "%s" (moyue-tools-list-string)))
     ((equal sub "install")
      (moyue-tools--install-named (cdr names) options))
     ((equal sub "remove")
      (unless (cdr names)
        (error "`moyue tools remove' needs at least one tool"))
      (dolist (name (cdr names))
        (moyue-tools-remove (moyue-tool-recipe name) (and (assq :force options) t))))
     (t
      (error "Unknown `moyue tools' subcommand `%s'.  Try: list, status, remove, install"
             sub)))))

;;;; ── Command registration ───────────────────────────────────────────────────

(moyue-defcommand "tools" "[list|status|remove|install] [TOOL...]"
                  "Show, install or remove the external tools the configuration uses.
Without a subcommand, status is printed: one line per known tool with the
pinned version, the version actually installed and the executable that
satisfies it on PATH.

  moyue tools                        # what is installed, what is missing
  moyue tools list                   # what can be installed
  moyue tools install ruff ty        # the same work `moyue install' does
  moyue tools remove clangd --force  # delete the package and its bin/ entry

Every tool that can be installed here is also a `config/tool-*' or
`config/lsp-*' check in `moyue doctor'."
                  (moyue-tools-command args))

;;;; ── Tests ──────────────────────────────────────────────────────────────────
;; Run with `moyue test '"^moyue-tools/"'.  The tests never touch the
;; network: downloads are served from local paths and the shell steps are
;; stubbed, so they also run on a machine with no network at all.

(with-eval-after-load 'ert

  (defun moyue-tools--test-fixture (name content)
    "Return NAME inside a fresh temporary directory holding CONTENT."
    (let* ((dir (make-temp-file "moyue-tools-test-" t))
           (file (expand-file-name name dir)))
      (with-temp-file file (insert content))
      file))

  (defun moyue-tools--test-workspace ()
    "Return a fresh writable directory inside the configuration.
The download tests extract files and then set their mode, and the sandbox
that runs `moyue test' only allows that inside the workspace."
    (let ((dir (expand-file-name (format ".cache/test-%d" (random 1000000))
                                 (moyue--config-root))))
      (ignore-errors (delete-directory dir t))
      (make-directory dir t)
      dir))

  (defmacro moyue-tools--test (config &rest body)
    "Run BODY with `moyue--config-root' pointing at CONFIG."
    (declare (indent 1))
    `(cl-letf (((symbol-function 'moyue--config-root)
                (lambda () (file-name-as-directory ,config))))
       ,@body))

  (ert-deftest moyue-tools/recipes-are-well-formed ()
    "Every recipe carries what the installer and doctor read."
    (should (consp moyue-tool-recipes))
    (let ((ids '()))
      (dolist (entry moyue-tool-recipes)
        (let ((recipe (cdr entry)))
          (should (symbolp (car entry)))
          (should-not (member (car entry) ids))
          (push (car entry) ids)
          (should (memq (moyue-tool-recipe-field recipe :kind) '(tool lsp)))
          (should (consp (moyue-tool-recipe-field recipe :commands)))
          (should (cl-every #'stringp (moyue-tool-recipe-field recipe :commands)))
          (should (memq (moyue-tool-recipe-field recipe :strategy)
                        '(single archive installer)))
          (should (stringp (moyue-tool-recipe-field recipe :version)))
          (should (stringp (moyue-tool-recipe-hint recipe)))
          (dolist (extra (moyue-tool-recipe-field recipe :also))
            (should (symbolp (plist-get extra :id)))
            (should (memq (plist-get extra :kind) '(tool lsp)))
            (should (consp (plist-get extra :commands)))
            (when (eq (plist-get extra :kind) 'lsp)
              (should (consp (plist-get extra :modes)))))
          (let ((download (moyue-tools--download-directive
                           recipe (moyue-tool-recipe-field recipe :version)
                           'linux-x86_64)))
            (should (string-match-p "\\`https://" (or (plist-get download :url) "")))
            (should (plist-get download :file))
            (when (memq (moyue-tool-recipe-field recipe :strategy) '(single archive))
              (should (plist-get download :format))
              (should (plist-get download :platforms))
              (should (plist-get download :bind)))
            (when (eq (moyue-tool-recipe-field recipe :strategy) 'installer)
              (should (plist-get download :shell))))))))

  (ert-deftest moyue-tools/unknown-tool-names-the-alternatives ()
    (should-error (moyue-tool-recipe "does-not-exist") :type 'error)
    (should (eq (moyue-tool-recipe-id (moyue-tool-recipe "ruff")) 'ruff)))

  (ert-deftest moyue-tools/requirements-are-derived-from-recipes ()
    "doctor's table and the install table describe the same commands."
    (let* ((requirements (moyue-tool-recipes->requirements))
           (ids (mapcar (lambda (r) (plist-get r :id)) requirements))
           (kinds (mapcar (lambda (r) (plist-get r :kind)) requirements)))
      (should (= (length requirements) 7))
      ;; A recipe and its `:also' entries are distinct checks.
      (should (equal (length ids) (length (delete-dups ids))))
      (should (memq 'cpp ids))
      ;; The cpp check is the eglot one, so its kind must be `lsp'.
      (should (eq (plist-get (cl-find 'cpp requirements
                                      :key (lambda (r) (plist-get r :id)))
                             :kind)
                  'lsp))
      (dolist (requirement requirements)
        (should (memq (plist-get requirement :kind) '(tool lsp))))))

  (ert-deftest moyue-tools/platform-key-is-recognised ()
    (should (memq (moyue-tools-platform)
                  '(linux-x86_64 linux-aarch64 linux-x86_64-musl linux-aarch64-musl
                    darwin-x86_64 darwin-arm64 windows-x86_64))))

  (ert-deftest moyue-tools/substitute-fills-version-and-platform ()
    (should (equal (moyue-tools-substitute "%v-%a-%v" "1.2" '(:a "linux"))
                   "1.2-linux-1.2"))
    (should (equal (moyue-tools-substitute "%s/x" "9" nil) "9/x"))
    (should-error (moyue-tools-substitute "%a" "1" nil) :type 'error))

  (ert-deftest moyue-tools/download-directive-expands-an-archive ()
    (let* ((recipe (moyue-tool-recipe "clangd"))
           (download (moyue-tools--download-directive recipe "23.1.0" 'linux-x86_64)))
      (should (equal (plist-get download :file) "clangd-linux-23.1.0.zip"))
      (should (equal (plist-get download :format) 'zip))
      (should (equal (plist-get download :url)
                     "https://github.com/clangd/clangd/releases/download/23.1.0/clangd-linux-23.1.0.zip"))
      (should (equal (plist-get (car (plist-get download :bind)) :version)
                     "clangd_23.1.0/bin/clangd"))
      (should (equal (plist-get (car (plist-get download :bind)) :as) "clangd"))))

  (ert-deftest moyue-tools/download-directive-builds-the-url-from-the-asset ()
    "clang-format's asset name differs per platform, so the URL follows it."
    (let* ((recipe (moyue-tool-recipe "clang-format"))
           (download (moyue-tools--download-directive
                      recipe "2026.09.01-5fb8802d" 'linux-aarch64)))
      (should (equal (plist-get download :url)
                     (concat "https://github.com/cpp-linter/clang-tools-static-binaries"
                             "/releases/download/2026.09.01-5fb8802d"
                             "/clang-format-22_linux-arm64")))
      (should (equal (plist-get download :file) "clang-format-22_linux-arm64"))
      (should (equal (plist-get download :format) 'single))))

  (ert-deftest moyue-tools/download-directive-refuses-an-unknown-platform ()
    (should-error (moyue-tools--download-directive (moyue-tool-recipe "clangd")
                                                   "23" 'freebsd-x86_64)
                  :type 'error))

  (ert-deftest moyue-tools/version-prefers-the-request-over-the-pin ()
    (should (equal (moyue-tools-resolve-version (moyue-tool-recipe "ruff") "1.2.3")
                   "1.2.3"))
    (should (equal (moyue-tools-resolve-version (moyue-tool-recipe "rust")) "stable")))

  (ert-deftest moyue-tools/checksum-expectations-read-a-sha256-file ()
    (let ((file (moyue-tools--test-fixture
                 "x.sha256" (concat (make-string 64 ?a) "  x.tar.gz\n"))))
      (should (equal (moyue-tools--checksum-expectations file)
                     (list (make-string 64 ?a))))))

  (ert-deftest moyue-tools/checksum-mismatch-is-an-error ()
    (let* ((payload (moyue-tools--test-fixture "p.bin" "hello"))
           (sums (moyue-tools--test-fixture "p.bin.sha256"
                                            (concat (make-string 64 ?b) "  p.bin\n"))))
      (should-error (moyue-tools--verify-checksum payload sums) :type 'error)
      (with-temp-file sums
        (insert (moyue-tools--sha256-file payload) "  p.bin\n"))
      (should (moyue-tools--verify-checksum payload sums))))

  (ert-deftest moyue-tools/zip-members-cannot-escape-the-directory ()
    "An archive with a `..' member is refused before anything is extracted."
    (skip-unless (and (executable-find "zip") (executable-find "unzip")))
    (let* ((work (make-temp-file "moyue-tools-zip-" t))
           (archive (expand-file-name "evil.zip" work))
           (default-directory work))
      (with-temp-file (expand-file-name "payload" work) (insert "x"))
      (call-process "zip" nil nil nil archive "../escape")
      (should-error (moyue-tools--extract archive (expand-file-name "out" work) 'zip)
                    :type 'error)))

  (ert-deftest moyue-tools/tar-archive-is-unpacked-and-bound ()
    "A tarball is expanded in place and its bind entry linked into bin/."
    (skip-unless (executable-find "tar"))
    (let* ((work (moyue-tools--test-workspace))
           (config (moyue-tools--test-workspace))
           (payload-dir (expand-file-name "tool-1.0/bin" work)))
      (make-directory payload-dir t)
      (with-temp-file (expand-file-name "tool" payload-dir) (insert "#!/bin/sh\necho 1.0\n"))
      (let ((archive (expand-file-name "tool.tar.gz" work))
            (default-directory work))
        (call-process "tar" nil nil nil "-czf" archive "tool-1.0")
        (moyue-tools--test config
          (let* ((download (list :file "tool.tar.gz" :format 'tar.gz :archive archive
                                 :bind (list (list :version "tool-1.0/bin/tool"
                                                   :as "tool"))))
                 (files (moyue-tools--install-archive (moyue-tool-recipe "clangd")
                                                      "1.0" download
                                                      (expand-file-name "pkg" work))))
            (should (equal files (list (expand-file-name "bin/tool" config))))
            (should (file-symlink-p (car files)))
            (should (file-executable-p
                     (expand-file-name (file-symlink-p (car files))
                                       (file-name-directory (car files))))))))))

  (ert-deftest moyue-tools/single-payload-is-copied-into-bin ()
    (let* ((config (make-temp-file "moyue-tools-config-" t))
           (dir (file-name-as-directory (make-temp-file "moyue-tools-work-" t))))
      (with-temp-file (expand-file-name "tool" dir) (insert "#!/bin/sh\n"))
      (moyue-tools--test config
        (let* ((download (list :file "tool" :format 'single
                               :bind (list (list :name "tool" :as "mytool"))))
               (files (moyue-tools--install-single (moyue-tool-recipe "ruff")
                                                   "1.0" download dir)))
          (should (equal files (list (expand-file-name "bin/mytool" config))))
          (should-not (file-symlink-p (car files)))
          (should (file-executable-p (car files)))))))

  (ert-deftest moyue-tools/installer-binds-its-environment-and-runs-post-steps ()
    "The script sees the recipe's variables, and :post runs afterwards."
    (let* ((config (make-temp-file "moyue-tools-config-" t))
           (dir (file-name-as-directory (make-temp-file "moyue-tools-work-" t)))
           (script (expand-file-name "install.sh" dir))
           (seen nil)
           (run nil))
      (with-temp-file script (insert "#!/bin/sh\ntrue\n"))
      (cl-letf (((symbol-function 'moyue--config-root)
                 (lambda () (file-name-as-directory config)))
                ((symbol-function 'executable-find) (lambda (_name) nil))
                ((symbol-function 'moyue-tools--run)
                 (lambda (command &optional _label)
                   (push command run)
                   (setq seen (getenv "RUFF_INSTALL_DIR"))
                   0)))
        (let ((download (list :file "install.sh" :shell "sh"
                              :env '(("RUFF_INSTALL_DIR" . "~/.local/bin"))
                              :post '(("ruff" "--version")))))
          (moyue-tools--install-installer (moyue-tool-recipe "ruff") "1.0"
                                          download dir)
          ;; The recipe names ~/.local/bin and the ~ is expanded for the
          ;; script; nothing rewrites it to the configuration's own bin/.
          (should (equal seen (expand-file-name "~/.local/bin")))
          (should (equal (nreverse run) (list (list "sh" script)
                                              (list "ruff" "--version"))))))))

  (ert-deftest moyue-tools/wget-flags-follow-what-wget-accepts ()
    "A busybox wget must not be handed GNU-only options.
Alpine ships one, and `--show-progress' there fails every download, which
is exactly what the container run of `moyue install ruff' hit."
    ;; curl would win the selection otherwise, and this is about wget.
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name) (and (equal name "wget") "/usr/bin/wget")))
              ((symbol-function 'moyue-tools--gnu-wget-p) (lambda () t)))
      (should (member "--show-progress" (cdr (moyue-tools--http-tool)))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name) (and (equal name "wget") "/usr/bin/wget")))
              ((symbol-function 'moyue-tools--gnu-wget-p) (lambda () nil)))
      (should-not (member "--show-progress" (cdr (moyue-tools--http-tool))))))

  (ert-deftest moyue-tools/wget-gnu-detection-reads-the-help ()
    "GNU wget is recognised by the long option only it advertises."
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _) (insert "--show-progress  bar") 0)))
      (should (moyue-tools--gnu-wget-p)))
    (cl-letf (((symbol-function 'call-process)
               (lambda (&rest _) (insert "BusyBox v1.36 wget") 0)))
      (should-not (moyue-tools--gnu-wget-p))))

  (ert-deftest moyue-tools/vendor-directory-is-created-not-just-tested ()
    "An empty $HOME must still receive the tools in ~/.local/bin.
A container starts with no ~/.local at all; reading that as unwritable sent
ruff and ty into the configuration's bin/ instead, which is not where the
vendor scripts put them."
    (let ((home (make-temp-file "moyue-tools-home-" t)))
      (let ((process-environment (cons (concat "HOME=" home) process-environment)))
        (should (equal (moyue-tools-vendor-bin-directory)
                       (file-name-as-directory (expand-file-name "~/.local/bin"))))
        (should (file-directory-p (expand-file-name "~/.local/bin"))))))

  (ert-deftest moyue-tools/vendor-directory-honours-the-override ()
    "MOYUE_TOOLS_INSTALL_DIR decides the prefix when it is set."
    (let ((prefix (make-temp-file "moyue-tools-prefix-" t)))
      (let ((process-environment
             (cons (concat "MOYUE_TOOLS_INSTALL_DIR=" prefix) process-environment)))
        (should (equal (moyue-tools-vendor-bin-directory)
                       (file-name-as-directory prefix))))))

  (ert-deftest moyue-tools/env-values-only-expand-a-leading-tilde ()
    "A flag stays a flag: expanding it would make it a path."
    (should (equal (moyue-tools--env-value "1") "1"))
    (should (equal (moyue-tools--env-value "~/.local/bin")
                   (expand-file-name "~/.local/bin")))
    (should (equal (moyue-tools--env-value "yes") "yes")))

  (ert-deftest moyue-tools/static-assets-are-published-for-musl-too ()
    "clang-format's static build serves glibc and musl alike, clangd's does not."
    (should (moyue-tools--platform-table (moyue-tool-recipe "clang-format")
                                         'linux-x86_64-musl))
    (should-error (moyue-tools--platform-table (moyue-tool-recipe "clangd")
                                               'linux-x86_64-musl)
                  :type 'error))

  (ert-deftest moyue-tools/arguments-are-parsed-without-eating-names ()
    (let ((parsed (moyue-tools--parse-args '("--force" "--version" "1.2" "ruff" "ty"))))
      (should (equal (cdr parsed) '("ruff" "ty")))
      (should (equal (cdr (assq :version (car parsed))) "1.2"))
      (should (assq :force (car parsed))))
    (should (equal (cdr (assq :version (car (moyue-tools--parse-args '("--version=2")))))
                   "2"))
    (should-error (moyue-tools--parse-args '("--version")) :type 'error)
    (should-error (moyue-tools--parse-args '("--nope")) :type 'error))

  (ert-deftest moyue-tools/status-lists-every-tool ()
    (let ((output (moyue-tools-status-string)))
      (dolist (id (moyue-tool-ids))
        (should (string-match-p (format "\\b%s\\b" id) output)))
      (should (string-match-p "clangd" output))
      (should (string-match-p "ruff" output))))

  (ert-deftest moyue-tools/remove-refuses-while-it-is-on-path ()
    "A package whose entry is still in bin/ is only removed with --force."
    (let* ((config (make-temp-file "moyue-tools-config-" t))
           (pkg (expand-file-name ".cache/pkgs/ruff/1.0" config))
           (entry (expand-file-name "bin/ruff" config)))
      (make-directory pkg t)
      (make-directory (file-name-directory entry) t)
      (with-temp-file entry (insert ""))
      (moyue-tools--test config
        (should-error (moyue-tools-remove (moyue-tool-recipe "ruff") nil) :type 'error)
        (moyue-tools-remove (moyue-tool-recipe "ruff") t)
        (should-not (file-exists-p pkg))
        (should-not (file-exists-p entry))))))

(provide 'tools)
;;; tools.el ends here
