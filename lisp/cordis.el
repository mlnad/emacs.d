;;; cordis.el --- Spatiotemporal composability kernel for Emacs Lisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Liu

;; Author: Liu
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: lisp, tools
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A small Cordis-style kernel for Emacs Lisp: contexts, fibers, revertible
;; effects, reactive service dependencies, event dispatch, and a declarative
;; plugin tree.
;;
;; The kernel follows the semantics described in "A Programming Paradigm for
;; Spatiotemporal Composability" (arXiv:2608.25512) and the DSH Cordis
;; implementation, restricted to the synchronous subset that Emacs Lisp can
;; express faithfully:
;;
;;   - temporal composability: every registration is an effect; effects are
;;     disposed in LIFO order, are idempotent, and reject registration while
;;     their fiber is unloading;
;;   - spatial composability: plugins declare `inject' dependencies; a plugin
;;     stays PENDING until all services are available and is unloaded/reloaded
;;     when a provider is removed or replaced;
;;   - isolates: per-context service realms selected by name->label maps;
;;   - events: emit / bail / serial / parallel / waterfall.  `parallel' is
;;     sequential fan-out in this kernel (see README "deviations").
;;
;; This is the C0 kernel: synchronous effects and dispatch only.  Async
;; disposal (two-phase), task scheduling, HMR and loader patches are C1+ work.
;;
;; Quick example:
;;
;;   (cordis-define-plugin my-greeter
;;     :inject '(config)
;;     :apply (lambda (ctx config)
;;              (message "hello %s" (cordis-ctx-get ctx 'config))))
;;
;;   (let* ((root (cordis-create-root))
;;          (config-fiber (cordis-plugin root 'config-provider)))
;;     (cordis-plugin root 'my-greeter))
;;
;; See test/cordis-test.el for the executable semantics (G1-G11).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;;; Errors

(define-error 'cordis-error "Cordis error")
(define-error 'cordis-inactive-effect
              "Cannot register an effect while its fiber is unloading"
              'cordis-error)
(define-error 'cordis-missing-inject
              "Service is not declared in the plugin's inject list"
              'cordis-error)
(define-error 'cordis-missing-service
              "Required service is not available"
              'cordis-error)
(define-error 'cordis-duplicate-service
              "Service is already provided in this isolation scope"
              'cordis-error)
(define-error 'cordis-invalid-plugin "Invalid plugin" 'cordis-error)
(define-error 'cordis-aggregate-error
              "Multiple errors were collected during parallel dispatch"
              'cordis-error)

;;;; Structs

(cl-defstruct (cordis-context (:constructor cordis--make-context))
  "A service container and the node of a context tree.

NAME is a human readable label.  PARENT is the parent context or nil.
FIBER is the fiber that owns this context (nil for the root context and for
plain scope contexts created by `cordis-context-isolate'/`-intercept').
ROOT points at the root context.  SERVICES, EVENTS, FIBERS and LOADER are
shared by every context of the same tree and are only set on the root.
DEFAULT-LABEL is the stable isolation label of the root scope.
ISOLATE-MAP is an alist NAME -> LABEL inherited from the parent context.
INTERCEPTS is an alist NAME -> CONFIG reserved for per-context configuration."
  name parent fiber root services events fibers loader default-label
  isolate-map intercepts)

(cl-defstruct (cordis-fiber (:constructor cordis--make-fiber))
  "One plugin run.

STATE is one of `pending', `loading', `active', `failed', `unloading',
`disposed'.  EFFECTS are held by ROOT-EFFECT; CHILDREN are child plugin
fibers; PROVIDED is the list of service names this fiber provided while
applying; EPOCH is the provider-uid signature that produced the current run."
  id plugin config ctx parent children state root-effect epoch inject
  provided failed-error)

(cl-defstruct (cordis-effect (:constructor cordis--make-effect))
  "A revertible effect node.

DISPOSERS is a list of (LABEL . FUNCTION) in registration order; they are run
in reverse order.  CHILDREN are nested effects, disposed before DISPOSERS.
DISPOSED makes disposal idempotent."
  label fiber parent children disposers disposed)

(cl-defstruct (cordis-service (:constructor cordis--make-service))
  "A service implementation registered by a fiber.

The value is visible to consumers in the same isolation scope once the
provider fiber becomes ACTIVE.  CHECK is an optional predicate; when it
returns nil the service is treated as unavailable and dependents unload."
  name label ctx fiber value check active)

(cl-defstruct (cordis-listener (:constructor cordis--make-listener))
  "An event listener.

CTX is the registering context (used for scope filtering), FIBER the owning
fiber, MODE one of emit/bail/serial/parallel/waterfall.  PREPEND listeners
run before normal listeners; SEQ preserves registration order within each
group.  ONCE listeners remove themselves before their first invocation."
  ctx fiber event function mode prepend once seq active global)

;;;; Variables

(defvar cordis--plugin-registry (make-hash-table :test #'eq)
  "Global registry of plugin definitions, keyed by plugin name.")

(defvar cordis--current-fiber nil
  "The fiber whose apply/effect body is currently executing.")

(defvar cordis--current-effect nil
  "The innermost effect being built, or nil.")

(defvar cordis--listener-seq 0
  "Monotone sequence number for listener registration order.")

(defvar cordis--notify-queue nil
  "Service names waiting to be re-evaluated by `cordis--notify'.")

(defvar cordis--notifying nil
  "Non-nil while a `cordis--notify' drain is running.")

(defvar cordis-debug nil
  "When non-nil, report internal errors with `message'.")

(defvar cordis-error-functions nil
  "Abnormal hook run as (FUNCTION ERROR CONTEXT) for swallowed errors.
Used by service disposers and `emit' listeners so one bad callback does not
abort teardown or unrelated listeners.")

(defvar cordis-fiber-disposed-functions nil
  "Abnormal hook run as (FUNCTION FIBER) after a fiber is disposed.")

(defvar cordis-service-retract-functions nil
  "Abnormal hook run as (FUNCTION RECORD) after a service record is removed.

This is a diagnostics hook; it also makes the provider-final-unregister
ordering observable in tests.")

;;;; Small utilities

(defun cordis--report-error (err &optional context)
  "Report ERR, optionally with CONTEXT, without aborting the current action."
  (when cordis-debug
    (message "cordis: %s%s" (error-message-string err)
             (if context (format " (%s)" context) "")))
  (run-hook-with-args 'cordis-error-functions err context))

(defun cordis--ctx-ancestor-p (ancestor ctx)
  "Return non-nil when ANCESTOR is CTX or one of its ancestors."
  (let ((cur ctx))
    (while (and cur (not (eq cur ancestor)))
      (setq cur (cordis-context-parent cur)))
    (eq cur ancestor)))

(defun cordis--ctx-depth (ctx)
  "Return the depth of CTX in its context tree."
  (let ((depth 0)
        (cur ctx))
    (while (cordis-context-parent cur)
      (setq depth (1+ depth)
            cur (cordis-context-parent cur)))
    depth))

(defun cordis--label-for (ctx name)
  "Return the isolation label used for service NAME from CTX.
The nearest isolate entry wins; otherwise the root's default label is used."
  (let ((entry (assoc name (cordis-context-isolate-map ctx))))
    (or (cdr entry)
        (cordis-context-default-label (cordis-context-root ctx)))))

(defun cordis--resolve-config (spec config)
  "Merge CONFIG over the default config plist declared by plugin SPEC."
  (let ((defaults (plist-get spec :config)))
    (cond
     ((null defaults) (copy-sequence config))
     ((null config) (copy-sequence defaults))
     (t (append (copy-sequence config) (copy-sequence defaults))))))

;;;; Contexts

(defun cordis-create-root (&optional name)
  "Create a new root context named NAME (default \"root\")."
  (let ((root (cordis--make-context
               :name (or name "root")
               :default-label (make-symbol "cordis-root-scope"))))
    (setf (cordis-context-root root) root
          (cordis-context-services root) (make-hash-table :test #'equal)
          (cordis-context-events root) (make-hash-table :test #'eq)
          (cordis-context-fibers root) (make-hash-table :test #'eq)
          (cordis-context-loader root) (make-hash-table :test #'equal))
    root))

(defalias 'cordis-root #'cordis-create-root
  "Create a new Cordis root context.")

(defun cordis-context-extend (ctx &optional name)
  "Create a child context of CTX named NAME.
The child inherits the root store, the event bus and CTX's isolate map."
  (let ((child (cordis--make-context
                :name (or name (gensym "cordis-context-"))
                :parent ctx
                :root (cordis-context-root ctx)
                :services (cordis-context-services (cordis-context-root ctx))
                :events (cordis-context-events (cordis-context-root ctx))
                :fibers (cordis-context-fibers (cordis-context-root ctx))
                :loader (cordis-context-loader (cordis-context-root ctx))
                :isolate-map (copy-tree (cordis-context-isolate-map ctx))
                :intercepts (copy-tree (cordis-context-intercepts ctx)))))
    child))

(defun cordis-context-isolate (ctx name &optional label)
  "Return a child context in which service NAME uses isolation LABEL.
When LABEL is nil a fresh label is created; two contexts that pass the same
LABEL share the service realm.  Only NAME is affected."
  (let* ((label (or label (make-symbol (format "cordis-isolate-%s" name))))
         (child (cordis-context-extend ctx (format "isolate:%s" name))))
    (setf (cordis-context-isolate-map child)
          (cons (cons name label) (cordis-context-isolate-map ctx)))
    child))

(defun cordis-context-intercept (ctx name config)
  "Return a child context in which NAME has CONFIG intercept added.
Reserved for C1 configuration resolution; included now so plugin code can
already express scoped configuration."
  (let ((child (cordis-context-extend ctx (format "intercept:%s" name))))
    (setf (cordis-context-intercepts child)
          (cons (cons name config) (cordis-context-intercepts ctx)))
    child))

;;;; Effects

(cl-defun cordis--effect-run (ctx label function)
  "Run FUNCTION as a new effect in CTX's current fiber and return the effect.
On error the effect's already-collected disposers are rolled back before the
error is re-signalled."
  (let ((fiber (or cordis--current-fiber (cordis-context-fiber ctx))))
    (unless fiber
      (signal 'cordis-error
              (list "cordis-effect called outside a plugin fiber")))
    (when (memq (cordis-fiber-state fiber) '(unloading disposed))
      (signal 'cordis-inactive-effect (list label)))
    (let* ((parent (or cordis--current-effect
                       (cordis-fiber-root-effect fiber)))
           (effect (cordis--make-effect
                    :label label :fiber fiber :parent parent)))
      (when parent
        (push effect (cordis-effect-children parent)))
      (let ((cordis--current-fiber fiber)
            (cordis--current-effect effect))
        (condition-case err
            (progn
              (funcall function)
              effect)
          (error
           (cordis--dispose-effect effect)
           (signal (car err) (cdr err))))))))

(defmacro cordis-effect (ctx label &rest body)
  "Run BODY as a revertible effect in CTX, under LABEL.
Return the effect object.  Disposers registered by BODY (directly or through
Cordis helpers) are disposed in LIFO order by `cordis-dispose-effect', when
the owning fiber is disposed, or when an enclosing effect is disposed."
  (declare (indent 2) (debug t))
  `(cordis--effect-run ,ctx ,label (lambda () ,@body)))

(defun cordis--dispose-effect (effect)
  "Dispose EFFECT, its children and its disposers, idempotently."
  (unless (cordis-effect-disposed effect)
    (setf (cordis-effect-disposed effect) t)
    ;; Children and disposers are stored newest-first (they are pushed), so
    ;; walking them as-is gives the required LIFO order.
    (dolist (child (cordis-effect-children effect))
      (cordis--dispose-effect child))
    (setf (cordis-effect-children effect) nil)
    (dolist (entry (cordis-effect-disposers effect))
      (condition-case err
          (funcall (cdr entry))
        (error
         (cordis--report-error err (format "disposer %S" (car entry))))))
    (setf (cordis-effect-disposers effect) nil)))

(defun cordis-dispose-effect (effect)
  "Dispose EFFECT and everything registered inside it.
This is safe to call more than once."
  (cordis--dispose-effect effect))

(defun cordis--register-for-fiber (fiber disposer label)
  "Register DISPOSER under LABEL in FIBER's current effect.
Signal `cordis-inactive-effect' when FIBER is unloading or disposed."
  (when (memq (cordis-fiber-state fiber) '(unloading disposed))
    (signal 'cordis-inactive-effect (list (or label "disposer"))))
  (let ((effect (if (eq cordis--current-fiber fiber)
                    (or cordis--current-effect
                        (cordis-fiber-root-effect fiber))
                  (cordis-fiber-root-effect fiber))))
    (unless effect
      (signal 'cordis-error
              (list "no active effect scope for registration")))
    (push (cons (or label "disposer") disposer)
          (cordis-effect-disposers effect))
    disposer))

(defun cordis-register-disposer (disposer &optional label)
  "Register DISPOSER in the current fiber's current effect, under LABEL.
Return DISPOSER.  Call this inside a plugin's `apply' function, usually via
one of the helper APIs (`cordis-ctx-on', `cordis-ctx-provide',
`cordis-add-hook', ...)."
  (let ((fiber cordis--current-fiber))
    (unless fiber
      (signal 'cordis-error
              (list "cordis-register-disposer called outside a plugin apply")))
    (cordis--register-for-fiber fiber disposer label)))

(defalias 'cordis-register #'cordis-register-disposer
  "Alias for `cordis-register-disposer'.")

(defun cordis--effect-tree (effect)
  "Return a diagnostic plist tree for EFFECT."
  (list :label (cordis-effect-label effect)
        :disposed (and (cordis-effect-disposed effect) t)
        :disposers (mapcar #'car (cordis-effect-disposers effect))
        :children (mapcar #'cordis--effect-tree
                          (cordis-effect-children effect))))

(defun cordis-fiber-effect-tree (fiber)
  "Return the effect tree of FIBER, or nil when it has no active run."
  (when-let* ((root (cordis-fiber-root-effect fiber)))
    (cordis--effect-tree root)))

;;;; Fibers

(defun cordis--fiber-active-p (fiber)
  "Return non-nil when FIBER is active."
  (and fiber (eq (cordis-fiber-state fiber) 'active)))

(defun cordis--service-usable-p (record)
  "Return non-nil when service RECORD may be consumed right now."
  (and record
       (cordis-service-active record)
       (cordis--fiber-active-p (cordis-service-fiber record))
       (let ((check (cordis-service-check record)))
         (or (null check)
             (funcall check)))))

(defun cordis--find-service (ctx name)
  "Return the service record for NAME visible from CTX, or nil.
Lookup is by isolation label, not by the fiber tree: services are shared by
every context that uses the same label, matching Cordis's root store."
  (let* ((root (cordis-context-root ctx))
         (label (cordis--label-for ctx name)))
    (gethash (cons label name) (cordis-context-services root))))

(defun cordis--fiber-available-p (fiber)
  "Return non-nil when every service FIBER injects is usable."
  (cl-every (lambda (name)
              (cordis--service-usable-p
               (cordis--find-service (cordis-fiber-ctx fiber) name)))
            (cordis-fiber-inject fiber)))

(defun cordis--fiber-epoch (fiber)
  "Return the provider-uid signature for FIBER's current dependencies."
  (let ((names (cordis-fiber-inject fiber)))
    (when (cl-every (lambda (name)
                      (cordis--service-usable-p
                       (cordis--find-service (cordis-fiber-ctx fiber) name)))
                    names)
      (mapconcat
       (lambda (name)
         (let ((record (cordis--find-service (cordis-fiber-ctx fiber) name)))
           (format "%s=%s" name (cordis-fiber-id (cordis-service-fiber record)))))
       names "|"))))

(defun cordis--apply-fiber (fiber epoch)
  "Apply the plugin of FIBER, whose dependency signature is EPOCH."
  (let* ((spec (gethash (cordis-fiber-plugin fiber) cordis--plugin-registry))
         (root-effect (cordis--make-effect :label 'root :fiber fiber))
         (cordis--current-fiber fiber)
         (cordis--current-effect root-effect))
    (setf (cordis-fiber-root-effect fiber) root-effect
          (cordis-fiber-state fiber) 'loading)
    (condition-case err
        (progn
          (funcall (plist-get spec :apply)
                   (cordis-fiber-ctx fiber)
                   (cordis-fiber-config fiber))
          (setf (cordis-fiber-state fiber) 'active
                (cordis-fiber-epoch fiber) epoch)
          ;; Services provided by this fiber become visible only now.
          (cordis--notify (cordis-fiber-ctx fiber)
                          (cordis-fiber-provided fiber)))
      (error
       (setf (cordis-fiber-state fiber) 'failed
             (cordis-fiber-failed-error fiber) err)
       (cordis--dispose-effect root-effect)
       (setf (cordis-fiber-root-effect fiber) nil
             (cordis-fiber-provided fiber) nil
             (cordis-fiber-epoch fiber) nil)
       (cordis--report-error err
                             (format "plugin %S failed"
                                     (cordis-fiber-plugin fiber)))))))

(defun cordis--unload-fiber (fiber)
  "Unload FIBER's effects and children while keeping the fiber object.
The fiber ends in `pending' state so it can be re-applied when its
dependencies reappear."
  (let ((cordis--current-fiber fiber)
        (cordis--current-effect nil))
    (setf (cordis-fiber-state fiber) 'unloading)
    (dolist (child (reverse (copy-sequence (cordis-fiber-children fiber))))
      (cordis-fiber-dispose child))
    (setf (cordis-fiber-children fiber) nil)
    (when (cordis-fiber-root-effect fiber)
      (cordis--dispose-effect (cordis-fiber-root-effect fiber))))
  (setf (cordis-fiber-root-effect fiber) nil
        (cordis-fiber-provided fiber) nil
        (cordis-fiber-epoch fiber) nil
        (cordis-fiber-state fiber) 'pending))

(defun cordis--refresh-fiber (fiber)
  "Re-evaluate FIBER's dependency state and apply/unload/reload as needed."
  (let ((state (cordis-fiber-state fiber)))
    (unless (memq state '(disposed unloading failed))
      (let* ((available (cordis--fiber-available-p fiber))
             (new-epoch (and available (cordis--fiber-epoch fiber))))
        (cond
         ((not available)
          (when (memq state '(active loading))
            (cordis--unload-fiber fiber))
          (setf (cordis-fiber-state fiber) 'pending
                (cordis-fiber-epoch fiber) nil))
         ((and (eq state 'active)
               (equal new-epoch (cordis-fiber-epoch fiber)))
          nil)
         (t
          (when (eq state 'active)
            (cordis--unload-fiber fiber))
          (cordis--apply-fiber fiber new-epoch)))))))

(defun cordis--notify (ctx names)
  "Schedule re-evaluation of every fiber that injects one of NAMES.
Notifications are drained through a queue so that a disposer/provider that
triggers further notifications cannot re-enter the drain."
  (when names
    (let ((root (cordis-context-root ctx)))
      (setq cordis--notify-queue (append cordis--notify-queue names))
      (unless cordis--notifying
        (let ((cordis--notifying t))
          (unwind-protect
              (while cordis--notify-queue
                (let ((name (pop cordis--notify-queue))
                      (fibers nil))
                  (maphash (lambda (_id fiber) (push fiber fibers))
                           (cordis-context-fibers root))
                  (dolist (fiber fibers)
                    (when (and (memq name (cordis-fiber-inject fiber))
                               (memq (cordis-fiber-state fiber)
                                     '(pending active)))
                      (cordis--refresh-fiber fiber)))))
            (setq cordis--notify-queue nil)))))))

(defun cordis-fiber-dispose (fiber)
  "Dispose FIBER: children first, then its effects, then remove it.
The fiber ends in `disposed' state.  Safe to call repeatedly."
  (unless (memq (cordis-fiber-state fiber) '(unloading disposed))
    (let ((cordis--current-fiber fiber)
          (cordis--current-effect nil))
      (setf (cordis-fiber-state fiber) 'unloading)
      ;; Child plugins die with their parent.  The mount disposer also calls
      ;; this function; idempotence keeps that harmless.
      (dolist (child (reverse (copy-sequence (cordis-fiber-children fiber))))
        (cordis-fiber-dispose child))
      (setf (cordis-fiber-children fiber) nil)
      (when (cordis-fiber-root-effect fiber)
        (cordis--dispose-effect (cordis-fiber-root-effect fiber)))
      (setf (cordis-fiber-root-effect fiber) nil
            (cordis-fiber-provided fiber) nil
            (cordis-fiber-epoch fiber) nil))
    (when (cordis-fiber-parent fiber)
      (setf (cordis-fiber-children (cordis-fiber-parent fiber))
            (delq fiber (cordis-fiber-children (cordis-fiber-parent fiber)))))
    (remhash (cordis-fiber-id fiber)
             (cordis-context-fibers
              (cordis-context-root (cordis-fiber-ctx fiber))))
    (setf (cordis-fiber-state fiber) 'disposed)
    (run-hook-with-args 'cordis-fiber-disposed-functions fiber)))

;;;; Services

(defun cordis-ctx-provide (ctx name value &optional check)
  "Provide VALUE as service NAME in CTX's isolation scope.
CHECK is an optional predicate consulted when dependents are refreshed.
Return the service record.  The registration is an effect of the current
fiber; it is removed automatically when the fiber unloads or is disposed."
  (let* ((fiber (or cordis--current-fiber (cordis-context-fiber ctx)))
         (root (cordis-context-root ctx))
         (label (cordis--label-for ctx name))
         (key (cons label name)))
    (unless fiber
      (signal 'cordis-error
              (list "cordis-ctx-provide called outside a plugin fiber")))
    (when (gethash key (cordis-context-services root))
      (signal 'cordis-duplicate-service (list name)))
    (let ((record (cordis--make-service
                   :name name :label label :ctx ctx :fiber fiber
                   :value value :check check :active t)))
      (puthash key record (cordis-context-services root))
      (cl-pushnew name (cordis-fiber-provided fiber))
      (cordis-register-disposer
       (lambda () (cordis--retract-service record))
       (format "service %s" name))
      record)))

(defun cordis--retract-service (record)
  "Retract service RECORD: mark it unavailable, wake dependents, then remove."
  (when (cordis-service-active record)
    (setf (cordis-service-active record) nil)
    (let* ((ctx (cordis-service-ctx record))
           (root (cordis-context-root ctx))
           (key (cons (cordis-service-label record)
                      (cordis-service-name record))))
      (cordis--notify ctx (list (cordis-service-name record)))
      (when (eq (gethash key (cordis-context-services root)) record)
        (remhash key (cordis-context-services root)))
      (run-hook-with-args 'cordis-service-retract-functions record))))

(defun cordis-notify-service (ctx name)
  "Force dependents of service NAME to re-evaluate availability.
Useful when a service's CHECK predicate depends on mutable state."
  (cordis--notify ctx (list name)))

(defun cordis-ctx-get (ctx name)
  "Return service NAME from CTX.
Signal `cordis-missing-inject' when the current fiber did not declare NAME
in its inject list, and `cordis-missing-service' when no active provider is
visible in CTX's isolation scope."
  (let ((fiber (or cordis--current-fiber (cordis-context-fiber ctx))))
    (when fiber
      (unless (memq name (cordis-fiber-inject fiber))
        (signal 'cordis-missing-inject
                (list name (cordis-fiber-plugin fiber))))))
  (let ((record (cordis--find-service ctx name)))
    (unless (cordis--service-usable-p record)
      (signal 'cordis-missing-service (list name)))
    (cordis-service-value record)))

(defun cordis-ctx-set (ctx name value)
  "Overwrite service NAME's value from its providing fiber.
Signals `cordis-missing-service' when NAME is not provided, and
`cordis-error' when called from a fiber different from the provider."
  (let ((record (cordis--find-service ctx name)))
    (unless record
      (signal 'cordis-missing-service (list name)))
    (unless (eq (cordis-service-fiber record) cordis--current-fiber)
      (signal 'cordis-error
              (list (format "cannot set service %S from another fiber" name))))
    (setf (cordis-service-value record) value)
    value))

(defun cordis--service-available-p (ctx name)
  "Return non-nil when service NAME is usable from CTX."
  (cordis--service-usable-p (cordis--find-service ctx name)))

;;;; Plugin registry and mounting

(defun cordis-plugin-register (name &rest plist)
  "Register plugin NAME with PLIST.
PLIST accepts :inject (list of service names), :config (default config
plist), :apply (function of CTX and CONFIG), and :doc.  Re-registering NAME
replaces the previous definition.  Return NAME."
  (unless (symbolp name)
    (signal 'cordis-invalid-plugin (list name)))
  (let ((apply-fn (plist-get plist :apply)))
    (unless (functionp apply-fn)
      (signal 'cordis-invalid-plugin (list name "missing :apply function")))
    (puthash name
             (list :name name
                   :inject (plist-get plist :inject)
                   :config (plist-get plist :config)
                   :isolate (plist-get plist :isolate)
                   :apply apply-fn
                   :doc (plist-get plist :doc))
             cordis--plugin-registry)
    name))

(defmacro cordis-define-plugin (name &rest plist)
  "Define a plugin NAME with PLIST; see `cordis-plugin-register'.
The plugin is stored in the global registry and can be mounted with
`cordis-plugin'."
  (declare (indent 1))
  `(cordis-plugin-register ',name ,@plist))

(defun cordis-plugin-spec (name)
  "Return the registry entry for plugin NAME, or nil."
  (gethash name cordis--plugin-registry))

(defun cordis--ctx-with-isolates (ctx isolates)
  "Return CTX wrapped in the isolation edges described by ISOLATES.
ISOLATES is an alist NAME -> LABEL; a bare NAME (symbol) gets a fresh label."
  (let ((cur ctx))
    (dolist (entry isolates cur)
      (if (consp entry)
          (setq cur (cordis-context-isolate cur (car entry) (cdr entry)))
        (setq cur (cordis-context-isolate cur entry))))))

(defun cordis-plugin (ctx plugin &optional config)
  "Mount PLUGIN under CTX with optional CONFIG, and return its fiber.
The new fiber is appended to CTX's fiber if CTX belongs to a plugin; in that
case disposing the parent disposes this plugin too.  If the plugin's inject
dependencies are not satisfied the fiber stays PENDING and is applied when
they become available."
  (let ((spec (gethash plugin cordis--plugin-registry)))
    (unless spec
      (signal 'cordis-invalid-plugin (list plugin)))
    (let* ((root (cordis-context-root ctx))
           (parent-fiber (cordis-context-fiber ctx))
           (base-ctx (cordis-context-extend
                      ctx (format "plugin:%s" plugin)))
           (plugin-ctx (cordis--ctx-with-isolates
                        base-ctx (plist-get spec :isolate)))
           (fiber (cordis--make-fiber
                   :id (gensym "cordis-fiber-")
                   :plugin plugin
                   :config (cordis--resolve-config spec config)
                   :ctx plugin-ctx
                   :parent parent-fiber
                   :state 'pending
                   :inject (plist-get spec :inject))))
      (setf (cordis-context-fiber plugin-ctx) fiber)
      (when parent-fiber
        (push fiber (cordis-fiber-children parent-fiber))
        (cordis--register-for-fiber
         parent-fiber
         (lambda () (cordis-fiber-dispose fiber))
         (format "child plugin %s" plugin)))
      (puthash (cordis-fiber-id fiber) fiber (cordis-context-fibers root))
      (cordis--refresh-fiber fiber)
      fiber)))

(defun cordis-inject (ctx inject function &optional name)
  "Mount an anonymous plugin that injects INJECT and runs FUNCTION.
This is the Elisp analogue of `ctx.inject(deps, callback)'.  NAME is used
for diagnostics only."
  (let ((plugin (or name (gensym "cordis-inject-"))))
    (cordis-plugin-register plugin
                            :inject inject
                            :apply function
                            :doc "anonymous injected plugin")
    (cordis-plugin ctx plugin)))

;;;; Event bus

(defconst cordis--event-modes '(emit bail serial parallel waterfall)
  "Supported event dispatch modes.")

(defun cordis--event-table (ctx)
  "Return the event hash table of CTX's root."
  (cordis-context-events (cordis-context-root ctx)))

(defun cordis-ctx-on (ctx event function &rest plist)
  "Register FUNCTION as a listener for EVENT from CTX.
PLIST accepts :mode (one of `cordis--event-modes', default `emit'),
:prepend, :once and :global.  The registration is an effect of the
current fiber and is removed when the fiber is disposed.  Return the
listener object."
  (let* ((fiber (or cordis--current-fiber (cordis-context-fiber ctx)))
         (mode (or (plist-get plist :mode) 'emit))
         (listener (cordis--make-listener
                    :ctx ctx :fiber fiber :event event :function function
                    :mode mode
                    :prepend (and (plist-get plist :prepend) t)
                    :once (and (plist-get plist :once) t)
                    :global (and (plist-get plist :global) t)
                    :seq (cl-incf cordis--listener-seq)
                    :active t)))
    (unless fiber
      (signal 'cordis-error
              (list "cordis-ctx-on called outside a plugin fiber")))
    (unless (memq mode cordis--event-modes)
      (signal 'cordis-error
              (list (format "unknown event mode %S" mode))))
    (push listener (gethash event (cordis--event-table ctx)))
    (cordis-register-disposer
     (lambda () (cordis--remove-listener listener))
     (format "listener %S" event))
    listener))

(defun cordis-ctx-once (ctx event function &rest plist)
  "Like `cordis-ctx-on' but remove FUNCTION after its first invocation."
  (apply #'cordis-ctx-on ctx event function :once t plist))

(defun cordis--remove-listener (listener)
  "Deactivate and unregister LISTENER."
  (when (cordis-listener-active listener)
    (setf (cordis-listener-active listener) nil)
    (let* ((ctx (cordis-listener-ctx listener))
           (table (cordis--event-table ctx))
           (event (cordis-listener-event listener)))
      (puthash event (delq listener (gethash event table)) table))))

(defun cordis--listener-visible-p (listener ctx)
  "Return non-nil when LISTENER may observe a dispatch from CTX."
  (or (cordis-listener-global listener)
      (cordis--ctx-ancestor-p (cordis-listener-ctx listener) ctx)))

(defun cordis--dispatch (ctx event mode)
  "Return active MODE listeners for EVENT visible from CTX, in call order."
  (let* ((table (cordis--event-table ctx))
         (listeners (copy-sequence (gethash event table)))
         (visible (seq-filter
                   (lambda (listener)
                     (and (cordis-listener-active listener)
                          (eq (cordis-listener-mode listener) mode)
                          (cordis--listener-visible-p listener ctx)))
                   listeners)))
    (sort visible
          (lambda (a b)
            (let ((pa (if (cordis-listener-prepend a) 0 1))
                  (pb (if (cordis-listener-prepend b) 0 1)))
              (if (/= pa pb)
                  (< pa pb)
                (< (cordis-listener-seq a)
                   (cordis-listener-seq b))))))))

(defun cordis--invoke-listener (listener args)
  "Invoke LISTENER with ARGS, handling `:once' before the call."
  (when (cordis-listener-once listener)
    (cordis--remove-listener listener))
  (apply (cordis-listener-function listener) args))

(defun cordis--bailed-p (value)
  "Return non-nil when VALUE is a bail result.
Cordis treats null and false as non-bail; in Elisp nil and :json-false are
the two false-like values."
  (and value (not (eq value :json-false))))

(defun cordis-ctx-emit (ctx event &rest args)
  "Dispatch EVENT in `emit' mode with ARGS and return nil.
All matching listeners are invoked in registration order.  Listener errors
are reported through `cordis-error-functions' and do not abort the others."
  (dolist (listener (cordis--dispatch ctx event 'emit))
    (condition-case err
        (cordis--invoke-listener listener args)
      (error
       (cordis--report-error
        err (format "emit listener for %S" event)))))
  nil)

(defun cordis-ctx-bail (ctx event &rest args)
  "Dispatch EVENT in `bail' mode with ARGS.
Return the first non-nil listener result (nil and :json-false are ignored)."
  (catch 'cordis-bail
    (dolist (listener (cordis--dispatch ctx event 'bail))
      (let ((value (cordis--invoke-listener listener args)))
        (when (cordis--bailed-p value)
          (throw 'cordis-bail value))))
    nil))

(defun cordis-ctx-serial (ctx event &rest args)
  "Dispatch EVENT in `serial' mode with ARGS.
Listeners run in order and the first bail value stops the chain."
  (catch 'cordis-serial
    (dolist (listener (cordis--dispatch ctx event 'serial))
      (let ((value (cordis--invoke-listener listener args)))
        (when (cordis--bailed-p value)
          (throw 'cordis-serial value))))
    nil))

(defun cordis-ctx-parallel (ctx event &rest args)
  "Dispatch EVENT in `parallel' mode with ARGS.
Every listener is invoked; listener errors are collected and, when any
occur, signalled as `cordis-aggregate-error'.  This C0 kernel has no true
concurrency: fan-out is sequential, but the observable contract (all
listeners invoked, all errors collected) is preserved."
  (let ((errors nil))
    (dolist (listener (cordis--dispatch ctx event 'parallel))
      (condition-case err
          (cordis--invoke-listener listener args)
        (error (push err errors))))
    (when errors
      (signal 'cordis-aggregate-error (nreverse errors)))))

(defun cordis-ctx-waterfall (ctx event &rest args)
  "Dispatch EVENT in `waterfall' mode with ARGS.
The last element of ARGS is the default implementation (as in Cordis);
listeners receive the remaining arguments plus a NEXT function.  Calling
NEXT delegates to the next listener and returns its value; returning without
calling NEXT short-circuits the chain."
  (let* ((listeners (cordis--dispatch ctx event 'waterfall))
         (default (car (last args)))
         (payload (butlast args))
         (callbacks listeners))
    (unless (functionp default)
      (signal 'cordis-error
              (list "waterfall requires a default function as last argument")))
    (cl-labels ((next (&rest xs)
                  (if (null callbacks)
                      (apply default xs)
                    (let ((listener (pop callbacks)))
                      (cordis--invoke-listener
                       listener
                       (append xs (list #'next)))))))
      (apply #'next payload))))

(defun cordis-ctx-listener-count (ctx event &optional mode)
  "Return the number of active listeners for EVENT (optionally MODE) from CTX."
  (length (seq-filter
           (lambda (listener)
             (and (cordis-listener-active listener)
                  (or (null mode)
                      (eq (cordis-listener-mode listener) mode))))
           (gethash event (cordis--event-table ctx)))))

;;;; Convenience registration helpers (built on effects)

(defun cordis-add-hook (_ctx hook function &optional depth local)
  "Add FUNCTION to HOOK and register its removal as an effect of CTX.
DEPTH and LOCAL are passed to `add-hook'.  Return the wrapper function that
was actually installed."
  (let* ((buffer (and local (current-buffer)))
         (wrapper (lambda (&rest args) (apply function args))))
    (if local
        (add-hook hook wrapper depth t)
      (add-hook hook wrapper depth))
    (cordis-register-disposer
     (lambda ()
       (if (and local buffer (buffer-live-p buffer))
           (with-current-buffer buffer
             (remove-hook hook wrapper t))
         (remove-hook hook wrapper)))
     (format "hook %S" hook))
    wrapper))

(defun cordis-advice-add (_ctx symbol where function &optional props)
  "Add FUNCTION as advice on SYMBOL at WHERE and register its removal.
WHERE is passed to `advice-add' (:before, :after, :around, ...)."
  (advice-add symbol where function props)
  (cordis-register-disposer
   (lambda () (advice-remove symbol function))
   (format "advice %S" symbol))
  function)

(defun cordis-run-at-time (_ctx time repeat function)
  "Run FUNCTION at TIME, repeating every REPEAT, as an effect of CTX.
Return the timer.  The timer is cancelled when the effect is disposed and a
stale callback that was already queued is ignored."
  (let* ((active t)
         (timer (run-at-time
                 time repeat
                 (lambda (&rest args)
                   (when active (apply function args))))))
    (cordis-register-disposer
     (lambda ()
       (setq active nil)
       (cancel-timer timer))
     "timer")
    timer))

(defun cordis-make-overlay (_ctx beg end &optional buffer &rest args)
  "Create an overlay in BUFFER and delete it when CTX's effect is disposed.
ARGS are passed to `make-overlay'."
  (let ((overlay (apply #'make-overlay beg end buffer args)))
    (cordis-register-disposer
     (lambda () (delete-overlay overlay))
     "overlay")
    overlay))

(defun cordis-setq-local (_ctx symbol value &optional buffer)
  "Set buffer-local SYMBOL to VALUE in BUFFER and revert it on disposal.
When SYMBOL was not buffer-local before, disposal kills the local binding
and the buffer default becomes visible again; otherwise the old local value
is restored."
  (let* ((buf (or buffer (current-buffer)))
         (was-local (local-variable-p symbol buf))
         (old (and was-local (buffer-local-value symbol buf))))
    (with-current-buffer buf
      (set (make-local-variable symbol) value))
    (cordis-register-disposer
     (lambda ()
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (if was-local
               (set (make-local-variable symbol) old)
             (kill-local-variable symbol)))))
     (format "buffer-local %S" symbol))
    value))

(defun cordis-define-key (_ctx keymap key definition)
  "Bind KEY to DEFINITION in KEYMAP and restore the old binding on disposal."
  (let ((old (lookup-key keymap key)))
    (define-key keymap key definition)
    (cordis-register-disposer
     (lambda () (define-key keymap key old))
     "key binding")
    definition))

(defun cordis-make-process (_ctx &rest args)
  "Call `make-process' with ARGS and kill the process on disposal.
Return the process object."
  (let ((process (apply #'make-process args)))
    (cordis-register-disposer
     (lambda ()
       (when (process-live-p process)
         (delete-process process)))
     "process")
    process))

;;;; Declarative loader (minimal C0 loader)

(defun cordis--entry-plugin (entry)
  "Return the plugin name declared by loader ENTRY."
  (or (plist-get entry :plugin) (plist-get entry :name)))

(defun cordis--entry-enabled-p (ctx entry)
  "Return non-nil when ENTRY is enabled in CTX.
A :disabled value may be nil, t, or a function of CTX."
  (let ((disabled (plist-get entry :disabled)))
    (not (if (functionp disabled)
             (funcall disabled ctx)
           disabled))))

(defun cordis-load (ctx entry)
  "Mount loader ENTRY under CTX and return its fiber, or nil if disabled.
ENTRY is a plist with :id, :plugin (or :name), :config and :disabled.
The :id is stored in the root's loader table and is used by
`cordis-reload'/`cordis-unload'."
  (let ((id (plist-get entry :id))
        (plugin (cordis--entry-plugin entry)))
    (unless id
      (signal 'cordis-error (list "loader entry is missing :id")))
    (unless plugin
      (signal 'cordis-invalid-plugin (list "loader entry is missing :plugin")))
    (when (cordis--entry-enabled-p ctx entry)
      (when (gethash id (cordis-context-loader (cordis-context-root ctx)))
        (signal 'cordis-error (list (format "duplicate loader id %S" id))))
      (let ((fiber (cordis-plugin ctx plugin (plist-get entry :config))))
        (puthash id fiber
                 (cordis-context-loader (cordis-context-root ctx)))
        fiber))))

(defun cordis-unload (ctx id)
  "Unload loader entry ID from CTX and dispose its fiber."
  (let* ((root (cordis-context-root ctx))
         (fiber (gethash id (cordis-context-loader root))))
    (when fiber
      (remhash id (cordis-context-loader root))
      (cordis-fiber-dispose fiber))))

(defun cordis-reload (ctx id &optional config)
  "Reload loader entry ID under CTX, optionally replacing its CONFIG."
  (let* ((root (cordis-context-root ctx))
         (old (gethash id (cordis-context-loader root))))
    (unless old
      (signal 'cordis-error (list (format "unknown loader id %S" id))))
    (let ((plugin (cordis-fiber-plugin old))
          (new-config (or config (cordis-fiber-config old))))
      (remhash id (cordis-context-loader root))
      (cordis-fiber-dispose old)
      (cordis-load ctx (list :id id :plugin plugin :config new-config)))))

(defun cordis-load-tree (ctx entries)
  "Mount each loader ENTRY in ENTRIES and return their fibers.
Entries are mounted in the given order; activation order is governed by
inject dependencies, not by list order."
  (delq nil (mapcar (lambda (entry) (cordis-load ctx entry)) entries)))

;;;; Introspection

(defun cordis-fiber-describe (fiber)
  "Return a diagnostic plist for FIBER."
  (list :id (cordis-fiber-id fiber)
        :plugin (cordis-fiber-plugin fiber)
        :state (cordis-fiber-state fiber)
        :inject (cordis-fiber-inject fiber)
        :epoch (cordis-fiber-epoch fiber)
        :provided (copy-sequence (cordis-fiber-provided fiber))
        :children (mapcar #'cordis-fiber-id (cordis-fiber-children fiber))
        :effects (cordis-fiber-effect-tree fiber)))

(defun cordis-context-describe (ctx)
  "Return a diagnostic plist for CTX."
  (list :name (cordis-context-name ctx)
        :depth (cordis--ctx-depth ctx)
        :isolate-map (copy-tree (cordis-context-isolate-map ctx))))

;;;; Tests
;; Run: emacs -Q --batch -L lisp -l lisp/cordis.el \
;;        --eval "(ert-run-tests-batch-and-exit)"
(defvar cordis-test-log nil
  "Scratch log used by the tests.")

(defun cordis-test--plugin (inject apply &optional config isolate)
  "Register and return an anonymous test plugin.
INJECT is the service list, APPLY a function of (CTX CONFIG), CONFIG optional
default configuration, ISOLATE an optional alist NAME -> LABEL."
  (let ((name (gensym "cordis-test-plugin-")))
    (cordis-plugin-register name :inject inject :apply apply :config config
                            :isolate isolate)
    name))

(defun cordis-test--mount (root plugin &optional config)
  "Mount PLUGIN under ROOT with CONFIG."
  (cordis-plugin root plugin config))

(defun cordis-test--host (root register)
  "Mount a listener host plugin under ROOT.
REGISTER is called as (REGISTER CTX) during apply.  Return (FIBER . CTX)."
  (let ((ctx nil))
    (let* ((plugin (cordis-test--plugin
                    nil
                    (lambda (c _)
                      (setq ctx c)
                      (funcall register c))))
           (fiber (cordis-plugin root plugin)))
      (cons fiber ctx))))


(with-eval-after-load 'ert

 ;;;; G1: effect LIFO, idempotence, teardown guard

  (ert-deftest cordis-test-g1-effect-lifo-and-idempotent ()
    "G1: disposers run LIFO; disposing twice is a no-op."
    (let* ((root (cordis-create-root "g1"))
           (log nil)
           (effect nil)
           (plugin (cordis-test--plugin
                    nil
                    (lambda (ctx _)
                      (setq effect
                            (cordis-effect ctx "outer"
                                           (cordis-register-disposer
                                            (lambda () (setq log (append log (list 'a)))) "a")
                                           (cordis-register-disposer
                                            (lambda () (setq log (append log (list 'b)))) "b")
                                           (cordis-register-disposer
                                            (lambda () (setq log (append log (list 'c)))) "c")))))))
      (let ((fiber (cordis-test--mount root plugin)))
        (should (eq (cordis-fiber-state fiber) 'active))
        (cordis-dispose-effect effect)
        (should (equal log '(c b a)))
        (cordis-dispose-effect effect)
        (should (equal log '(c b a)))
        (cordis-fiber-dispose fiber))))

  (ert-deftest cordis-test-g2-register-during-teardown-signals ()
    "G2: registering an effect while unloading signals `cordis-inactive-effect'."
    (let* ((root (cordis-create-root "g2"))
           (caught nil)
           (plugin (cordis-test--plugin
                    nil
                    (lambda (ctx _)
                      (cordis-effect ctx "outer"
                                     (cordis-register-disposer
                                      (lambda ()
                                        (condition-case err
                                            (cordis-effect ctx "nested" (ignore))
                                          (cordis-inactive-effect
                                           (setq caught (car err)))))
                                      "teardown-probe"))))))
      (let ((fiber (cordis-test--mount root plugin)))
        (should (eq (cordis-fiber-state fiber) 'active))
        (cordis-fiber-dispose fiber)
        (should (eq caught 'cordis-inactive-effect))
        (should (eq (cordis-fiber-state fiber) 'disposed)))))

  (ert-deftest cordis-test-g3-effect-error-rolls-back ()
    "G3: a failing effect body rolls back its collected disposers, then rethrows."
    (let* ((root (cordis-create-root "g3"))
           (log nil)
           (caught nil)
           (plugin (cordis-test--plugin
                    nil
                    (lambda (ctx _)
                      (condition-case err
                          (cordis-effect ctx "outer"
                                         (cordis-register-disposer
                                          (lambda () (setq log (append log (list 'rolled-back)))) "r")
                                         (error "boom"))
                        (error (setq caught err)))))))
      (let ((fiber (cordis-test--mount root plugin)))
        (should (eq (cordis-fiber-state fiber) 'active))
        (should (equal log '(rolled-back)))
        (should (equal (error-message-string caught) "boom")))))

;;;; G4: resource helpers are fully reverted

  (defun cordis-test--advice-target (x)
    "Named target used to test advice registration and removal."
    x)

  (ert-deftest cordis-test-g4-resource-helpers-cleanup ()
    "G4: hook/advice/timer/overlay/buffer-local/key/process are reverted."
    (let* ((root (cordis-create-root "g4"))
           (hook (make-symbol "cordis-test-hook"))
           (keymap (make-sparse-keymap))
           (baseline (lookup-key keymap [3 116]))
           (target 'cordis-test--advice-target)
           (advice-fn (lambda (orig &rest args) (apply orig args)))
           (buffer (generate-new-buffer " *cordis-test-g4*"))
           (local-var (make-symbol "cordis-test-local"))
           (process nil)
           (timer nil)
           (overlay nil)
           (plugin (cordis-test--plugin
                    nil
                    (lambda (ctx _)
                      (cordis-add-hook ctx hook #'ignore)
                      (cordis-advice-add ctx target :around advice-fn)
                      (setq timer (cordis-run-at-time ctx 60 nil #'ignore))
                      (setq overlay (cordis-make-overlay ctx 1 2 buffer))
                      (cordis-setq-local ctx local-var 42 buffer)
                      (cordis-define-key ctx keymap [3 116] #'ignore)
                      (when (executable-find "sleep")
                        (setq process
                              (cordis-make-process
                               ctx :name "cordis-test-g4"
                               :command '("sleep" "30") :noquery t)))))))
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (insert "abc")
              (goto-char (point-min)))
            (let ((fiber (cordis-test--mount root plugin)))
              (should (eq (cordis-fiber-state fiber) 'active))
              (should (= 1 (length (symbol-value hook))))
              (should (advice-member-p advice-fn target))
              (should (timerp timer))
              (should (overlayp overlay))
              (should (with-current-buffer buffer
                        (= 1 (length (overlays-in (point-min) (point-max))))))
              (should (local-variable-p local-var buffer))
              (should (eq (lookup-key keymap [3 116]) #'ignore))
              (when process
                (should (process-live-p process)))
              (cordis-fiber-dispose fiber)
              (should (= 0 (length (symbol-value hook))))
              (should-not (advice-member-p advice-fn target))
              (should-not (memq timer timer-list))
              (should (with-current-buffer buffer
                        (null (overlays-in (point-min) (point-max)))))
              (should-not (local-variable-p local-var buffer))
              (should (equal (lookup-key keymap [3 116]) baseline))
              (when process
                (should-not (process-live-p process)))))
        (when (buffer-live-p buffer) (kill-buffer buffer)))))

;;;; G5: inject keeps consumers pending until providers are active

  (ert-deftest cordis-test-g5-inject-pending ()
    "G5: a consumer stays pending, then applies exactly once."
    (let* ((root (cordis-create-root "g5"))
           (applied 0)
           (seen nil)
           (consumer (cordis-test--plugin
                      '(greeter)
                      (lambda (ctx _)
                        (cl-incf applied)
                        (setq seen (cordis-ctx-get ctx 'greeter)))))
           (provider (cordis-test--plugin
                      nil
                      (lambda (ctx _)
                        (cordis-ctx-provide ctx 'greeter "hello")))))
      (let ((consumer-fiber (cordis-test--mount root consumer)))
        (should (eq (cordis-fiber-state consumer-fiber) 'pending))
        (should (= applied 0))
        (let ((provider-fiber (cordis-test--mount root provider)))
          (should (eq (cordis-fiber-state provider-fiber) 'active))
          (should (eq (cordis-fiber-state consumer-fiber) 'active))
          (should (= applied 1))
          (should (equal seen "hello"))))))

;;;; G6: provider final unregistration happens after dependents unload

  (ert-deftest cordis-test-g6-provider-ordering ()
    "G6: consumer unloads before the provider record is removed."
    (let* ((root (cordis-create-root "g6"))
           (log nil)
           (retract-fn (lambda (_record)
                         (setq log (append log (list 'provider-unregistered)))))
           (provider (cordis-test--plugin
                      nil
                      (lambda (ctx _)
                        (cordis-ctx-provide ctx 'svc "v")
                        (cordis-register-disposer
                         (lambda ()
                           (setq log (append log (list 'provider-effect))))
                         "provider-effect"))))
           (consumer (cordis-test--plugin
                      '(svc)
                      (lambda (_ctx _)
                        (cordis-register-disposer
                         (lambda ()
                           (setq log (append log (list 'consumer-down))))
                         "consumer")))))
      (add-hook 'cordis-service-retract-functions retract-fn)
      (unwind-protect
          (let ((consumer-fiber (cordis-test--mount root consumer)))
            (let ((provider-fiber (cordis-test--mount root provider)))
              (should (eq (cordis-fiber-state consumer-fiber) 'active))
              (cordis-fiber-dispose provider-fiber)
              (should (equal log '(provider-effect
                                   consumer-down
                                   provider-unregistered)))
              (should (eq (cordis-fiber-state consumer-fiber) 'pending))))
        (remove-hook 'cordis-service-retract-functions retract-fn))))

;;;; G7: replacing a provider reloads dependents

  (ert-deftest cordis-test-g7-provider-replacement ()
    "G7: a new provider with a different uid reloads the consumer."
    (let* ((root (cordis-create-root "g7"))
           (applied 0)
           (seen nil)
           (consumer (cordis-test--plugin
                      '(svc)
                      (lambda (ctx _)
                        (cl-incf applied)
                        (push (cordis-ctx-get ctx 'svc) seen))))
           (provider-one (cordis-test--plugin
                          nil
                          (lambda (ctx _)
                            (cordis-ctx-provide ctx 'svc "one"))))
           (provider-two (cordis-test--plugin
                          nil
                          (lambda (ctx _)
                            (cordis-ctx-provide ctx 'svc "two")))))
      (let ((consumer-fiber (cordis-test--mount root consumer)))
        (let ((first (cordis-test--mount root provider-one)))
          (should (eq (cordis-fiber-state consumer-fiber) 'active))
          (should (= applied 1))
          (should (equal seen '("one")))
          (cordis-fiber-dispose first)
          (should (eq (cordis-fiber-state consumer-fiber) 'pending))
          (let ((_second (cordis-test--mount root provider-two)))
            (should (eq (cordis-fiber-state consumer-fiber) 'active))
            (should (= applied 2))
            (should (equal seen '("two" "one"))))))))

;;;; G8: service check predicates are re-evaluated

  (ert-deftest cordis-test-g8-service-check ()
    "G8: a false CHECK unloads dependents; true re-activates them."
    (let* ((root (cordis-create-root "g8"))
           (available t)
           (applied 0)
           (consumer (cordis-test--plugin
                      '(svc)
                      (lambda (_ctx _) (cl-incf applied))))
           (provider (cordis-test--plugin
                      nil
                      (lambda (ctx _)
                        (cordis-ctx-provide ctx 'svc "v"
                                            (lambda () available))))))
      (let ((consumer-fiber (cordis-test--mount root consumer)))
        (let ((_provider-fiber (cordis-test--mount root provider)))
          (should (eq (cordis-fiber-state consumer-fiber) 'active))
          (setq available nil)
          (cordis-notify-service root 'svc)
          (should (eq (cordis-fiber-state consumer-fiber) 'pending))
          (setq available t)
          (cordis-notify-service root 'svc)
          (should (eq (cordis-fiber-state consumer-fiber) 'active))
          (should (= applied 2))))))

;;;; G9: parent disposes its child plugins first

  (ert-deftest cordis-test-g9-parent-disposes-child-first ()
    "G9: child plugin effects run before the parent's own effects."
    (let* ((root (cordis-create-root "g9"))
           (log nil)
           (child (cordis-test--plugin
                   nil
                   (lambda (_ctx _)
                     (cordis-register-disposer
                      (lambda () (setq log (append log (list 'child)))) "child"))))
           (parent (cordis-test--plugin
                    nil
                    (lambda (ctx _)
                      (cordis-plugin ctx child)
                      (cordis-register-disposer
                       (lambda () (setq log (append log (list 'parent)))) "parent")))))
      (let ((fiber (cordis-test--mount root parent)))
        (should (eq (cordis-fiber-state fiber) 'active))
        (cordis-fiber-dispose fiber)
        (should (equal log '(child parent))))))

;;;; G10: five event dispatch modes

  (ert-deftest cordis-test-g10-emit-order-and-prepend ()
    "G10: emit calls every listener in order, prepend first."
    (let* ((root (cordis-create-root "g10-emit"))
           (log nil)
           (host (cordis-test--host
                  root
                  (lambda (ctx)
                    (cordis-ctx-on ctx 'ev
                                   (lambda () (setq log (append log (list 'first)))))
                    (cordis-ctx-on ctx 'ev
                                   (lambda () (setq log (append log (list 'second)))))
                    (cordis-ctx-on ctx 'ev
                                   (lambda () (setq log (append log (list 'pre))))
                                   :prepend t)))))
      (cordis-ctx-emit (cdr host) 'ev)
      (should (equal log '(pre first second)))
      (cordis-fiber-dispose (car host))))

  (ert-deftest cordis-test-g10-bail ()
    "G10: bail stops at the first non-false value; :json-false is false."
    (let* ((root (cordis-create-root "g10-bail"))
           (log nil)
           (result nil)
           (host (cordis-test--host
                  root
                  (lambda (ctx)
                    (cordis-ctx-on ctx 'ev
                                   (lambda ()
                                     (setq log (append log (list 'one)))
                                     nil)
                                   :mode 'bail)
                    (cordis-ctx-on ctx 'ev (lambda () :json-false) :mode 'bail)
                    (cordis-ctx-on ctx 'ev (lambda () 'found) :mode 'bail)
                    (cordis-ctx-on ctx 'ev
                                   (lambda ()
                                     (setq log (append log (list 'three))))
                                   :mode 'bail)))))
      (setq result (cordis-ctx-bail (cdr host) 'ev))
      (should (eq result 'found))
      (should (equal log '(one)))
      (cordis-fiber-dispose (car host))))

  (ert-deftest cordis-test-g10-serial ()
    "G10: serial runs listeners in order and stops at a bail value."
    (let* ((root (cordis-create-root "g10-serial"))
           (log nil)
           (result nil)
           (host (cordis-test--host
                  root
                  (lambda (ctx)
                    (cordis-ctx-on ctx 'ev
                                   (lambda ()
                                     (setq log (append log (list 'one)))
                                     nil)
                                   :mode 'serial)
                    (cordis-ctx-on ctx 'ev
                                   (lambda ()
                                     (setq log (append log (list 'two)))
                                     'stop)
                                   :mode 'serial)
                    (cordis-ctx-on ctx 'ev
                                   (lambda ()
                                     (setq log (append log (list 'three))))
                                   :mode 'serial)))))
      (setq result (cordis-ctx-serial (cdr host) 'ev))
      (should (eq result 'stop))
      (should (equal log '(one two)))
      (cordis-fiber-dispose (car host))))

  (ert-deftest cordis-test-g10-parallel-aggregates-errors ()
    "G10: parallel calls every listener and aggregates errors."
    (let* ((root (cordis-create-root "g10-parallel"))
           (ran nil)
           (host (cordis-test--host
                  root
                  (lambda (ctx)
                    (cordis-ctx-on ctx 'ev (lambda () (error "first"))
                                   :mode 'parallel)
                    (cordis-ctx-on ctx 'ev (lambda () (setq ran t))
                                   :mode 'parallel)))))
      (should-error (cordis-ctx-parallel (cdr host) 'ev)
                    :type 'cordis-aggregate-error)
      (should ran)
      (cordis-fiber-dispose (car host))))

  (ert-deftest cordis-test-g10-waterfall ()
    "G10: waterfall delegates, wraps, and short-circuits correctly."
    (let* ((root (cordis-create-root "g10-waterfall"))
           (log nil)
           (result nil)
           (host (cordis-test--host
                  root
                  (lambda (ctx)
                    (cordis-ctx-on
                     ctx 'ev
                     (lambda (value next)
                       (push (list 'first value) log)
                       (funcall next (concat value "-first")))
                     :mode 'waterfall)
                    (cordis-ctx-on
                     ctx 'ev
                     (lambda (value _next)
                       (push (list 'second value) log)
                       (concat value "-second"))
                     :mode 'waterfall)))))
      (setq result
            (cordis-ctx-waterfall
             (cdr host) 'ev "x"
             (lambda (value) (push (list 'default value) log) value)))
      (should (equal result "x-first-second"))
      (should (equal (nreverse log) '((first "x") (second "x-first"))))
      (cordis-fiber-dispose (car host))))

  (ert-deftest cordis-test-g10-waterfall-short-circuit-and-prepend ()
    "G10: a prepended waterfall listener can short-circuit the chain."
    (let* ((root (cordis-create-root "g10-waterfall-2"))
           (log nil)
           (result nil)
           (host (cordis-test--host
                  root
                  (lambda (ctx)
                    (cordis-ctx-on
                     ctx 'ev
                     (lambda (value next)
                       (funcall next (concat value "-normal")))
                     :mode 'waterfall)
                    (cordis-ctx-on
                     ctx 'ev
                     (lambda (value _next)
                       (push 'prepend log)
                       (concat value "-short"))
                     :mode 'waterfall :prepend t)))))
      (setq result
            (cordis-ctx-waterfall
             (cdr host) 'ev "x"
             (lambda (value) (concat value "-default"))))
      (should (equal result "x-short"))
      (should (equal log '(prepend)))
      (cordis-fiber-dispose (car host))))

;;;; G11: isolation realms

  (ert-deftest cordis-test-g11-isolate-realms ()
    "G11: identical service names with different labels stay separate."
    (let* ((root (cordis-create-root "g11"))
           (cell-a (list nil))
           (cell-b (list nil))
           (make-provider
            (lambda (value label)
              (cordis-test--plugin
               nil
               (lambda (ctx _)
                 (cordis-ctx-provide ctx 'svc value))
               nil
               (list (cons 'svc label)))))
           (make-consumer
            (lambda (label cell)
              (cordis-test--plugin
               '(svc)
               (lambda (ctx _)
                 (setcar cell (cordis-ctx-get ctx 'svc)))
               nil
               (list (cons 'svc label))))))
      (let ((consumer-a (cordis-test--mount root
                                            (funcall make-consumer "realm-a" cell-a)))
            (consumer-b (cordis-test--mount root
                                            (funcall make-consumer "realm-b" cell-b))))
        (should (eq (cordis-fiber-state consumer-a) 'pending))
        (should (eq (cordis-fiber-state consumer-b) 'pending))
        (let ((_provider-a (cordis-test--mount root
                                               (funcall make-provider "A" "realm-a")))
              (_provider-b (cordis-test--mount root
                                               (funcall make-provider "B" "realm-b"))))
          (should (eq (cordis-fiber-state consumer-a) 'active))
          (should (eq (cordis-fiber-state consumer-b) 'active))
          (should (equal (car cell-a) "A"))
          (should (equal (car cell-b) "B"))))))

;;;; Additional semantics: errors, duplicate provide, loader, config

  (ert-deftest cordis-test-missing-inject-signals ()
    "`cordis-ctx-get' signals when the caller did not declare INJECT."
    (let* ((root (cordis-create-root "missing-inject"))
           (caught nil)
           (plugin (cordis-test--plugin
                    nil
                    (lambda (ctx _)
                      (condition-case err
                          (cordis-ctx-get ctx 'svc)
                        (cordis-missing-inject
                         (setq caught (car err))))))))
      (cordis-test--mount root plugin)
      (should (eq caught 'cordis-missing-inject))))

  (ert-deftest cordis-test-duplicate-service-fails-fiber ()
    "Providing the same service twice fails the second fiber, not the root."
    (let* ((root (cordis-create-root "duplicate"))
           (provider (cordis-test--plugin
                      nil
                      (lambda (ctx _)
                        (cordis-ctx-provide ctx 'svc "v")))))
      (cordis-test--mount root provider)
      (let ((second (cordis-test--mount root provider)))
        (should (eq (cordis-fiber-state second) 'failed))
        (should (eq (car (cordis-fiber-failed-error second))
                    'cordis-duplicate-service)))))

  (ert-deftest cordis-test-loader-mount-and-unload ()
    "The declarative loader mounts, disposes and removes entries."
    (let* ((root (cordis-create-root "loader"))
           (applied 0)
           (plugin (cordis-test--plugin
                    nil
                    (lambda (_ctx _) (cl-incf applied))))
           (fiber (cordis-load root (list :id 'one :plugin plugin))))
      (should (eq (cordis-fiber-state fiber) 'active))
      (should (= applied 1))
      (should (eq (gethash 'one (cordis-context-loader root)) fiber))
      (cordis-unload root 'one)
      (should (eq (cordis-fiber-state fiber) 'disposed))
      (should-not (gethash 'one (cordis-context-loader root)))))

  (ert-deftest cordis-test-config-defaults ()
    "Runtime config overrides plugin config defaults."
    (let* ((root (cordis-create-root "config"))
           (seen nil)
           (plugin (cordis-test--plugin
                    nil
                    (lambda (_ctx config) (setq seen config))
                    '(:retries 3 :timeout 60))))
      (cordis-test--mount root plugin '(:retries 10))
      (should (= (plist-get seen :retries) 10))
      (should (= (plist-get seen :timeout) 60)))))


(provide 'cordis)
;;; cordis.el ends here
