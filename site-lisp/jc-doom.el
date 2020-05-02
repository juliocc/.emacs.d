;;; jc-doom.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(defun doom-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.

If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list
         (delq ,(if fetcher
                    `(funcall ,fetcher ,elt ,list)
                  elt)
               ,list)))


(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))


(defun doom--resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (doom-enlist (doom-unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defun doom--setq-hook-fns (hooks rest &optional singles)
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
                             vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook in (doom--resolve-hook-forms hooks)
           for mode = (string-remove-suffix "-hook" (symbol-name hook))
           append
           (cl-loop for (var . val) in vars
                    collect
                    (list var val hook
                          (intern (format "doom--setq-%s-for-%s-h"
                                          var mode))))))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

If N and M = 1, there's no benefit to using this macro over `add-hook'.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
     unquoted list of modes, a quoted hook variable or a quoted list of hook
     variables.
  2. Optional properties :local and/or :append, which will make the hook
     buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be one function, a quoted list
     thereof, a list of `defun's, or body forms (implicitly wrapped in a
     lambda).

\(fn HOOKS [:append :local] FUNCTIONS)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (doom--resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p
         local-p
         remove-p
         forms)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (let ((first (car-safe (car rest))))
      (cond ((null first)
             (setq func-forms rest))

            ((eq first 'defun)
             (setq func-forms (mapcar #'cadr rest)
                   defn-forms rest))

            ((memq first '(quote function))
             (setq func-forms
                   (if (cdr rest)
                       (mapcar #'doom-unquote rest)
                     (doom-enlist (doom-unquote (car rest))))))

            ((setq func-forms (list `(lambda (&rest _) ,@rest)))))
      (dolist (hook hook-forms)
        (dolist (func func-forms)
          (push (if remove-p
                    `(remove-hook ',hook #',func ,local-p)
                  `(add-hook ',hook #',func ,append-p ,local-p))
                forms)))
      (macroexp-progn
       (append defn-forms
               (if append-p
                   (nreverse forms)
                 forms))))))

(defvar doom-disabled-packages ()
  "A list of packages that should be ignored by `use-package!' and `after!'.")

(setq doom-disabled-packages nil)

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.
PACKAGE is a symbol or list of them. These are package names, not modes,
functions or variables. It can be:
- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
  Without :or/:any/:and/:all, :and/:all are implied.
This is a wrapper around `eval-after-load' that:
1. Suppresses warnings for disabled packages at compile-time
2. No-ops for package that are disabled by the user (via `package!')
3. Supports compound package statements (see below)
4. Prevents eager expansion pulling in autoloaded macros all at once"
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (unless (memq package (bound-and-true-p doom-disabled-packages))
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              (let ((body (macroexp-progn body)))
                `(if (featurep ',package)
                     ,body
                   ;; We intentionally avoid `with-eval-after-load' to prevent
                   ;; eager macro expansion from pulling (or failing to pull) in
                   ;; autoloaded macros/packages.
                   (eval-after-load ',package ',body)))))
    (let ((p (car package)))
      (cond ((not (keywordp p))
             `(after! (:and ,@package) ,@body))
            ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (cdr package))
               (setq body `((after! ,next ,@body))))
             (car body))))))

(defvar doom--transient-counter 0)
(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then never
again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        ;; Avoid `make-symbol' and `gensym' here because an interned symbol is
        ;; easier to debug in backtraces (and is visible to `describe-function')
        (fn (intern (format "doom--transient-%d-h" (cl-incf doom--transient-counter)))))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" (doom-unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append))))))


(defmacro setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (doom--setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest _)
                       ,(format "%s = %s" var (pp-to-string val))
                       (setq-local ,var ,val))
            collect `(remove-hook ',hook #',fn) ; ensure set order
            collect `(add-hook ',hook #',fn))))


(defvar doom--deferred-packages-alist '(t))
(with-eval-after-load 'use-package-core
  ;; ;; Macros are already fontified, no need for this
  ;; (font-lock-remove-keywords 'emacs-lisp-mode use-package-font-lock-keywords)

  ;; ;; We define :minor and :magic-minor from the `auto-minor-mode' package here
  ;; ;; so we don't have to load `auto-minor-mode' so early.
  ;; (dolist (keyword '(:minor :magic-minor))
  ;;   (setq use-package-keywords
  ;;         (use-package-list-insert keyword use-package-keywords :commands)))

  ;; (defalias 'use-package-normalize/:minor #'use-package-normalize-mode)
  ;; (defun use-package-handler/:minor (name _ arg rest state)
  ;;   (use-package-handle-mode name 'auto-minor-mode-alist arg rest state))

  ;; (defalias 'use-package-normalize/:magic-minor #'use-package-normalize-mode)
  ;; (defun use-package-handler/:magic-minor (name _ arg rest state)
  ;;   (use-package-handle-mode name 'auto-minor-mode-magic-alist arg rest state))

  ;; Adds two keywords to `use-package' to expand its lazy-loading capabilities:
  ;;
  ;;   :after-call SYMBOL|LIST
  ;;   :defer-incrementally SYMBOL|LIST|t
  ;;
  ;; Check out `use-package!'s documentation for more about these two.
  ;; (dolist (keyword '(:defer-incrementally :after-call))
  (dolist (keyword '(:after-call))
     (push keyword use-package-deferring-keywords)
     (setq use-package-keywords
           (use-package-list-insert keyword use-package-keywords :after)))

  ;; (defalias 'use-package-normalize/:defer-incrementally #'use-package-normalize-symlist)
  ;; (defun use-package-handler/:defer-incrementally (name _keyword targets rest state)
  ;;   (use-package-concat
  ;;    `((doom-load-packages-incrementally
  ;;       ',(if (equal targets '(t))
  ;;             (list name)
  ;;           (append targets (list name)))))
  ;;    (use-package-process-keywords name rest state)))

  (defalias 'use-package-normalize/:after-call #'use-package-normalize-symlist)
  (defun use-package-handler/:after-call (name _keyword hooks rest state)
    (if (plist-get state :demand)
        (use-package-process-keywords name rest state)
      (let ((fn (make-symbol (format "doom--after-call-%s-h" name))))
        (use-package-concat
         `((fset ',fn
                 (lambda (&rest _)
                   ;(message "Loading deferred package %s from %s" ',name ',fn)
                   (condition-case e
                       (require ',name)
                     ((debug error)
                      (message "Failed to load deferred package %s: %s" ',name e)))
                   (when-let (deferral-list (assq ',name doom--deferred-packages-alist))
                     (dolist (hook (cdr deferral-list))
                       (advice-remove hook #',fn)
                       (remove-hook hook #',fn))
                     (delq! deferral-list doom--deferred-packages-alist)
                     (unintern ',fn nil)))))
         (let (forms)
           (dolist (hook hooks forms)
             (push (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
                       `(add-hook ',hook #',fn)
                     `(advice-add #',hook :before #',fn))
                   forms)))
         `((unless (assq ',name doom--deferred-packages-alist)
             (push '(,name) doom--deferred-packages-alist))
           (nconc (assq ',name doom--deferred-packages-alist)
                  '(,@hooks)))
         (use-package-process-keywords name rest state))))))


;; (defvar doom-incremental-packages '(t)
;;   "A list of packages to load incrementally after startup. Any large packages
;; here may cause noticable pauses, so it's recommended you break them up into
;; sub-packages. For example, `org' is comprised of many packages, and can be
;; broken up into:

;;   (doom-load-packages-incrementally
;;    '(calendar find-func format-spec org-macs org-compat
;;      org-faces org-entities org-list org-pcomplete org-src
;;      org-footnote org-macro ob org org-clock org-agenda
;;      org-capture))

;; This is already done by the lang/org module, however.

;; If you want to disable incremental loading altogether, either remove
;; `doom-load-packages-incrementally-h' from `emacs-startup-hook' or set
;; `doom-incremental-first-idle-timer' to nil. Incremental loading does not occur
;; in daemon sessions (they are loaded immediately at startup).")

;; (defvar doom-incremental-first-idle-timer 2.0
;;   "How long (in idle seconds) until incremental loading starts.

;; Set this to nil to disable incremental loading.")

;; (defvar doom-incremental-idle-timer 0.75
;;   "How long (in idle seconds) in between incrementally loading packages.")

;; (defun doom-load-packages-incrementally (packages &optional now)
;;   "Registers PACKAGES to be loaded incrementally.

;; If NOW is non-nil, load PACKAGES incrementally, in `doom-incremental-idle-timer'
;; intervals."
;;   (if (not now)
;;       (nconc doom-incremental-packages packages)
;;     (while packages
;;       (let ((req (pop packages)))
;;         (unless (featurep req)
;;           (message "Incrementally loading %s" req)
;;           (condition-case e
;;               (or (while-no-input
;;                     (let ((gc-cons-threshold most-positive-fixnum)
;;                           file-name-handler-alist)
;;                       (require req nil t))
;;                     t)
;;                   (push req packages))
;;             ((error debug)
;;              (message "Failed to load '%s' package incrementally, because: %s"
;;                       req e)))
;;           (if (not packages)
;;               (message "Finished incremental loading")
;;             (run-with-idle-timer doom-incremental-idle-timer
;;                                  nil #'doom-load-packages-incrementally
;;                                  packages t)
;;             (setq packages nil)))))))

;; (defun doom-load-packages-incrementally-h ()
;;   "Begin incrementally loading packages in `doom-incremental-packages'.

;; If this is a daemon session, load them all immediately instead."
;;   (if (daemonp)
;;       (mapc #'require (cdr doom-incremental-packages))
;;     (when (numberp doom-incremental-first-idle-timer)
;;       (run-with-idle-timer doom-incremental-first-idle-timer
;;                            nil #'doom-load-packages-incrementally
;;                            (cdr doom-incremental-packages) t))))

;; (add-hook 'emacs-startup-hook #'doom-load-packages-incrementally-h)

(defun doom-apply-ansi-color-to-compilation-buffer-h ()
  "Applies ansi codes to the compilation buffers. Meant for
`compilation-filter-hook'."
  (with-silent-modifications
    (ansi-color-apply-on-region compilation-filter-start (point))))

(provide 'jc-doom)
;;; jc-doom.el ends here
