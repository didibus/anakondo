;;; anakondo.el --- Adds clj-kondo based Clojure[Script] editing facilities.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Didier A.

;; Author: Didier A. <didibus@users.noreply.github.com>
;; URL: https://github.com/didibus/anakondo
;; Version: 0.1
;; Package-Requires: ((emacs "26.3") (json) (projectile))
;; Keywords: clojure, clojurescript, cljc, clj-kondo, completion

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package makes use of clj-kondo's analysis data to provide code editing
;; facilities related to Clojure, ClojureScript and cljc source.

;; See accompanying README file for more info: https://github.com/didibus/anakondo/blob/master/README.org

;;;; Installation

;; See accompanying README file for install instructions: https://github.com/didibus/anakondo/blob/master/README.org#Installation

;;;; Usage

;; See accompanying README file for usage instructions: https://github.com/didibus/anakondo/blob/master/README.org#Usage

;;;; Credits

;; See accompanying README file for credits: https://github.com/didibus/anakondo/blob/master/README.org#Credits

;;; License:

;; MIT License, see accompanying LICENSE file: https://github.com/didibus/anakondo/blob/master/LICENSE

;;; Code:

;;;; Requirements

(require 'json)
(require 'projectile)

;;;; Customization

(defgroup anakondo nil
  "Clojure, ClojureScript and cljc minor mode powered by clj-kondo."
  :group 'clojure)

(defcustom anakondo-minor-mode-lighter " k"
  "Text to display in the mode line when anakondo minor mode is on."
  :type 'string
  :group 'anakondo)

;;;; Variables

(defvar anakondo--cache nil
  "Cache where per-project clj-kondo analysis maps are stored.")

;;;;; Keymaps

(defvar anakondo-minor-mode-map
  ;; This makes it easy and much less verbose to define keys
  (let ((map (make-sparse-keymap "Anakondo minor mode map"))
        (maps (list
               ;; Mappings go here, e.g.:
               ;; "RET" #'package-name-RET-command
               ;; [remap search-forward] #'package-name-search-forward
               )))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    map)
  "Keymap used to specify key-bindings for anakondo minor mode.")

;;;; Macros

(defmacro anakondo--with-projectile-root (&rest body)
  "Invoke BODY with `root' bound to the projectile root.

Anaphoric macro, binds `root' implicitly."
  `(projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
     (let* ((root default-directory))
       ,@body)))

;;;; Functions

(defun anakondo--get-projectile-cache (root)
  "Return clj-kondo analysis cache for given project ROOT."
  (gethash root anakondo--cache))

(defun anakondo--set-projectile-cache (root root-cache)
  "Set given clj-kondo analysis ROOT-CACHE for given project ROOT."
  (puthash root root-cache anakondo--cache))

(defun anakondo--get-projectile-var-def-cache ()
  "Return cached var-definitions for current projectile project."
  (anakondo--with-projectile-root
   (gethash :var-def-cache (anakondo--get-projectile-cache root))))

(defun anakondo--get-projectile-ns-def-cache ()
  "Return cached ns-definitions for current projectile project."
  (anakondo--with-projectile-root
   (gethash :ns-def-cache (anakondo--get-projectile-cache root))))

(defun anakondo--get-projectile-ns-usage-cache ()
  "Return cached ns-usages for current projectile project."
  (anakondo--with-projectile-root
   (gethash :ns-usage-cache (anakondo--get-projectile-cache root))))

(defun anakondo--completion-symbol-bounds ()
  "Return bounds of symbol at point which needs completion.

Tries to infer start and end of Clojure symbol at point.

It is smart enough to skip number literals, strings, comments,
keywords, meta, tagged literals, Java fields and methods and
character literals.

It is smart enough to ignore quote, syntax quote, unquote,
unquote-splice and @ deref."
  (let* ((pt (point))
         (syntax (syntax-ppss))
         (skip-regex "a-zA-Z0-9*+!_'?<>=/.:^#\\\\-"))
    ;; Don't auto-complete inside strings or comments
    (unless (or (nth 3 syntax)          ;skip strings
                (nth 4 syntax))         ;skip comments
      (save-excursion
        (skip-chars-backward skip-regex)
        (let ((ch (char-after)))
          (unless (or (and ch (>= ch ?0) (<= ch ?9)) ;skip numbers
                      (and ch (= ch ?:)) ;skip keywords
                      (and ch (= ch ?\\)) ;skip chars
                      (and ch (= ch ?^)) ;skip meta
                      (and ch (= ch ?#)) ;skip tagged literal
                      (and ch (= ch ?.))) ;skip . at start as reserved by Clojure
            (when (and ch (= ch ?'))
              (forward-char))
            (setq pt (point))
            (skip-chars-forward skip-regex)
            (cons pt (point))))))))

(defun anakondo--get-buffer-lang ()
  "Return the current buffer detected Clojure language.

Used when calling clj-kondo `--lang' argument.

Return nil if Clojure not detected."
  (if buffer-file-name
      (file-name-extension buffer-file-name)
    (pcase major-mode
      ('clojure-mode "clj")
      ('clojurec-mode "cljc")
      ('clojurescript-mode "cljs"))))

(defun anakondo--clj-kondo-analyse-sync (path default-lang)
  "Return clj-kondo's analysis data as a hash-map of lists and keywords.

Is synchronous, and will block Emacs until done.

PATH is the value passed to clj-kondo's `--lint' option. It can be a path to a file,
directory or classpath. In the case of a directory or classpath, only .clj, .cljs and
.cljc will be processed. Use `-' as path for having it analyze current buffer.

DEFAULT-LANG is the value passed to clj-kondo's `--lang' option. If lang cannot be
derived from the file extension this option will be used."
  (let* ((buffer "*anakondo*")
         (analysis-key :analysis)
         (kondo-command (concat "clj-kondo --lint '" path
                                "' --config '{:output {:analysis true :format :json}}'"))
         (kondo-command (if default-lang
                            (concat kondo-command " --lang '" default-lang "'")
                          kondo-command)))
    (unwind-protect
        (let* ((_ (call-shell-region nil nil
                                     kondo-command
                                     nil buffer))
               (json-object-type 'hash-table)
               (json-array-type 'list)
               (json-key-type 'keyword)
               (kondo-result-hashmap (with-current-buffer buffer
                                       (goto-char (point-min))
                                       (json-read))))
          (gethash analysis-key kondo-result-hashmap))
      (when (get-buffer buffer)
        (kill-buffer buffer)))))

(defun anakondo--get-project-path ()
  "Return the path to `--lint' for clj-kondo in current projectile project.

It uses Clojure's `tools.deps' to get the project's classpath."
  ;; TODO: add support for lein, boot, and default to directory otherwise
  (shell-command-to-string "clojure -Spath"))

(defun anakondo--string->keyword (str)
  "Convert STR to an interned keyword symbol."
  (when str
    (intern (concat ":" str))))

(defun anakondo--upsert-var-def-cache (var-def-cache-table var-defs &optional invalidation-ns)
  "Update or insert into VAR-DEF-CACHE-TABLE the clj-kondo var-definitions from VAR-DEFS.

INVALIDATION-NS : optional, can be a keyword of the namespace to invalidate before updating.
                  This means it'll replace the cached var-definitions for that namespace
                  instead of merging it in. This is useful when we want to remove var-definitions
                  no longer present in the source code from the cache."
  (when invalidation-ns
    (remhash invalidation-ns var-def-cache-table))
  (seq-reduce
   (lambda (hash-table var-def)
     (let* ((key (anakondo--string->keyword (gethash :ns var-def)))
            (curr-val (gethash key hash-table))
            (var-def-key (anakondo--string->keyword (gethash :name var-def))))
       (if curr-val
           (progn
             (puthash var-def-key var-def curr-val)
             (puthash key curr-val hash-table))
         (let* ((new-curr-val (make-hash-table)))
           (puthash var-def-key var-def new-curr-val)
           (puthash key new-curr-val hash-table)))
       hash-table))
   var-defs
   var-def-cache-table))

(defun anakondo--upsert-ns-def-cache (ns-def-cache-table ns-defs)
  "Update or insert into NS-DEF-CACHE-TABLE the clj-kondo ns-definitions from NS-DEFS."
  (seq-reduce
   (lambda (hash-table ns-def)
     (let* ((key (anakondo--string->keyword (gethash :name ns-def))))
       (puthash key ns-def hash-table)
       hash-table))
   ns-defs
   ns-def-cache-table))

(defun anakondo--upsert-ns-usage-cache (ns-usage-cache-table ns-usages &optional invalidation-ns)
  "Update or insert into NS-USAGE-CACHE-TABLE the clj-kondo ns-usages from NS-USAGES.

INVALIDATION-NS : optional, can be a keyword of the namespace to invalidate before updating.
                  This means it'll replace the cached ns-usages for that namespace
                  instead of merging it in. This is useful when we want to remove ns-usages
                  no longer present in the source code from the cache."
  (when invalidation-ns
    (remhash invalidation-ns ns-usage-cache-table))
  (seq-reduce
   (lambda (hash-table ns-usage)
     (let* ((key (anakondo--string->keyword (gethash :from ns-usage)))
            (curr-val (gethash key hash-table))
            (ns-usage-key (anakondo--string->keyword (gethash :to ns-usage))))
       (if curr-val
           (progn
             (puthash ns-usage-key ns-usage curr-val)
             (puthash key curr-val hash-table))
         (let* ((new-curr-val (make-hash-table)))
           (puthash ns-usage-key ns-usage new-curr-val)
           (puthash key new-curr-val hash-table)))
       hash-table))
   ns-usages
   ns-usage-cache-table))

(defun anakondo--clj-kondo-projectile-analyse-sync (var-def-cache-table ns-def-cache-table ns-usage-cache-table)
  "Analyze synchronously the current project and upsert the analysis result into the given VAR-DEF-CACHE-TABLE, NS-DEF-CACHE-TABLE and NS-USAGE-CACHE-TABLE.

It is synchronous and will block Emacs, but it will message the user of the work
it is doing, so they are aware Emacs is not frozen, but busy analyzing their project."
  (message "Analysing project for completion...")
  (anakondo--with-projectile-root
   (let* ((kondo-analyses (anakondo--clj-kondo-analyse-sync (anakondo--get-project-path) (anakondo--get-buffer-lang)))
          (var-defs (gethash :var-definitions kondo-analyses))
          (ns-defs (gethash :namespace-definitions kondo-analyses))
          (ns-usages (gethash :namespace-usages kondo-analyses)))
     (anakondo--upsert-var-def-cache var-def-cache-table var-defs)
     (anakondo--upsert-ns-def-cache ns-def-cache-table ns-defs)
     (anakondo--upsert-ns-usage-cache ns-usage-cache-table ns-usages)
     (message "Analysing project for completion...done")
     root)))

(defun anakondo--clj-kondo-buffer-analyse-sync (var-def-cache-table ns-def-cache-table ns-usage-cache-table)
  "Analyze synchronously the current buffer and upsert the analysis result into the given VAR-DEF-CACHE-TABLE, NS-DEF-CACHE-TABLE and NS-USAGE-CACHE-TABLE.

It is synchronous and will block Emacs, but should be fast enough we don't bother messaging the user.
Also, this is called by completion-at-point, which for company-mode, means it is called on every
keystroke that qualifies for completion, and messaging was excessive in that case."
  (let* ((kondo-analyses (anakondo--clj-kondo-analyse-sync "-" (anakondo--get-buffer-lang)))
         (var-defs (gethash :var-definitions kondo-analyses))
         (ns-defs (gethash :namespace-definitions kondo-analyses))
         (ns-usages (gethash :namespace-usages kondo-analyses))
         (curr-ns-def (car ns-defs))
         ;; Default to user namespace when there is no namespace defined in the buffer
         (curr-ns (if curr-ns-def
                      (anakondo--string->keyword (gethash :name curr-ns-def))
                    :user)))
    (anakondo--upsert-var-def-cache var-def-cache-table var-defs curr-ns)
    (anakondo--upsert-ns-def-cache ns-def-cache-table ns-defs)
    (anakondo--upsert-ns-usage-cache ns-usage-cache-table ns-usages curr-ns)
    curr-ns))

(defun anakondo--safe-hash-table-values (hash-table)
  "Like `hash-table-values', but return nil instead of signaling an error when HASH-TABLE is nil."
  (when hash-table
    (hash-table-values hash-table)))

(defun anakondo--get-completion-candidates ()
  "Return a candidate list compatible with completion-at-point for current symbol at point.

How it works:
1. It gets the current namespace by analyzing the current buffer with clj-kondo.
2. As it analyses the current buffer with clj-kondo, it will also take this opportunity
   to upsert the result back into the analysis cache of the current project.
3. It will then grab from the current project's caches the vars from the current namespace
   and the vars from all namespaces it requires, as well as the list of all available
   namespaces and join them all into out candidates list.
4. It'll properly prefix the alias or the namespace qualifier for Vars from the required
   namespaces. If there is an alias, it uses the alias, else the namespace qualifier.
5. Does not support refer yet.
6. Does not support keywords yet.
7. Does not support locals yet."
  (let* ((var-def-cache (anakondo--get-projectile-var-def-cache))
         (ns-def-cache (anakondo--get-projectile-ns-def-cache))
         (ns-usage-cache (anakondo--get-projectile-ns-usage-cache))
         (curr-ns (anakondo--clj-kondo-buffer-analyse-sync var-def-cache ns-def-cache ns-usage-cache)))
    (append
     (mapcar
      (lambda (var-def)
        (gethash :name var-def))
      (append
       (anakondo--safe-hash-table-values (gethash curr-ns var-def-cache))
       (anakondo--safe-hash-table-values (gethash :clojure.core var-def-cache))))
     (mapcar
      (lambda (ns-def)
        (gethash :name ns-def))
      (anakondo--safe-hash-table-values ns-def-cache))
     (seq-mapcat
      (lambda (ns-usage)
        (let* ((ns-name (gethash :to ns-usage))
               (alias (gethash :alias ns-usage))
               (ns-qualifier (or alias ns-name))
               (ns-key (anakondo--string->keyword ns-name))
               (ns-var-names (mapcar
                              (lambda (var-def)
                                (gethash :name var-def))
                              (anakondo--safe-hash-table-values (gethash ns-key var-def-cache)))))
          (mapcar
           (lambda (var-name)
             (concat ns-qualifier "/" var-name))
           ns-var-names)))
      (anakondo--safe-hash-table-values (gethash curr-ns ns-usage-cache))))))

(defun anakondo-completion-at-point ()
  "Return a `completion-at-point' list for use with `completion-at-point-functions' generated from clj-kondo's analysis."
  (let* ((bounds (anakondo--completion-symbol-bounds))
         (start (car bounds))
         (end (cdr bounds)))
    (when bounds
      (list
       start
       end
       (completion-table-with-cache
        (lambda (_)
          (anakondo--get-completion-candidates)))))))

(defun anakondo--init-projectile-cache (root)
  "Initialize clj-kondo analysis cache of caches for given ROOT, if it isn't already.

This includes performing initial clj-kondo project wide analysis and upserting it
into the newly initialized cache.

Cache looks like:
{root {:var-def-cache {ns {var {var-def-map}}}
       :ns-def-cache {ns {ns-def-map}}
       :ns-usage-cache {ns {to-ns {ns-usage-map}}}}}"
  (unless anakondo--cache
    (setq anakondo--cache (make-hash-table :test 'equal)))
  (let* ((root-cache (anakondo--get-projectile-cache root)))
    (if (not root-cache)
        (let* ((root-cache (make-hash-table))
               (var-def-cache (make-hash-table))
               (ns-def-cache (make-hash-table))
               (ns-usage-cache (make-hash-table)))
          (puthash :var-def-cache var-def-cache root-cache)
          (puthash :ns-def-cache ns-def-cache root-cache)
          (puthash :ns-usage-cache ns-usage-cache root-cache)
          (anakondo--set-projectile-cache root root-cache)
          (anakondo--clj-kondo-projectile-analyse-sync var-def-cache ns-def-cache ns-usage-cache))
      (let* ((var-def-cache (anakondo--get-projectile-var-def-cache))
             (ns-def-cache (anakondo--get-projectile-ns-def-cache))
             (ns-usage-cache (anakondo--get-projectile-ns-usage-cache)))
        (anakondo--clj-kondo-buffer-analyse-sync var-def-cache ns-def-cache ns-usage-cache)))))

(defun anakondo--delete-projectile-cache (root)
  "Delete the cache for the given ROOT project, releasing its memory."
  (when anakondo--cache
    (when (anakondo--get-projectile-cache root)
      (remhash root anakondo--cache))))

;;;;; Commands

;;;###autoload
(define-minor-mode anakondo-minor-mode
  "Minor mode for Clojure[Script] completion powered by clj-kondo.

Toggle anakondo-minor-mode on or off.

With a prefix argument ARG, enable anakondo-minor-mode if ARG is
positive, and disable it otherwise. If called from Lisp, enable
the mode if ARG is omitted or nil, and toggle it if ARG is ‘toggle’."
  nil
  anakondo-minor-mode-lighter
  anakondo-minor-mode-map
  (if anakondo-minor-mode
      (anakondo--minor-mode-enter)
    (anakondo--minor-mode-exit)))

(defun anakondo-refresh-project-cache ()
  "Refresh the anakondo project analysis cache.

Run this command if you feel anakondo is out-of-sync with your project source.
Will not pick up changes to source which have not been saved. So you might want
to save your buffers first.

Runs synchronously, and might take a few seconds for big projects."
  (interactive)
  (anakondo--minor-mode-guard)
  (let* ((var-def-cache (anakondo--get-projectile-var-def-cache))
         (ns-def-cache (anakondo--get-projectile-ns-def-cache))
         (ns-usage-cache (anakondo--get-projectile-ns-usage-cache)))
    (anakondo--clj-kondo-projectile-analyse-sync var-def-cache ns-def-cache ns-usage-cache)))

;;;;; Support

(defun anakondo--minor-mode-enter ()
  "Setup `anakondo-minor-mode' in current buffer."
  (add-hook 'completion-at-point-functions #'anakondo-completion-at-point nil t)
  (anakondo--with-projectile-root
   (anakondo--init-projectile-cache root)))

(defun anakondo--minor-mode-exit ()
  "Tear down `anakondo-minor-mode' in current buffer."
  (remove-hook 'completion-at-point-functions #'anakondo-completion-at-point t)
  (anakondo--with-projectile-root
   (anakondo--delete-projectile-cache root)))

(defun anakondo--minor-mode-guard ()
  "Signal an error when `anakondo-minor-mode' is not on in current buffer."
  (unless anakondo-minor-mode
    (error "Anakondo minor mode not on in current buffer")))

;;;; Footer

(provide 'anakondo)

;;; anakondo.el ends here
