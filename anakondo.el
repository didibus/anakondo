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

;; Currently, it gives you contextual auto-complete using completion-at-point,
;; which works without a REPL connected, since it relies on clj-kondo's static
;; source analysis. It will list out all available Vars, Ns and Aliases in a
;; given buffer.

;; It supports the notion of projects, using projectile. So it will allow you
;; to auto-complete required dependencies' Vars using the following logic:
;;
;; 1. It will try to get your classpath using Clojure's tools.deps in the context of your
;;    projectile root, which it will use afterwards for auto-completion.

;;;; Installation

;; To be determined.

;;;; Usage

;; Simply activate `anakondo-minor-mode' in a Clojure, ClojureScript or Clojurec buffer.

;;;; Credits

;; This package would not have been possible without the following
;; packages: clj-kondo[1], which does all the heavy lifting, projectile[2],
;; which gives Emacs a common notion of projects, tools.deps[3], which lets
;; me find the classpath effortlessly, and json[4] which lets me parse
;; clj-kondo's analysis file inside Emacs.
;;
;;  [1] https://github.com/borkdude/clj-kondo
;;  [2] https://github.com/bbatsov/projectile
;;  [3] https://github.com/clojure/tools.deps.alpha
;;  [4] https://github.com/ryancrum/json.el

;;; License:

;; MIT License, see accompanying LICENSE file: https://github.com/didibus/anakondo/blob/master/LICENSE

;;; Code:

;;;; Requirements

(require 'json)
(require 'projectile)

;;;; Customization

;;;; Variables

(defvar anakondo--cache nil)

;;;;; Keymaps

(defvar anakondo-map
  ;; This makes it easy and much less verbose to define keys
  (let ((map (make-sparse-keymap "anakondo map"))
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
    map))

;;;; Functions

(defun anakondo--get-projectile-cache (root)
  (gethash root anakondo--cache))

(defun anakondo--set-projectile-cache (root root-cache)
  (puthash root root-cache anakondo--cache))

(defun anakondo--get-projectile-var-def-cache ()
  (anakondo--with-projectile-root
   (gethash :var-def-cache (anakondo--get-projectile-cache root))))

(defun anakondo--get-projectile-ns-def-cache ()
  (anakondo--with-projectile-root
   (gethash :ns-def-cache (anakondo--get-projectile-cache root))))

(defun anakondo--get-projectile-ns-usage-cache ()
  (anakondo--with-projectile-root
   (gethash :ns-usage-cache (anakondo--get-projectile-cache root))))

(defun anakondo--completion-symbol-bounds ()
  (let ((pt (point))
        (syntax (syntax-ppss)))
    ;; Don't auto-complete inside strings or comments
    (unless (or (nth 3 syntax)          ;skip strings
                (nth 4 syntax))         ;skip comments
      (save-excursion
        (skip-chars-backward "a-zA-Z0-9*+!_'?<>=/.:\-")
        (let ((ch (char-after)))
          (unless (or (and ch (>= ch ?0) (<= ch ?9)) ;skip numbers
                      (and ch (= ch ?:)) ;skip keywords
                      (and ch (= ch ?.))) ;skip . at start as reserved by Clojure
            (when (or (= ch ?'))
              (forward-char))
            (setq pt (point))
            (skip-chars-forward "a-zA-Z0-9*+!_'?<>=/.:\-")
            (cons pt (point))))))))

(defun anakondo--get-buffer-lang ()
  "Return the current buffer detected Clojure language to pass
to clj-kondo --lang argument, nil if Clojure not detected."
  (if buffer-file-name
      (file-name-extension buffer-file-name)
    (pcase major-mode
      ('clojure-mode "clj")
      ('clojurec-mode "cljc")
      ('clojurescript-mode "cljs"))))

(defmacro anakondo--with-projectile-root (&rest body)
  "Invoke body with `root` bound to the projectile root."
  `(projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
     (let* ((root default-directory))
       ,@body)))

(defun anakondo--clj-kondo-analyse-sync (path default-lang)
  "Returns clj-kondo's analysis data as a hash-map of lists and keywords."
  (let* ((buffer "*clj-kondo*")
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
               (kondo-result-json (with-current-buffer buffer
                                    (buffer-string)))
               (kondo-result-hashmap (json-read-from-string kondo-result-json)))
          (gethash analysis-key kondo-result-hashmap))
      (when (get-buffer buffer)
        (kill-buffer buffer)))))

(defun anakondo--get-projectile-path ()
  "Returns the path to --lint for clj-kondo within projectile's project by trying
to use clojure tools.deps first to get the classpath of the project, lein otherwise,
or fallback to the project root otherwise."
  ;; TODO: finish finding path logic
  (shell-command-to-string "clojure -Spath"))

(defun anakondo--string->keyword (str)
  (when str
    (intern (concat ":" str))))

(defun anakondo--upsert-var-def-cache (var-def-cache-table var-defs &optional invalidation-ns)
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
  (seq-reduce
   (lambda (hash-table ns-def)
     (let* ((key (anakondo--string->keyword (gethash :name ns-def))))
       (puthash key ns-def hash-table)
       hash-table))
   ns-defs
   ns-def-cache-table))

(defun anakondo--upsert-ns-usage-cache (ns-usage-cache-table ns-usages &optional invalidation-ns)
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
  "Returns clj-kondo analyses data for projectile project."
  (message "Analysing project for completion...")
  (anakondo--with-projectile-root
   (let* ((kondo-analyses (anakondo--clj-kondo-analyse-sync (anakondo--get-projectile-path) (anakondo--get-buffer-lang)))
          (var-defs (gethash :var-definitions kondo-analyses))
          (ns-defs (gethash :namespace-definitions kondo-analyses))
          (ns-usages (gethash :namespace-usages kondo-analyses)))
     (anakondo--upsert-var-def-cache var-def-cache-table var-defs)
     (anakondo--upsert-ns-def-cache ns-def-cache-table ns-defs)
     (anakondo--upsert-ns-usage-cache ns-usage-cache-table ns-usages)
     (message "Analysing project for completion...done")
     root)))

(defun anakondo--clj-kondo-buffer-analyse-sync (var-def-cache-table ns-def-cache-table ns-usage-cache-table)
  "Returns clj-kondo analyses data for current buffer."
  (message "Analysing buffer for completion...")
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
    (message "Analysing buffer for completion...done")
    curr-ns))

(defun anakondo--safe-hash-table-values (hash-table)
  (when hash-table
    (hash-table-values hash-table)))

(defun anakondo--completion-at-point ()
  (let* ((var-def-cache (anakondo--get-projectile-var-def-cache))
         (ns-def-cache (anakondo--get-projectile-ns-def-cache))
         (ns-usage-cache (anakondo--get-projectile-ns-usage-cache))
         (curr-ns (anakondo--clj-kondo-buffer-analyse-sync var-def-cache ns-def-cache ns-usage-cache))
         (candidates (append
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
                       (anakondo--safe-hash-table-values (gethash curr-ns ns-usage-cache)))))
         (bounds (anakondo--completion-symbol-bounds))
         (start (car bounds))
         (end (cdr bounds)))
    (when bounds
      (list
       start
       end
       candidates))))

(defun anakondo--init-projectile-cache (root)
  (when (not anakondo--cache)
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
  (when anakondo--cache
    (when (anakondo--get-projectile-cache root)
      (remhash root anakondo--cache))))

;;;;; Commands

;;;###autoload
(define-minor-mode anakondo-minor-mode
  "Minor mode for Clojure[Script] completion powered by clj-kondo."
  nil
  "k"
  anakondo-map
  (if anakondo-minor-mode
      (anakondo--minor-mode-enter)
    (anakondo--minor-mode-exit)))

;;;;; Support

(defun anakondo--minor-mode-enter ()
  (add-hook 'completion-at-point-functions 'anakondo--completion-at-point nil t)
  (anakondo--with-projectile-root
   (anakondo--init-projectile-cache root)))

(defun anakondo--minor-mode-exit ()
  (remove-hook 'completion-at-point-functions 'anakondo--completion-at-point)
  (anakondo--with-projectile-root
   (anakondo--delete-projectile-cache root)))

;;;; Footer

(provide 'anakondo)

;;; anakondo.el ends here
