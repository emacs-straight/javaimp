;;; javaimp-util.el --- javaimp util  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(require 'xml)
(require 'cl-lib)
(require 'seq)

(defcustom javaimp-cygpath-program
  (if (eq system-type 'cygwin) "cygpath")
  "Path to the `cygpath' program (Cygwin only).  Customize it if
the program is not on `exec-path'."
  :group 'javaimp
  :type 'string)

(defconst javaimp-debug-buf-name "*javaimp-debug*")

(defconst javaimp--basedir (file-name-directory load-file-name))

;; Structs

(cl-defstruct javaimp-node
  parent children contents)

(cl-defstruct javaimp-module
  id parent-id
  file
  file-orig
  final-name                           ;may be relative (to build-dir)
  source-dirs build-dir
  dep-jars
  load-ts
  dep-jars-path-fetcher
  raw                                   ;used only during parsing
  )

(cl-defstruct javaimp-id
  group artifact version)

(cl-defstruct javaimp-cached-jar        ;jar or jmod
  file read-ts classes)


(defun javaimp--xml-children (xml-tree child-name)
  "Returns list of children of XML-TREE filtered by CHILD-NAME"
  (seq-filter (lambda (child)
		(and (consp child)
		     (eq (car child) child-name)))
	      (cddr xml-tree)))

(defun javaimp--xml-child (name el)
  "Returns a child of EL named by symbol NAME"
  (assq name (cddr el)))

(defun javaimp--xml-first-child (el)
  "Returns a first child of EL"
  (car (cddr el)))


(defun javaimp--get-file-ts (file)
  (nth 5 (file-attributes file)))

(defun javaimp-print-id (id)
  (format "%s:%s:%s"
          (javaimp-id-artifact id)
          (javaimp-id-group id)
          (javaimp-id-version id)))


;; TODO use functions `cygwin-convert-file-name-from-windows' and
;; `cygwin-convert-file-name-to-windows' when they are available
;; instead of calling `cygpath'.  See
;; https://cygwin.com/ml/cygwin/2013-03/msg00228.html

(defun javaimp-cygpath-convert-maybe (path &optional mode is-really-path)
  "On Cygwin, converts PATH using cygpath according to MODE and
IS-REALLY-PATH.  If MODE is `unix' (the default), adds -u switch.
If MODE is `windows', adds -m switch.  If `is-really-path' is
non-nil, adds `-p' switch.  On other systems, PATH is returned
unchanged."
  (if (and path (eq system-type 'cygwin))
      (progn
	(unless mode (setq mode 'unix))
	(let (args)
	  (push (cond ((eq mode 'unix) "-u")
		      ((eq mode 'windows) "-m")
		      (t (error "Invalid mode: %s" mode)))
		args)
	  (and is-really-path (push "-p" args))
	  (push path args)
	  (car (apply #'process-lines javaimp-cygpath-program args))))
    path))


(defun javaimp--call-build-tool (program handler &rest args)
  "Runs PROGRAM with ARGS, then calls HANDLER in the temporary
buffer and returns its result"
  (message "Calling %s on args: %s" program args)
  (with-temp-buffer
    (let ((status (let ((coding-system-for-read
                         (if (eq system-type 'cygwin) 'utf-8-dos)))
                    ;; TODO check  in output on Gnu/Linux
                    (apply #'process-file program nil t nil args)))
	  (buf (current-buffer)))
      (with-current-buffer (get-buffer-create javaimp-debug-buf-name)
	(erase-buffer)
	(insert-buffer-substring buf))
      (or (and (numberp status) (= status 0))
	  (error "\"%s\" failed with status \"%s\"" program status))
      (goto-char (point-min))
      (funcall handler))))

(defun javaimp--split-native-path (path)
  (when path
    ;; don't use parse-colon-path because it makes resulting elements
    ;; to be directories
    (split-string (javaimp-cygpath-convert-maybe path 'unix t)
                  (concat "[" path-separator "\n]+")
                  t)))

(defun javaimp--build-tree (this parent-node all)
  (message "Building tree for module: %s" (javaimp-print-id (javaimp-module-id this)))
  (let ((children
	 ;; more or less reliable way to find children is to look for
	 ;; modules with "this" as the parent
	 (seq-filter (lambda (m)
		       (equal (javaimp-module-parent-id m) (javaimp-module-id this)))
		     all)))
    (let* ((this-node (make-javaimp-node
		       :parent parent-node
		       :children nil
		       :contents this))
	   ;; recursively build child nodes
	   (child-nodes
	    (mapcar (lambda (child)
		      (javaimp--build-tree child this-node all))
		    children)))
      (setf (javaimp-node-children this-node) child-nodes)
      this-node)))


;; Java source parsing

(defun javaimp--get-package ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward "^\\s *package\\s +\\([^;]+\\)\\s *;" nil t)
          (let ((state (syntax-ppss)))
            (unless (syntax-ppss-context state)
              (throw 'found (match-string 1)))))))))

(defun javaimp--get-file-classes (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((parse-sexp-ignore-comments t)
          (class-re (concat
                     (regexp-opt '("class" "interface" "enum") 'words)
                     (rx (and (+ (syntax whitespace))
                              (group (+ (any alnum ?_)))))))
          res)
      (while (re-search-forward class-re nil t)
        (let ((state (syntax-ppss))
              curr)
          (unless (syntax-ppss-context state)
            (setq curr (list (match-string 2)))
            ;; collect enclosing classes, if any
            (save-excursion
              (catch 'stop
                (while (nth 1 state)
                  ;; find innermost enclosing open-bracket
                  (goto-char (nth 1 state))
                  (if (and (= (char-after) ?{)
                           (re-search-backward class-re nil t)
                           ;; if there's no paren in between - assume
                           ;; it's a valid class (not a method - this
                           ;; way we exclude local classes)
                           (not (save-match-data
                                  (search-forward "(" (nth 1 state) t))))
                      (progn
                        (push (match-string 2) curr)
                        (setq state (syntax-ppss)))
                    (setq curr nil)
                    (throw 'stop nil)))))
            (when curr
              (let ((package (javaimp--get-package)))
                (if package (push package curr)))
              (push (mapconcat #'identity curr ".") res)))))
      (nreverse res))))

(provide 'javaimp-util)
