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

(defconst javaimp--basedir (file-name-directory load-file-name))

(defconst javaimp--classlike-scope-types
  '(class interface enum))

(defconst javaimp--all-scope-types
  (append
   '(anonymous-class
     array
     method
     simple-statement
     statement
     array
     unknown)
   javaimp--classlike-scope-types))

(defconst javaimp--help-scope-type-abbrevs
  '((anonymous-class . "ac")
    (statement . "st")
    (simple-statement . "ss")
    (array . "ar")
    (unknown . "un")
    (method . "me")
    (class . "cl")
    (interface . "in")
    (enum . "en")))

(defvar javaimp-tool-output-buf-name "*javaimp-tool-output*"
  "Name of the buffer to which `javaimp--call-build-tool' copies
build tool output.  Can be let-bound to nil to suppress copying.")



;; Structs

(cl-defstruct javaimp-node
  parent children contents)

(cl-defstruct javaimp-module
  id parent-id
  file
  file-orig
  ;; Artifact final name, may be relative to build dir
  final-name
  source-dirs build-dir
  dep-jars
  load-ts
  ;; Function to retrieve DEP-JARS for MODULE, called with two
  ;; arguments: MODULE and list of parent IDs.  Should return a list
  ;; of strings - jar file names.
  dep-jars-fetcher
  raw                                   ;used only during parsing
  )

(cl-defstruct javaimp-id
  group artifact version)

(cl-defstruct javaimp-cached-jar        ;jar or jmod
  file read-ts classes)

(cl-defstruct javaimp-scope
  type                                  ; see javaimp--all-scope-types
  name
  start
  open-brace
  parent)



;; Xml

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



;; Scopes

(defun javaimp--copy-scope (scope)
  "Recursively copies SCOPE and its parents."
  (let* ((res (copy-javaimp-scope scope))
         (tmp res)
         orig-parent)
    (while (setq orig-parent (javaimp-scope-parent tmp))
      (setf (javaimp-scope-parent tmp) (copy-javaimp-scope orig-parent))
      (setq tmp (javaimp-scope-parent tmp)))
    res))

(defun javaimp--filter-scope-parents (scope pred)
  "Rewrite SCOPE's parents so that only those matching PRED are
left."
  (while scope
    (let ((parent (javaimp-scope-parent scope)))
      (if (and parent
               (not (funcall pred parent)))
          ;; leave out this parent
          (setf (javaimp-scope-parent scope) (javaimp-scope-parent parent))
        (setq scope (javaimp-scope-parent scope))))))

(defun javaimp--concat-scope-parents (scope)
  (let (parents)
    (while (setq scope (javaimp-scope-parent scope))
      (push scope parents))
    (mapconcat #'javaimp-scope-name parents ".")))

(defsubst javaimp-test-scope-type (scope leaf-types parent-types)
  (declare (indent 1))
  (let ((res (memq (javaimp-scope-type scope) leaf-types)))
    (while (and res
                (setq scope (javaimp-scope-parent scope)))
      (setq res (memq (javaimp-scope-type scope) parent-types)))
    res))


;; Tree

(defun javaimp--build-tree (this all child-p &optional parent-node sort-pred)
  "Recursively builds tree for element THIS and its children.
Children are those elements from ALL for which CHILD-P invoked
with this element and tested element returns non-nil.  Children
are sorted by SORT-PRED, if given.  PARENT-NODE is indented for
recursive calls."
  (let ((children (seq-filter (apply-partially child-p this)
                              all)))
    (if sort-pred
        (setq children (sort children sort-pred)))
    (let* ((this-node (make-javaimp-node
		       :parent parent-node
		       :children nil
		       :contents this))
	   (child-nodes
	    (mapcar (lambda (child)
		      (javaimp--build-tree
                       child all child-p this-node sort-pred))
		    children)))
      (setf (javaimp-node-children this-node) child-nodes)
      this-node)))

(defun javaimp--find-node (contents-pred forest &optional unwrap)
  "Return first node for which CONTENTS-PRED returns non-nil.  If
UNWRAP is non-nil, then node contents is returned."
  (catch 'found
    (dolist (tree forest)
      (javaimp--find-node-in-tree tree contents-pred unwrap))))

(defun javaimp--find-node-in-tree (tree contents-pred unwrap)
  (when tree
    (if (funcall contents-pred (javaimp-node-contents tree))
	(throw 'found
               (if unwrap
                   (javaimp-node-contents tree)
                 tree)))
    (dolist (child (javaimp-node-children tree))
      (javaimp--find-node-in-tree child contents-pred unwrap))))


(defun javaimp--collect-nodes (contents-pred forest)
  "Return all nodes' contents for which CONTENTS-PRED returns
non-nil."
  (apply #'seq-concatenate 'list
	 (mapcar (lambda (tree)
                   (delq nil
		         (javaimp--collect-nodes-from-tree tree contents-pred)))
		 forest)))

(defun javaimp--collect-nodes-from-tree (tree contents-pred)
  (when tree
    (cons (and (funcall contents-pred (javaimp-node-contents tree))
               (javaimp-node-contents tree))
	  (apply #'seq-concatenate 'list
		 (mapcar (lambda (child)
                           (delq nil
			         (javaimp--collect-nodes-from-tree child contents-pred)))
			 (javaimp-node-children tree))))))


(defun javaimp--map-nodes (function pred forest)
  "Recursively applies FUNCTION to each node's contents in FOREST
and returns new tree.  FUNCTION should return (t . VALUE) if the
result for this node should be made a list of the form (VALUE
. CHILDREN), or (nil . VALUE) for plain VALUE as the result (in
this case children are discarded).  The result for each node is
additionally tested by PRED."
  (delq nil
        (mapcar (lambda (tree)
                  (javaimp--map-nodes-from-tree tree function pred))
                forest)))

(defun javaimp--map-nodes-from-tree (tree function pred)
  (when tree
    (let* ((cell (funcall function (javaimp-node-contents tree)))
           (res
            (if (car cell)
                (let ((children
                       (delq nil
                             (mapcar (lambda (child)
                                       (javaimp--map-nodes-from-tree
                                        child function pred))
                                     (javaimp-node-children tree)))))
                  (cons (cdr cell) children))
              (cdr cell))))
      (and (funcall pred res)
           res))))

(defun javaimp--get-root (node)
  (while (javaimp-node-parent node)
    (setq node (javaimp-node-parent node)))
  node)



;; Other

(defsubst javaimp-print-id (id)
  (format "%s:%s:%s"
          (javaimp-id-artifact id)
          (javaimp-id-group id)
          (javaimp-id-version id)))

(defsubst javaimp--get-file-ts (file)
  (nth 5 (file-attributes file)))

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
  "Run PROGRAM with ARGS, then call HANDLER in the temporary buffer
with point set to eob and return its result."
  (message "Calling program: %s %s" program (string-join args " "))
  (with-temp-buffer
    (let ((status
           (let ((coding-system-for-read
                  (when (eq system-type 'cygwin) 'utf-8-dos))
                 (process-environment
                  (cons (format "JAVA_HOME=%s" javaimp-java-home)
                        process-environment)))
             (apply #'process-file program nil t nil args)))
	  (buf (current-buffer)))
      (when javaimp-tool-output-buf-name
        (with-current-buffer (get-buffer-create
                              javaimp-tool-output-buf-name)
          (setq buffer-read-only nil)
	  (erase-buffer)
	  (insert-buffer-substring buf)
          (setq buffer-read-only t)))
      (unless (and (numberp status) (= status 0))
        (when javaimp-tool-output-buf-name
          (display-buffer javaimp-tool-output-buf-name))
	(error "%s exit status: %s" program status))
      (goto-char (point-min))
      (funcall handler))))

(defun javaimp--split-native-path (path)
  (when path
    ;; don't use parse-colon-path because it makes resulting elements
    ;; to be directories
    (split-string (javaimp-cygpath-convert-maybe path 'unix t)
                  (concat "[" path-separator "\n]+")
                  t)))

(provide 'javaimp-util)
