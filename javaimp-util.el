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


(defun javaimp--rsb-outside-context (regexp &optional bound noerror count)
  "Like `re-search-backward', but count only occurences outside
syntactic context as given by `syntax-ppss-context'.  Assumes
point is outside of any context initially."
  (or count (setq count 1))
  (let ((step (if (>= count 0) 1 -1))
        res)
    (dotimes (iter (abs count) res)
      (while (and (setq res (re-search-backward regexp bound noerror step))
                  (syntax-ppss-context (syntax-ppss)))))))



;; Tree building & search

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

(defun javaimp--find-node (predicate forest)
  (catch 'found
    (dolist (tree forest)
      (javaimp--find-node-in-tree-1 tree predicate))))

(defun javaimp--find-node-in-tree-1 (tree predicate)
  (when tree
    (if (funcall predicate (javaimp-node-contents tree))
	(throw 'found tree))
    (dolist (child (javaimp-node-children tree))
      (javaimp--find-node-in-tree-1 child predicate))))


(defun javaimp--collect-nodes (predicate forest)
  (apply #'seq-concatenate 'list
	 (mapcar (lambda (tree)
		   (javaimp--collect-nodes-from-tree tree predicate))
		 forest)))

(defun javaimp--collect-nodes-from-tree (tree &optional predicate)
  (when tree
    (append (when (or (not predicate)
                      (funcall predicate (javaimp-node-contents tree)))
	      (list tree))
	    (apply #'seq-concatenate 'list
		   (mapcar (lambda (child)
			     (javaimp--collect-nodes-from-tree child predicate))
			   (javaimp-node-children tree))))))

(defun javaimp--get-root (node)
  (while (javaimp-node-parent node)
    (setq node (javaimp-node-parent node)))
  node)

(provide 'javaimp-util)
