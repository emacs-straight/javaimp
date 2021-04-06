;;; javaimp-maven.el --- javaimp maven support  -*- lexical-binding: t; -*-

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

(require 'javaimp-util)

(defcustom javaimp-mvn-program "mvn"
  "Path to the `mvn' program.  Customize it if the program is not
on `exec-path'."
  :group 'javaimp
  :type 'string)


(defun javaimp--maven-visit (file)
  "Calls \"mvn help:effective-pom\" on FILE,
reads project structure from the output and records which files
belong to which modules and other module information.  Returns
resulting module trees."
  (message "Visiting Maven POM file %s..." file)
  (let* ((xml-tree (javaimp--call-build-tool
                    javaimp-mvn-program
                    #'javaimp--maven-effective-pom-handler
                    "-f" (javaimp-cygpath-convert-maybe file)
                    "help:effective-pom"))
	 (projects (or (if (assq 'project xml-tree)
                           (list (assq 'project xml-tree))
                         (javaimp--xml-children (assq 'projects xml-tree) 'project))
                       (user-error "No projects found")))
	 (modules (mapcar (lambda (proj-elt)
                            (javaimp--maven-module-from-xml proj-elt file))
                          projects)))

    ;; Set :file slots in a separate step because Maven doesn't seem
    ;; to give pom.xml file location for each subproject.  We build
    ;; tree below by using child->parent links (<parent> in child
    ;; pom.xml), that also doesn't help to find pom.xml itself.
    ;; Furthermore, the reverse link (<modules> in parent) may not
    ;; correspond to a project's real children.  So we take this
    ;; approach: starting from the top-level pom.xml, descend into
    ;; children using <modules>.  For each child pom.xml, find its
    ;; corresponding module by id in the whole module list, and set
    ;; that module's :file.
    (javaimp--maven-fill-modules-files file modules)
    (when-let ((without-files
                (seq-filter (lambda (m)
                              (null (javaimp-module-file m)))
                            modules)))
      (error "Cannot find file for module(s): %s"
             (mapconcat (lambda (m)
                          (javaimp-print-id (javaimp-module-id m)))
                        without-files
                        ", ")))
    (dolist (m modules)
      (setf (javaimp-module-raw m) nil)) ;no longer need it

    (let ((roots
           (cons
            (car modules)               ;first module is always root
            ;; In some rare cases, a project inside a tree has
            ;; parent outside the tree (examples: "log4j-bom" in
            ;; log4j-2.11.2, "external" in jaxb-ri-2.3.3).  Such
            ;; projects didn't get it into the tree, and we need
            ;; to add them a separate roots.
            (seq-filter (lambda (m)
                          (not (seq-find (lambda (aux)
                                           (equal (javaimp-module-id aux)
                                                  (javaimp-module-parent-id m)))
                                         modules)))
		        (cdr modules)))))
      (mapcar (lambda (root)
                (javaimp--build-tree root nil modules))
              roots))))

(defun javaimp--maven-effective-pom-handler ()
  (let ((start
	 (save-excursion
	   (progn
	     (goto-char (point-min))
	     (re-search-forward "<\\?xml\\|<projects?")
	     (match-beginning 0))))
	(end
	 (save-excursion
	   (progn
	     (goto-char (point-min))
	     (re-search-forward "<\\(projects?\\)")
	     ;; corresponding close tag is the end of parse region
	     (search-forward (concat "</" (match-string 1) ">"))
	     (match-end 0)))))
    (xml-parse-region start end)))

(defun javaimp--maven-module-from-xml (elt file-orig)
  (let ((build-elt (javaimp--xml-child 'build elt)))
    (make-javaimp-module
     :id (javaimp--maven-id-from-xml elt)
     :parent-id (javaimp--maven-id-from-xml (javaimp--xml-child 'parent elt))
     ;; <project> element does not contain pom file path, so we set this slot
     ;; later, see javaimp--maven-fill-modules-files
     :file nil
     :file-orig file-orig
     ;; jar/war supported
     :final-name (let ((packaging (or (javaimp--xml-first-child
		                       (javaimp--xml-child 'packaging elt))
                                      "jar")))
                   (when (member packaging '("jar" "war"))
                     (concat (javaimp--xml-first-child
                              (javaimp--xml-child 'finalName build-elt))
                             "." packaging)))
     :source-dirs (list (file-name-as-directory
		         (javaimp-cygpath-convert-maybe
		          (javaimp--xml-first-child
		           (javaimp--xml-child 'sourceDirectory build-elt))))
                        (file-name-as-directory
		         (javaimp-cygpath-convert-maybe
			  (javaimp--xml-first-child
			   (javaimp--xml-child 'testSourceDirectory build-elt)))))
     :build-dir (file-name-as-directory
		 (javaimp-cygpath-convert-maybe
		  (javaimp--xml-first-child (javaimp--xml-child 'directory build-elt))))
     :dep-jars nil          ; dep-jars is initialized lazily on demand
     :load-ts (current-time)
     :dep-jars-path-fetcher #'javaimp--maven-fetch-dep-jars-path
     :raw elt)))

(defun javaimp--maven-id-from-xml (elt)
  (make-javaimp-id
   :group (javaimp--xml-first-child (javaimp--xml-child 'groupId elt))
   :artifact (javaimp--xml-first-child (javaimp--xml-child 'artifactId elt))
   :version (javaimp--xml-first-child (javaimp--xml-child 'version elt))))

(defun javaimp--maven-fill-modules-files (file modules)
  ;; Reads module id from FILE, looks up corresponding module in
  ;; MODULES, sets its :file slot, then recurses for each submodule.
  ;; A submodule file path is constructed by appending relative path
  ;; taken from <module> to FILE's directory.
  (let* ((xml-tree (with-temp-buffer
		     (insert-file-contents file)
		     (xml-parse-region (point-min) (point-max))))
	 (project-elt (assq 'project xml-tree))
	 (this-id (javaimp--maven-id-from-xml project-elt))
         (module (seq-find
                  (lambda (m)
                    (let ((mid (javaimp-module-id m)))
                      (and (equal (javaimp-id-artifact this-id)
				  (javaimp-id-artifact mid))
	                   ;; Group and version may be inherited and
	                   ;; thus not presented in file
                           (or (null (javaimp-id-group this-id))
                               (equal (javaimp-id-group this-id)
				      (javaimp-id-group mid)))
                           (or (null (javaimp-id-version this-id))
                               (equal (javaimp-id-version this-id)
				      (javaimp-id-version mid))))))
                  modules)))
    (unless module
      (error "Cannot find module corresponding to file \"%s\" (id %s)"
             file (javaimp-print-id this-id)))
    (and (null (javaimp-id-group this-id))
         (null (javaimp-id-version this-id))
         (message "File \"%s\" contains incomplete id, used lax matching" file))

    (setf (javaimp-module-file module) file)

    ;; Take <module> subelements from raw project, not from file,
    ;; because Maven can extend the list with elements from profiles.
    (let ((rel-paths
           (mapcar #'javaimp--xml-first-child
		   (javaimp--xml-children
                    (javaimp--xml-child 'modules (javaimp-module-raw module))
                    'module))))
      ;; recurse into each child <module>
      (dolist (rel-path rel-paths)
	(javaimp--maven-fill-modules-files
         (concat (file-name-directory file)
                 (mapconcat #'file-name-as-directory
                            (split-string rel-path "[/\\\\]+" t) nil)
		 "pom.xml")
	 modules)))))

(defun javaimp--maven-fetch-dep-jars-path (module)
  (javaimp--call-build-tool
   javaimp-mvn-program
   (lambda ()
     (goto-char (point-min))
     (search-forward "Dependencies classpath:")
     (forward-line 1)
     (thing-at-point 'line))
   ;; always invoke for this module's pom.ml
   "-f" (javaimp-cygpath-convert-maybe
         (javaimp-module-file module))
   "dependency:build-classpath"))

(provide 'javaimp-maven)
