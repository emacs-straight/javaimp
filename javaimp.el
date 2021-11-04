;;; javaimp.el --- Add and reorder Java import statements in Maven/Gradle projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2021  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>
;; Version: 0.7.1
;; Keywords: java, maven, gradle, programming

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

;;; Commentary:

;; Allows to manage Java import statements in Maven/Gradle projects.
;;
;;   Quick start:
;;
;; - customize `javaimp-import-group-alist'
;; - call `javaimp-visit-project', giving it the top-level project
;; directory where pom.xml / build.gradle[.kts] resides
;;
;; Then in a Java buffer visiting a file under that project or one of its
;; submodules call `javaimp-organize-imports' or `javaimp-add-import'.
;;
;; This module does not add all needed imports automatically!  It only helps
;; you to quickly add imports when stepping through compilation errors.
;;
;;   Some details:
;;
;; Contents of jar files and Maven/Gradle project structures are
;; cached, so usually only the first command should take a
;; considerable amount of time to complete.  If a module's build file
;; or any of its parents' build files (within visited tree) was
;; modified after information was loaded, dependencies are fetched
;; from the build tool again.  If a jar file was changed, its contents
;; are re-read.
;;
;;
;;   Example:
;;
;; (require 'javaimp)
;; (add-to-list 'javaimp-import-group-alist
;;   '("\\`\\(my\\.company\\.\\|my\\.company2\\.\\)" . 80))
;; (setq javaimp-additional-source-dirs '("generated-sources/thrift"))
;; (add-hook 'java-mode-hook
;; 	  (lambda ()
;; 	    (local-set-key "\C-ci" #'javaimp-add-import)
;; 	    (local-set-key "\C-co" #'javaimp-organize-imports)))
;; (global-set-key (kbd "C-c j v") #'javaimp-visit-project)
;;

;;; News:

;; v0.7:
;; - Added Gradle support.
;;
;; - Removed javaimp-maven-visit-project in favor of javaimp-visit-project.
;;
;; - Split into multiple files.


;;; Code:

(require 'javaimp-maven)
(require 'javaimp-gradle)
(require 'javaimp-parse)
(require 'cc-mode)                      ;for java-mode-syntax-table
(require 'imenu)


;; User options

(defgroup javaimp ()
  "Add and reorder Java import statements in Maven/Gradle
projects"
  :group 'c)

(defcustom javaimp-import-group-alist
  '(("\\`java\\." . 10)
    ("\\`javax\\." . 15))
  "Specifies how to group classes and how to order resulting
groups in the imports list.

Each element should be of the form (CLASSNAME-REGEXP . ORDER)
where CLASSNAME-REGEXP is a regexp matching the fully qualified
class name.  Lowest-order groups are placed earlier.

The order of classes which were not matched is defined by
`javaimp-import-default-order'."
  :type '(alist :key-type string :value-type integer))

(defcustom javaimp-import-default-order 50
  "Defines the order of classes which were not matched by
`javaimp-import-group-alist'"
  :type 'integer)

(defcustom javaimp-java-home (getenv "JAVA_HOME")
  "Path to the JDK.  Should contain subdirectory
\"jre/lib\" (pre-JDK9) or just \"lib\".  By default, it is
initialized from the JAVA_HOME environment variable."
  :type 'string)

(defcustom javaimp-additional-source-dirs nil
  "List of directories where additional (e.g. generated)
source files reside.

Each directory is a relative path from ${project.build.directory} project
property value.

Typically you would check documentation for a Maven plugin, look
at the parameter's default value there and add it to this list.

E.g. \"${project.build.directory}/generated-sources/<plugin_name>\"
becomes \"generated-sources/<plugin_name>\" (note the absence
of the leading slash.

Custom values set in plugin configuration in pom.xml are not
supported yet."
  :type '(repeat (string :tag "Relative directory")))

(defcustom javaimp-jar-program "jar"
  "Path to the `jar' program used to read contents of jar files.
Customize it if the program is not on `exec-path'."
  :type 'string)

(defcustom javaimp-jmod-program "jmod"
  "Path to the `jmod' program used to read contents of jmod files.
Customize it if the program is not on `exec-path'."
  :type 'string)

(defcustom javaimp-enable-parsing t
  "If non-nil, javaimp will try to parse current module's source
files to determine completion alternatives, in addition to those
from module dependencies."
  :type 'boolean)

(defcustom javaimp-imenu-use-sub-alists nil
  "If non-nil, make sub-alist for each containing scope (e.g. a
class)."
  :type 'boolean)

(defcustom javaimp-cygpath-program
  (if (eq system-type 'cygwin) "cygpath")
  "Path to the `cygpath' program (Cygwin only).  Customize it if
the program is not on `exec-path'."
  :type 'string)

(defcustom javaimp-mvn-program "mvn"
  "Path to the `mvn' program.  Customize it if the program is not
on `exec-path'."
  :type 'string)

(defcustom javaimp-gradle-program "gradle"
  "Path to the `gradle' program.  Customize it if the program is
not on `exec-path'.  If the visited project's directory contains
gradlew program, it is used in preference."
  :type 'string)


;; Variables

(defvar javaimp-project-forest nil
  "Visited projects")

(defvar javaimp-cached-jars nil
  "Alist of cached jars.  Each element is of the form (FILE
  . CACHED-JAR).")

(defvar javaimp--jdk-classes 'need-init)

(defvar javaimp-syntax-table
  (make-syntax-table java-mode-syntax-table) ;TODO don't depend
  "Javaimp syntax table")

(defvar javaimp--arglist-syntax-table
  (let ((st (make-syntax-table javaimp-syntax-table)))
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")<" st)
    (modify-syntax-entry ?. "_" st) ; separates parts of fully-qualified type
    st)
  "Enables parsing angle brackets as lists")

(defconst javaimp--jar-error-header
  "There were errors when reading some of the dependency files,
they are listed below.

Note that if you're using java-library plugin in Gradle for any
modules inside the project tree, then Gradle may avoid creating
jars for them.  You need to put this into
$HOME/.gradle/gradle.properties to force that:

systemProp.org.gradle.java.compile-classpath-packaging=true

For more info, see
https://docs.gradle.org/current/userguide/java_library_plugin.html\
#sec:java_library_classes_usage
")


;;;###autoload
(defun javaimp-visit-project (dir)
  "Loads a project and its submodules.  DIR should point to a
directory containing pom.xml / build.gradle[.kts].

After being processed by this command, the module tree becomes
known to javaimp and `javaimp-add-import' may be called inside
any module file."
  (interactive "DVisit Gradle or Maven project in directory: ")
  (setq dir (file-name-as-directory (expand-file-name dir)))
  (if-let ((build-file
            (seq-find #'file-exists-p
                      (mapcar (lambda (f)
                                (concat dir f))
                              '("build.gradle" "build.gradle.kts"
                                "pom.xml")))))
      (progn
        ;; Forget previous tree(s) loaded from this build file, if
        ;; any.  Additional project trees (see below) have the same
        ;; file-orig, so there may be several here.
        (when-let ((existing-list
                    (seq-filter (lambda (node)
                                  (equal (javaimp-module-file-orig
                                          (javaimp-node-contents node))
	                                 build-file))
                                javaimp-project-forest)))
          (if (y-or-n-p "Forget already loaded project(s)?")
              (setq javaimp-project-forest
                    (seq-remove (lambda (node)
                                  (memq node existing-list))
                                javaimp-project-forest))
            (user-error "Aborted")))
        (let ((trees (funcall (if (string-match
                                   "gradle" (file-name-nondirectory build-file))
                                  #'javaimp--gradle-visit
                                #'javaimp--maven-visit)
                              build-file)))
          (push (car trees) javaimp-project-forest)
          (dolist (node (cdr trees))
            (when (y-or-n-p
                   (format "Include additional project tree rooted at %S?"
                           (javaimp-module-id (javaimp-node-contents node))))
              (push node javaimp-project-forest)))
          (message "Loaded tree for %s" dir)))
    (error "Could not find build file in directory %s" dir)))


;; Dependency jars

(defun javaimp--update-module-maybe (node)
  (let ((module (javaimp-node-contents node))
	need-update ids)
    ;; check if deps are initialized
    (unless (javaimp-module-dep-jars module)
      (message "Loading dependencies for module: %s" (javaimp-module-id module))
      (setq need-update t))
    ;; check if this or any parent build file has changed since we
    ;; loaded the module
    (let ((tmp node))
      (while tmp
	(let ((cur (javaimp-node-contents tmp)))
	  (when (and (not need-update)
                     (> (max (if (file-exists-p (javaimp-module-file cur))
                                 (float-time
                                  (javaimp--get-file-ts (javaimp-module-file cur)))
                               -1)
                             (if (file-exists-p (javaimp-module-file-orig cur))
                                 (float-time
                                  (javaimp--get-file-ts (javaimp-module-file-orig cur)))
                               -1))
		        (float-time (javaimp-module-load-ts module))))
	    (message "Will reload dependencies for %s because build file changed"
                     (javaimp-module-id cur))
	    (setq need-update t))
          (push (javaimp-module-id cur) ids))
	(setq tmp (javaimp-node-parent tmp))))
    (when need-update
      (setf (javaimp-module-dep-jars module)
            (funcall (javaimp-module-dep-jars-fetcher module) module ids))
      (setf (javaimp-module-load-ts module)
            (current-time)))))

(defun javaimp--get-jar-classes (file)
  (condition-case err
      (let ((jar (alist-get file javaimp-cached-jars nil nil #'equal)))
        (when (or (not jar)
                  ;; If the file doesn't exist this will be current
                  ;; time, and thus condition always true
                  (> (float-time (javaimp--get-file-ts file))
	             (float-time (javaimp-cached-jar-read-ts jar))))
          (setq jar (make-javaimp-cached-jar
		     :file file
		     :read-ts (javaimp--get-file-ts file)
		     :classes (javaimp--read-jar-classes file))))
        (setf (alist-get file javaimp-cached-jars) jar)
        (javaimp-cached-jar-classes jar))
    (t
     (setf (alist-get file javaimp-cached-jars nil 'remove #'equal) nil)
     (signal (car err) (cdr err)))))

(defun javaimp--read-jar-classes (file)
  "Read FILE which should be a .jar or a .jmod and return classes
contained in it as a list."
  (let ((ext (downcase (file-name-extension file))))
    (unless (member ext '("jar" "jmod"))
      (error "Unexpected file name: %s" file))
    (let ((javaimp-tool-output-buf-name nil)) ;don't log
      (javaimp--call-build-tool
       (symbol-value (intern (format "javaimp-%s-program" ext)))
       #'javaimp--read-jar-classes-handler
       (if (equal ext "jar") "tf" "list")
       ;; On cygwin, "jar/jmod" is a windows program, so file path
       ;; needs to be converted appropriately.
       (javaimp-cygpath-convert-maybe file 'windows)))))

(defun javaimp--read-jar-classes-handler ()
  "Used by `javaimp--read-jar-classes' to handle jar program
output."
  (let (result curr)
    (while (re-search-forward
            (rx (and bol
                     (? "classes/")     ; prefix output by jmod
                     (group (+ (any alnum "_/$")))
                     ".class"
                     eol))
            nil t)
      (setq curr (match-string 1))
      (unless (or (string-suffix-p "module-info" curr)
                  (string-suffix-p "package-info" curr)
                  ;; like Provider$1.class
                  (string-match-p "\\$[[:digit:]]" curr))
        (push
         (string-replace "/" "."
                         (string-replace "$" "." curr))
         result)))
    result))


;; Some API functions
;;
;; do not expose tree structure, return only modules

(defun javaimp-find-module (predicate)
  "Returns first module in `javaimp-project-forest' for which
PREDICATE returns non-nil."
  (javaimp--find-node predicate javaimp-project-forest t))

(defun javaimp-collect-modules (predicate)
  "Returns all modules in `javaimp-project-forest' for which
PREDICATE returns non-nil."
  (javaimp--collect-nodes predicate javaimp-project-forest))

(defun javaimp-map-modules (function)
  (javaimp--map-nodes function #'always javaimp-project-forest))


;;; Adding imports

;;;###autoload
(defun javaimp-add-import (classname)
  "Imports classname in the current file by asking for input (with
completion) and calling `javaimp-organize-imports'.

Completion alternatives are constructed as follows:

- If `javaimp-java-home' is set then add JDK classes.  lib-dir is
\"jre/lib\" or \"lib\" subdirectory.  First, attempt to read jmod
files in \"lib-dir/jmods\" subdirectory.  If there's no jmods
subdirectory - fallback to reading all jar files in lib-dir, this
is for pre-jdk9 versions of java.

- If current module can be determined, then add all classes from
its dependencies.

- If `javaimp-enable-parsing' is non-nil, also add current
module's classes.  If there's no current module, then add all
classes from the current file tree: if there's a \"package\"
directive in the current file and it matches last components of
the file name, then file tree starts in the parent directory of
the package, otherwise just use current directory.

- If no prefix arg is given, keep only candidates whose class
simple name (last component of a fully-qualified name) matches
current `symbol-at-point'."
  (interactive
   (let* ((file (expand-file-name (or buffer-file-name
				      (error "Buffer is not visiting a file!"))))
	  (node (javaimp--find-node
		 (lambda (m)
                   (seq-some (lambda (dir)
                               (string-prefix-p dir file))
                             (javaimp-module-source-dirs m)))
                 javaimp-project-forest))
          (module (when node
                    (javaimp--update-module-maybe node)
                    (javaimp-node-contents node)))
          (classes
           (append
            ;; jdk
            (progn
              (when (eq javaimp--jdk-classes 'need-init)
                (setq javaimp--jdk-classes (javaimp--get-jdk-classes)))
              javaimp--jdk-classes)
            ;; module dependencies
            (when module
	      ;; We're not caching full list of classes coming from
	      ;; module dependencies because jars may change
              (let (jar-errors)
                (prog1
                    (seq-mapcat (lambda (jar)
                                  (condition-case err
                                      (javaimp--get-jar-classes jar)
                                    (t
                                     (push (error-message-string err) jar-errors)
                                     nil)))
                                (javaimp-module-dep-jars module))
                  (when jar-errors
                    (with-output-to-temp-buffer "*Javaimp Jar errors*"
                      (princ javaimp--jar-error-header)
                      (terpri)
                      (dolist (err (nreverse jar-errors))
                        (princ err)
                        (terpri)))))))
            ;; current module or source tree
            (when (and module javaimp-enable-parsing)
              (seq-mapcat #'javaimp--get-directory-classes
                          (javaimp--get-current-source-dirs module)))
            ))
          (completion-regexp-list
           (and (not current-prefix-arg)
                (symbol-at-point)
                (list (rx (and symbol-start
                               (literal (symbol-name (symbol-at-point)))
                               eol))))))
     (list (completing-read "Import: " classes nil t nil nil
                            (symbol-name (symbol-at-point))))))
  (javaimp-organize-imports (cons classname 'ordinary)))

(defun javaimp--get-jdk-classes ()
  (when javaimp-java-home
    (unless (file-accessible-directory-p javaimp-java-home)
      (user-error "Java home directory \"%s\" is not accessible" javaimp-java-home))
    (let ((dir (concat (file-name-as-directory javaimp-java-home) "jmods")))
      (if (file-directory-p dir)
          ;; java9 and later contain modules, scan them
          (apply #'append
	         (mapcar #'javaimp--get-jar-classes
                         (directory-files dir t "\\.jmod\\'")))
        ;; pre-jdk9
        (setq dir (mapconcat #'file-name-as-directory
                             `(,javaimp-java-home "jre" "lib") nil))
        (message "jmods directory not found, reading jars from \"%s\"" dir)
        (if (file-directory-p dir)
            (apply #'append
	           (mapcar #'javaimp--get-jar-classes
                           (directory-files dir t "\\.jar\\'")))
          (user-error "jre/lib dir \"%s\" doesn't exist" dir))))))

(defun javaimp--get-current-source-dirs (module)
  "Return list of source directories to check for Java files."
  (if module
      (append
       (javaimp-module-source-dirs module)
       ;; additional source dirs
       (mapcar (lambda (dir)
                 (concat (javaimp-module-build-dir module)
                         (file-name-as-directory dir)))
               javaimp-additional-source-dirs))
    (list
     (if-let ((package (save-excursion
                         (save-restriction
                           (widen)
                           (javaimp--parse-get-package)))))
         (string-remove-suffix
          (mapconcat #'file-name-as-directory (split-string package "\\." t) nil)
          default-directory)
       default-directory))))

(defun javaimp--get-directory-classes (dir)
  (when (file-accessible-directory-p dir)
    (seq-mapcat #'javaimp--get-file-classes
                (seq-filter (lambda (file)
                              (not (file-symlink-p file)))
                            (directory-files-recursively dir "\\.java\\'")))))

(defun javaimp--get-file-classes (file)
  (if-let ((buf (get-file-buffer file)))
      (with-current-buffer buf
        (save-excursion
          (save-restriction
            (widen)
            (javaimp--get-classes))))
    (with-temp-buffer
      (insert-file-contents file)
      (javaimp--get-classes))))

(defun javaimp--get-classes ()
  "Return fully-qualified names of all class-like scopes."
  (let ((package (javaimp--parse-get-package))
        (scopes (javaimp--parse-get-all-scopes
                 (lambda (scope)
                   (javaimp-test-scope-type scope
                     javaimp--classlike-scope-types
                     javaimp--classlike-scope-types)))))
    (mapcar (lambda (class)
              (if package
                  (concat package "." class)
                class))
            (mapcar (lambda (scope)
                      (let ((name (javaimp-scope-name scope))
                            (parent-names (javaimp--concat-scope-parents scope)))
                        (if (string-empty-p parent-names)
                            name
                          (concat parent-names "." name))))
                    scopes))))



;; Organizing imports

;;;###autoload
(defun javaimp-organize-imports (&rest new-imports)
  "Groups import statements according to the value of
`javaimp-import-group-alist' (which see) and prints resulting
groups leaving one blank line between groups.

If the file already contains some import statements, this command
rewrites them, starting with the same place.  Else, if the the
file contains package directive, this command inserts one blank
line below and then imports.  Otherwise, imports are inserted at
the beginning of buffer.

Classes within a single group are ordered in a lexicographic
order.  Imports not matched by any regexp in `javaimp-import-group-alist'
are assigned a default order defined by
`javaimp-import-default-order'.  Duplicate imports are squashed.

NEW-IMPORTS is a list of additional imports; each element should
be of the form (CLASS . TYPE), where CLASS is a string and TYPE
is `ordinary' or `static'.  Interactively, NEW-IMPORTS is nil."
  (interactive)
  (barf-if-buffer-read-only)
  (save-excursion
    (goto-char (point-min))
    (let* ((old-data (javaimp--parse-imports))
	   (first (car old-data))
	   (last (cadr old-data))
	   (all-imports (append new-imports (cddr old-data))))
      (if all-imports
	  (progn
	    ;; delete old imports, if any
	    (if first
		(progn
		  (goto-char last)
		  (forward-line)
		  (delete-region first (point))))
	    (javaimp--prepare-for-insertion first)
	    (setq all-imports
		  (cl-delete-duplicates
                   all-imports
                   :test (lambda (first second)
                           (equal (car first) (car second)))))
	    ;; assign order
	    (let ((with-order
		   (mapcar
		    (lambda (import)
		      (let ((order (or (assoc-default (car import)
						      javaimp-import-group-alist
						      'string-match)
				       javaimp-import-default-order)))
			(cons import order)))
		    all-imports)))
	      (setq with-order
		    (sort with-order
			  (lambda (first second)
			    ;; sort by order, name
			    (if (= (cdr first) (cdr second))
				(string< (caar first) (caar second))
			      (< (cdr first) (cdr second))))))
	      (javaimp--insert-imports with-order)))
        (message "Nothing to organize!")))))

(defun javaimp--parse-imports ()
  "Returns (FIRST LAST . IMPORTS)"
  (let (first last imports)
    (while (re-search-forward "^\\s *import\\s +\\(static\\s +\\)?\\([._[:word:]]+\\)" nil t)
      (let ((type (if (match-string 1) 'static 'ordinary))
	    (class (match-string 2)))
	(push (cons class type) imports))
      (setq last (line-beginning-position))
      (or first (setq first last)))
    (cons first (cons last imports))))

(defun javaimp--prepare-for-insertion (start)
  (cond (start
	 ;; if there were any imports, we start inserting at the same place
	 (goto-char start))
	((re-search-forward "^\\s *package\\s " nil t)
	 ;; if there's a package directive, insert one blank line
	 ;; below it
	 (end-of-line)
	 (if (eobp)
	     (insert ?\n)
	   (forward-line))
	 (insert ?\n))
	(t
	 ;; otherwise, just go to bob
	 (goto-char (point-min)))))

(defun javaimp--insert-imports (imports)
  (let ((static (seq-filter (lambda (elt)
			      (eq (cdar elt) 'static))
			    imports))
	(ordinary (seq-filter (lambda (elt)
				(eq (cdar elt) 'ordinary))
			      imports)))
    (javaimp--insert-import-group "import static %s;" static)
    (and static ordinary (insert ?\n))
    (javaimp--insert-import-group "import %s;" ordinary)))

(defun javaimp--insert-import-group (pattern imports)
  (let (last-order)
    (dolist (import imports)
      ;; if adjacent imports have different order value, insert a newline
      ;; between them
      (let ((order (cdr import)))
	(and last-order
	     (/= order last-order)
	     (insert ?\n))
	(insert (format pattern (caar import)) ?\n)
	(setq last-order order)))))



;; Imenu support

;;;###autoload
(defun javaimp-imenu-create-index ()
  "Function to use as `imenu-create-index-function', can be set
in a major mode hook."
  (let ((forest (javaimp-imenu--get-forest)))
    (if javaimp-imenu-use-sub-alists
        (javaimp--map-nodes
         (lambda (scope)
           (if (eq (javaimp-scope-type scope) 'method)
               ;; entry
               (cons nil (javaimp-imenu--make-entry scope))
             ;; sub-alist
             (cons t (javaimp-scope-name scope))))
         (lambda (res)
           (or (functionp (nth 2 res)) ;entry
               (cdr res)))             ;non-empty sub-alist
         forest)
      (let ((entries
             (mapcar #'javaimp-imenu--make-entry
                     (seq-sort-by #'javaimp-scope-start #'<
                                  (javaimp--collect-nodes
                                   (lambda (scope)
                                     (eq (javaimp-scope-type scope) 'method))
                                   forest))))
            alist)
        (mapc (lambda (entry)
                (setf (alist-get (car entry) alist 0 nil #'equal)
                      (1+ (alist-get (car entry) alist 0 nil #'equal))))
              entries)
        (mapc (lambda (entry)
                ;; disambiguate same method names
                (when (> (alist-get (car entry) alist 0 nil #'equal) 1)
                  (setcar entry
                          (format "%s [%s]"
                                  (car entry)
                                  (javaimp--concat-scope-parents
                                   (nth 3 entry))))))
              entries)))))

(defun javaimp-imenu--get-forest ()
  (let* ((scopes (javaimp--parse-get-all-scopes
                  (lambda (scope)
                    (javaimp-test-scope-type scope
                      '(class interface enum method)
                      javaimp--classlike-scope-types))))
         (methods (seq-filter
                   (lambda (scope)
                     (eq (javaimp-scope-type scope) 'method))
                   scopes))
         (classes (seq-filter
                   (lambda (scope)
                     (not (eq (javaimp-scope-type scope) 'method)))
                   scopes))
         (top-classes (seq-filter (lambda (s)
                                    (null (javaimp-scope-parent s)))
                                  classes))
         (abstract-methods (append
                            (javaimp--parse-get-class-abstract-methods)
                            (javaimp--parse-get-interface-abstract-methods))))
    (mapcar
     (lambda (top-class)
       (message "Building tree for top-level class-like scope: %s"
                (javaimp-scope-name top-class))
       (javaimp--build-tree top-class
                            (append methods
                                    classes
                                    abstract-methods)
                            (lambda (el tested)
                              (equal el (javaimp-scope-parent tested)))
                            nil
                            (lambda (s1 s2)
                              (< (javaimp-scope-start s1)
                                 (javaimp-scope-start s2)))))
     top-classes)))

(defsubst javaimp-imenu--make-entry (scope)
  (list (javaimp-scope-name scope)
        (if imenu-use-markers
            (copy-marker (javaimp-scope-start scope))
          (javaimp-scope-start scope))
        #'javaimp-imenu--function
        scope))

(defun javaimp-imenu--function (_index-name index-position _scope)
  (goto-char index-position)
  (back-to-indentation))


;; Help

(defvar javaimp-help-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-m" #'javaimp-help-goto-scope)
    (define-key map [mouse-2] #'javaimp-help-goto-scope-mouse)
    map)
  "Keymap for Javaimp help buffers.")

(defun javaimp-help-goto-scope (pos)
  "Go to scope at point in another window."
  (interactive "d")
  (javaimp-help--goto-scope-1 pos))

;; TODO handle mouse-1
(defun javaimp-help-goto-scope-mouse (event)
  "Go to scope you click on in another window."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (with-current-buffer (window-buffer window)
      (javaimp-help--goto-scope-1 pos))))

(defun javaimp-help--goto-scope-1 (pos)
  "Go to the opening brace (`javaimp-scope-open-brace') of the
scope at POS.  With prefix arg, go to scope
start (`javaimp-scope-start') instead."
  (let ((scope (get-text-property pos 'javaimp-help-scope))
        (file (get-text-property (point-min) 'javaimp-help-file)))
    (when (and scope file)
      (with-current-buffer (find-file-other-window file)
        (goto-char (if current-prefix-arg
                       (javaimp-scope-start scope)
                     (javaimp-scope-open-brace scope)))))))

(defun javaimp-help-show-scopes ()
  "Show scopes in a *javaimp-scopes* buffer."
  (interactive)
  (let ((scopes (save-excursion
                  (save-restriction
                    (widen)
                    (javaimp--parse-get-all-scopes))))
        (file buffer-file-name)
        (buf (get-buffer-create "*javaimp-scopes*")))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (propertize (format "%s\n\n" file)
                          'javaimp-help-file file))
      (dolist (scope scopes)
        (let ((depth 0)
              (tmp scope))
          (while (setq tmp (javaimp-scope-parent tmp))
            (setq depth (1+ depth)))
          (insert (propertize
                   (format "%d %s: %s%s\n"
                           depth
                           (cdr (assq (javaimp-scope-type scope)
                                      javaimp--help-scope-type-abbrevs))
                           (make-string depth ? )
                           (javaimp-scope-name scope))
                   'mouse-face 'highlight
                   'help-echo "mouse-2: go to this scope"
                   'javaimp-help-scope scope
                   'keymap javaimp-help-keymap))))
      (setq buffer-read-only t))
    (display-buffer buf)))


;; Misc

(defun javaimp-reset (arg)
  "Forget loaded trees state.  With prefix arg, also reset jars
cache."
  (interactive "P")
  (setq javaimp-project-forest nil
        javaimp--jdk-classes 'need-init)
  (when arg
    (setq javaimp-cached-jars nil)))

(provide 'javaimp)

;;; javaimp.el ends here
