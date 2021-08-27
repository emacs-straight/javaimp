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
;; If Maven/Gradle failed, you can see its output in the buffer named
;; by `javaimp-debug-buf-name' (default is "*javaimp-debug*").
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

(defcustom javaimp-include-current-module-classes t
  "If non-nil, current module's classes are included into
completion alternatives.  `javaimp-add-import' will find all java
files in the current project and add their fully-qualified names
to the completion alternatives list."
  :type 'boolean)

(defcustom javaimp-imenu-group-methods t
  "How to lay out methods in Imenu index.
If t (the default), methods are grouped in their enclosing
scopes.  nil means use just flat list of simple method names.
`qualified' means use flat list where each method name is
prepended with nested scopes.  See also
`javaimp-format-method-name'."
  :type '(choice :tag "Group methods"
		 (const :tag "Group into enclosing scopes" t)
		 (const :tag "Flat list of simple name" nil)
		 (const :tag "Flat list of qualified names" qualified)))

(defcustom javaimp-cygpath-program
  (if (eq system-type 'cygwin) "cygpath")
  "Path to the `cygpath' program (Cygwin only).  Customize it if
the program is not on `exec-path'."
  :type 'string)

(defcustom javaimp-format-method-name #'javaimp-format-method-name-full
  "Function to format method name, invoked with 3 arguments:
NAME, ARGS and THROWS-ARGS.  The last two are lists with elements
of the form (TYPE . NAME).  For THROWS-ARGS, only TYPE is
present."
  :type 'function)

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


;;;###autoload
(defun javaimp-visit-project (dir)
  "Loads a project and its submodules.  DIR should point to a
directory containing pom.xml / build.gradle[.kts].

After being processed by this command, the module tree becomes
known to javaimp and `javaimp-add-import' may be called inside
any module file."
  (interactive "DVisit Maven or Gradle project in directory: ")
  (let* ((exp-dir (expand-file-name (file-name-as-directory dir)))
         build-file
         (trees (cond
                 ((file-regular-p (setq build-file (concat exp-dir "pom.xml")))
                  (javaimp--maven-visit build-file))
                 ((or (file-regular-p
                       (setq build-file (concat exp-dir "build.gradle")))
                      (file-regular-p
                       (setq build-file (concat exp-dir "build.gradle.kts"))))
                  (list (javaimp--gradle-visit build-file)))
                 (t
                  (error "Could not find build file in dir %s" dir)))))
    ;; delete previous tree(s) loaded from this build file, if any
    (setq javaimp-project-forest
	  (seq-remove (lambda (node)
			(equal (javaimp-module-file-orig (javaimp-node-contents node))
			       build-file))
		      javaimp-project-forest))
    (push (car trees) javaimp-project-forest)
    (dolist (node (cdr trees))
      (when (y-or-n-p (format "Include additional project tree rooted at %S? "
                              (javaimp-module-id (javaimp-node-contents node))))
        (push node javaimp-project-forest)))
    (message "Loaded tree for %s" dir)))


;; Dependency jars

(defun javaimp--update-module-maybe (node)
  (let ((module (javaimp-node-contents node))
	need-update)
    ;; check if deps are initialized
    (unless (javaimp-module-dep-jars module)
      (message "Loading dependencies: %s" (javaimp-module-id module))
      (setq need-update t))
    ;; check if this or any parent build file has changed since we
    ;; loaded the module
    (let ((tmp node))
      (while (and tmp (not need-update))
	(let ((cur (javaimp-node-contents tmp)))
	  (when (> (max (if (file-exists-p (javaimp-module-file cur))
                            (float-time (javaimp--get-file-ts (javaimp-module-file cur)))
                          -1)
                        (if (file-exists-p (javaimp-module-file-orig cur))
                            (float-time (javaimp--get-file-ts (javaimp-module-file-orig cur)))
                          -1))
		   (float-time (javaimp-module-load-ts module)))
	    (message "Will reload dependencies for %s (some build-file changed)"
                     (javaimp-module-id cur))
	    (setq need-update t)))
	(setq tmp (javaimp-node-parent tmp))))
    (when need-update
      (let* ((path (funcall (javaimp-module-dep-jars-path-fetcher module) module))
             (new-dep-jars (javaimp--split-native-path path))
	     (new-load-ts (current-time)))
	(setf (javaimp-module-dep-jars module) new-dep-jars)
	(setf (javaimp-module-load-ts module) new-load-ts)))))

(defun javaimp--get-jar-classes (file)
  (let ((cached (cdr (assoc file javaimp-cached-jars))))
    (cond ((null cached)
	   ;; create, load & put into cache
	   (setq cached
		 (make-javaimp-cached-jar
		  :file file
		  :read-ts (javaimp--get-file-ts file)
		  :classes (javaimp--fetch-jar-classes file)))
	   (push (cons file cached) javaimp-cached-jars))
	  ((> (float-time (javaimp--get-file-ts (javaimp-cached-jar-file cached)))
	      (float-time (javaimp-cached-jar-read-ts cached)))
	   ;; reload
	   (setf (javaimp-cached-jar-classes cached) (javaimp--fetch-jar-classes file))
	   ;; update read-ts
	   (setf (javaimp-cached-jar-read-ts cached) (current-time))))
    ;; return from cached
    (javaimp-cached-jar-classes cached)))

(defun javaimp--fetch-jar-classes (file)
  (let ((ext (downcase (file-name-extension file))))
    (unless (member ext '("jar" "jmod"))
      (error "Unexpected file name: %s" file))
    (message "Reading classes in file: %s" file)
    (with-temp-buffer
      (let ((coding-system-for-read (when (eq system-type 'cygwin)
                                      'utf-8-dos)))
        (process-file
         (symbol-value (intern (format "javaimp-%s-program" ext)))
         nil
         t
         nil
         (if (equal ext "jar") "tf" "list")
         ;; On cygwin, "jar/jmod" is a windows program, so file path
         ;; needs to be converted appropriately.
         (javaimp-cygpath-convert-maybe file 'windows)))
      (goto-char (point-min))
      (let (result curr)
	(while (re-search-forward
                (rx (and bol
                         (? "classes/") ; prefix output by jmod
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
        result))))


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
files in \"lib-dir/jmods\" subdirectory.  If there's jmods
subdirectory - fallback to reading all jar files in lib-dir.

- If current module can be determined, then add all classes from
sits dependencies.

- If `javaimp-include-current-module-classes' is set, then add
current module's classes.  If there's no current module, then add
all classes from the current file tree: if there's a \"package\"
directive in the current file and it matches last components of
the file name, then file tree starts in the parent directory of
the package, otherwise just use current directory.

- Keep only candidates whose class simple name (last component of
a fully-qualified name) matches current `symbol-at-point'.  If a
prefix arg is given, don't do this filtering."
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
	      ;; module dependencies because jars may change and we
	      ;; need to reload them
	      (apply #'append
		     (mapcar #'javaimp--get-jar-classes
                             (javaimp-module-dep-jars module))))
            ;; current module or source tree
            (when javaimp-include-current-module-classes
              (if module
                  (javaimp--get-module-classes module)
                (javaimp--get-directory-classes
                 (or (javaimp--dir-above-current-package) default-directory))))
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
        (message "jmods directory not found, fallback to reading jars from \"%s\"" dir)
        (if (file-directory-p dir)
            (apply #'append
	           (mapcar #'javaimp--get-jar-classes
                           (directory-files dir t "\\.jar\\'")))
          (user-error "JRE lib dir \"%s\" doesn't exist" dir))))))

(defun javaimp--get-module-classes (module)
  "Returns list of classes in current module"
  (append
   ;; source dirs
   (seq-mapcat #'javaimp--get-directory-classes
               (javaimp-module-source-dirs module))
   ;; additional source dirs
   (seq-mapcat (lambda (rel-dir)
                 (javaimp--get-directory-classes
                  (concat (javaimp-module-build-dir module)
                          (file-name-as-directory rel-dir))))
               javaimp-additional-source-dirs)))

(defun javaimp--dir-above-current-package ()
  (let ((package (javaimp--parse-get-package)))
    (when package
      (string-remove-suffix
       (mapconcat #'file-name-as-directory
                  (split-string package "\\." t) nil)
       default-directory))))

(defun javaimp--get-directory-classes (dir)
  (if (file-accessible-directory-p dir)
      (seq-mapcat #'javaimp--get-file-classes
                  (seq-filter (lambda (file)
                                (not (file-symlink-p file)))
                              (directory-files-recursively dir "\\.java\\'")))))

(defun javaimp--get-file-classes (file)
  (let ((buf (seq-find (lambda (b) (equal (buffer-file-name b) file))
                       (buffer-list))))
    (if buf
        (with-current-buffer buf
          (javaimp--get-file-classes-1))
      (with-temp-buffer
        (insert-file-contents file)
        (setq javaimp--parse-dirty-pos (point-min))
        (javaimp--get-file-classes-1)))))

(defun javaimp--get-file-classes-1 ()
  (let ((package (javaimp--parse-get-package)))
    (mapcar (lambda (class)
              (if package
                  (concat package "." class)
                class))
            (javaimp--parse-get-all-classlikes))))


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
	 ;; if there's a package directive, insert one blank line below and
	 ;; leave point after it
	 (end-of-line)
	 (if (eobp)
	     (insert ?\n)
	   (forward-line))
	 ;; then insert one blank line and we're done
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
  "Function to use as `imenu-create-index-function'.

Currently it requires some manual setup, something like this in
the `java-mode-hook':

  (setq imenu-create-index-function #'javaimp-imenu-create-index)
  (setq syntax-ppss-table javaimp-syntax-table)
  (setq javaimp--parse-dirty-pos (point-min))
  (add-hook 'after-change-functions #'javaimp--parse-update-dirty-pos)

In future, when we implement a minor / major mode, it will be
done in mode functions automatically."
  (let ((forest (javaimp--parse-get-imenu-forest)))
    (cond ((not javaimp-imenu-group-methods)
           ;; plain list of methods
           (let ((entries
                  (mapcar #'javaimp-imenu--make-entry
                          (seq-sort-by
                           #'javaimp-scope-start #'<
                           (javaimp--collect-nodes
                            #'javaimp--is-imenu-included-method forest))))
                 name-alist)
             (mapc (lambda (entry)
                     (setf (alist-get (car entry) name-alist 0 nil #'equal)
                           (1+ (alist-get (car entry) name-alist 0 nil #'equal))))
                   entries)
             (mapc (lambda (entry)
                     ;; disambiguate same method names
                     (when (> (alist-get (car entry) name-alist 0 nil #'equal) 1)
                       (setcar entry
                               (format "%s [%s]"
                                       (car entry)
                                       (javaimp--concat-scope-parents
                                        (nth 3 entry))))))
                   entries)))
          ((eq javaimp-imenu-group-methods 'qualified)
           ;; list of qualified methods
           (mapc (lambda (entry)
                   ;; prepend parents to name
                   (setcar entry (concat (javaimp--concat-scope-parents
                                          (nth 3 entry))
                                         "."
                                         (car entry))))
                 (mapcar #'javaimp-imenu--make-entry
                         (seq-sort-by
                          #'javaimp-scope-start #'<
                          (javaimp--collect-nodes
                           #'javaimp--is-imenu-included-method forest)))))

          (t
           ;; group methods inside their enclosing class
           (javaimp--map-nodes
            (lambda (scope)
              (cond ((javaimp--is-classlike scope)
                     ;; sub-alist
                     (cons t (javaimp-scope-name scope)))
                    ((javaimp--is-imenu-included-method scope)
                     ;; entry
                     (cons nil (javaimp-imenu--make-entry scope)))))
            (lambda (res)
              (or (functionp (nth 2 res)) ;entry
                  (cdr res)))             ;non-empty sub-alist
            forest)))))

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
  (let ((scopes (javaimp--parse-get-all-scopes))
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
