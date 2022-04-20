;;; javaimp.el --- Add and reorder Java import statements in Maven/Gradle projects  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2021  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>
;; Version: 0.8
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
;; This module does not add all needed imports automatically!  It only
;; helps you to quickly add imports when stepping through compilation
;; errors.  In addition, this module provides good Imenu support for
;; Java source files - with nesting and abstract methods in interfaces
;; and abstract classes.  It provides suitable functions to use as
;; beginning-of-defun-function / end-of-defun-function as well.
;;
;;
;;   Quick start:
;;
;; - Customize `javaimp-import-group-alist'.
;;
;; - Call `javaimp-visit-project', giving it the top-level build file
;; of your project.  If called within a project, supplies useful
;; default candidates in minibuffer input (topmost build file in the
;; current directory hierarchy, then nested ones).
;;
;; - Then in a Java buffer visiting a file under that project or one
;; of its submodules call `javaimp-organize-imports' or
;; `javaimp-add-import'.
;;
;;
;;   Some details:
;;
;; Contents of jar files, list of classes in source files, and
;; Maven/Gradle project structures are cached, so usually only the
;; first command should take a considerable amount of time to
;; complete.  Project structure is re-read if a module's build file or
;; any of its parents' build files (within visited tree) was modified
;; since last check.  `javaimp-flush-cache' clears jar / source cache.
;;
;; To forget visited projects structure eval this:
;; (setq javaimp-project forest nil)
;;
;; Project structure and dependency information is retrieved from the
;; build tool, see `javaimp--maven-visit' and `javaimp--gradle-visit',
;; and the `javaimp-handler-regexp-alist' variable.  The output from
;; the build tool can be inspected in buffer named by
;; `javaimp-tool-output-buf-name' variable.  If there exists
;; Maven/Gradle wrapper in the project directory, as it is popular
;; these days, it will be used in preference to `javaimp-mvn-program'
;; / `javaimp-gradle-program'.
;;
;; See docstring of `javaimp-add-import' for how import completion
;; alternative are collected.
;;
;; If you get jar reading errors with Gradle despite following
;; recommendation which is shown (text from
;; `javaimp--jar-error-header' followed by offending jars), then it
;; might be the case that Gradle reordered build in such a way that
;; those jars are really not built yet.  In this case, just build them
;; manually, like: './gradlew :project1:build :project2:build'.
;;
;; Important defcustoms are:
;;
;; - `javaimp-java-home' - used to obtain classes in the JDK, and also
;; the build tool is invoked with JAVA_HOME environment variable set
;; to it.  It's initialized from JAVA_HOME env var, so typically it's
;; not required to set it explicitly in Lisp.
;;
;; - `javaimp-parse-current-module' - determines whether we parse the
;; current module for the list of classes.  Parsing is implemented in
;; javaimp-parse.el using `syntax-ppss', generally is simple (we do
;; not try to parse the source completely - just the interesting
;; pieces), but can be time-consuming for large projects (to be
;; improved).  Currently, on the author's machine, source for
;; java.util.Collections from JDK 11 (~ 5600 lines and > 1000
;; "scopes") parses in ~1.5 seconds, which is not that bad...
;;
;; Parsing is also used for Imenu support and for navigation commands.
;; As there's no minor/major mode (yet), you have to set
;; `imenu-create-index-function' and `beginning-of-defun-function' /
;; `end-of-defun-function' in major mode hook yourself.  See example
;; below.
;;
;; - `javaimp-imenu-use-sub-alists' - if non-nil then Imenu items are
;; presented in a nested fashion, instead of a flat list (the
;; default).
;;
;; See other defcustoms via 'M-x customize-group javaimp'.
;;
;;
;; `javaimp-show-scopes' lists all parsed "scopes" (blocks of code in
;; braces), with support for `next-error'.
;;
;;
;; Configuration example:
;;
;; (require 'javaimp)
;; (add-to-list 'javaimp-import-group-alist
;;   '("\\`\\(my\\.company\\.\\|my\\.company2\\.\\)" . 80))
;; (global-set-key (kbd "C-c J v") 'javaimp-visit-project)
;;
;;
;; And in `java-mode-hook':
;;
;; (local-set-key (kbd "C-c i") #'javaimp-add-import)
;; (local-set-key (kbd "C-c o") #'javaimp-organize-imports)
;; (local-set-key (kbd "C-c s") #'javaimp-show-scopes)
;;
;; To set imenu and navigation functions use:
;;
;; (setq imenu-create-index-function #'javaimp-imenu-create-index)
;;
;; (setq beginning-of-defun-function #'javaimp-beginning-of-defun)
;; (setq end-of-defun-function #'javaimp-end-of-defun)
;; (define-key java-mode-map (kbd "C-M-a") #'beginning-of-defun)
;; (define-key java-mode-map (kbd "C-M-e") #'end-of-defun)
;;


;;; Code:

(require 'javaimp-maven)
(require 'javaimp-gradle)
(require 'javaimp-parse)
(require 'javaimp-util)
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

(defcustom javaimp-java-home
  (let ((val (getenv "JAVA_HOME")))
    (and val (not (string-blank-p val))
         val))
  "Path to the JDK.  The directory given should contain
subdirectory \"jre/lib\" (pre-JDK9) or just \"lib\".  By default,
it is initialized from the JAVA_HOME environment variable."
  :type 'string)

(defcustom javaimp-additional-source-dirs nil
  "List of directories where additional (e.g. generated)
source files reside.

Each directory in the list should be a name relative to project
build directory.

With Maven, typically you would check the documentation for a
particular plugin, look for default value there, and add it to
this list.
E.g. \"${project.build.directory}/generated-sources/<plugin_name>\"
becomes \"generated-sources/<plugin_name>\" (note the absence of
the leading slash)."
  :type '(repeat (string :tag "Relative directory")))

(defcustom javaimp-parse-current-module t
  "If non-nil, javaimp will try to parse current module's source
files to determine completion alternatives, in addition to those
from module dependencies.  This can be time-consuming, that's why
this defcustom exists, to turn it off if it's annoying (perhaps
in per-directory locals)."
  :type 'boolean)

(defcustom javaimp-imenu-use-sub-alists nil
  "If non-nil, make sub-alist for each containing scope (e.g. a
class)."
  :type 'boolean)

(defcustom javaimp-verbose nil
  "If non-nil, be verbose."
  :type 'boolean)


(defcustom javaimp-jar-program "jar"
  "Path to the `jar' program used to read contents of jar files."
  :type 'string)

(defcustom javaimp-jmod-program "jmod"
  "Path to the `jmod' program used to read contents of jmod files."
  :type 'string)

(defcustom javaimp-cygpath-program
  (if (eq system-type 'cygwin) "cygpath")
  "Path to the `cygpath' program (Cygwin only)."
  :type 'string)

(defcustom javaimp-mvn-program "mvn"
  "Path to the `mvn' program.  If the visited project has local
mvnw (Maven wrapper), it is used in preference."
  :type 'string)

(defcustom javaimp-gradle-program "gradle"
  "Path to the `gradle' program.  If the visited project has local
gradlew (Gradle wrapper), it is used in preference."
  :type 'string)



;; Variables

(defvar javaimp-handler-regexp-alist
  `(("\\`build.gradle" . ,#'javaimp--gradle-visit)
    ("\\`pom.xml\\'" . ,#'javaimp--maven-visit))
  "Alist of file name patterns vs corresponding handler function.
A handler function takes one argument, a FILE.")

(defvar javaimp-project-forest nil
  "Visited projects")

(defvar javaimp-jar-file-cache nil
  "Jar file cache, an alist of (FILE . CACHED-FILE), where FILE is
expanded file name and CACHED-FILE is javaimp-cached-file
struct.")

(defvar javaimp-source-file-cache nil
  "Source file cache, an alist of (FILE . CACHED-FILE), where FILE
is expanded file name and CACHED-FILE is javaimp-cached-file
struct.")

(defvar javaimp-syntax-table
  (make-syntax-table java-mode-syntax-table) ;TODO don't depend on cc-mode
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
(defun javaimp-visit-project (file)
  "Loads a project and its submodules from FILE.
FILE should have a handler as per `javaimp-handler-regexp-alist'.
Interactively, finds suitable files in this directory and parent
directories, and offers them as completion alternatives for FILE,
topmost first.

After being processed by this command, the module tree becomes
known to javaimp and `javaimp-add-import' may be called inside
any module's source file."
  (interactive
   (let ((file-regexp (mapconcat #'car javaimp-handler-regexp-alist "\\|"))
         (cur-dir (expand-file-name default-directory))
         files parent)
     (while (setq files (append (directory-files cur-dir t file-regexp) files)
                  ;; Prevent infloop on root
                  parent (file-name-directory (directory-file-name cur-dir))
                  cur-dir (unless (string= parent cur-dir) parent)))
     (list (read-file-name "Visit project from file: " nil files t))))
  (setq file (expand-file-name file))
  (let ((handler (or (assoc-default (file-name-nondirectory file)
                                    javaimp-handler-regexp-alist
                                    #'string-match)
                     (user-error "No handler for file: %s" file))))
    ;; Forget previous tree(s) loaded from this build file, if any.
    ;; Additional project trees (see below) have the same file-orig,
    ;; so there may be several here.
    (when-let ((existing-list
                (seq-filter (lambda (node)
                              (equal (javaimp-module-file-orig
                                      (javaimp-node-contents node))
	                             file))
                            javaimp-project-forest)))
      (if (y-or-n-p "Forget already loaded project(s)?")
          (setq javaimp-project-forest
                (seq-remove (lambda (node)
                              (memq node existing-list))
                            javaimp-project-forest))
        (user-error "Aborted")))
    (let ((trees (funcall handler file)))
      (push (car trees) javaimp-project-forest)
      (dolist (node (cdr trees))
        (when (y-or-n-p
               (format "Include additional project tree rooted at %S?"
                       (javaimp-module-id (javaimp-node-contents node))))
          (push node javaimp-project-forest)))
      (message "Loaded project from %s" file))))


;; Dependencies

(defun javaimp--update-module-maybe (node)
  (let ((module (javaimp-node-contents node))
	need-update ids)
    ;; check if deps are initialized
    (unless (javaimp-module-dep-jars module)
      (message "Will load dependencies for %s" (javaimp-module-id module))
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
  (javaimp--get-file-classes-cached
   file
   'javaimp-jar-file-cache
   #'javaimp--read-jar-classes))

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
  "Import CLASSNAME in the current buffer and call `javaimp-organize-imports'.
Interactively, provide completion alternatives relevant for this
file, additionally filtering them by matching simple class
name (without package) against `symbol-at-point' (with prefix arg
- don't filter).

The set of relevant classes is collected from the following:

- If `javaimp-java-home' is set then add JDK classes, see
`javaimp--get-jdk-classes'.

- If current module can be determined, then add all classes from
its dependencies.

- If `javaimp-parse-current-module' is non-nil, also add classes in
current module or source tree, see
`javaimp--get-current-source-dirs'."
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
          (classes (append
                    ;; jdk
                    (when javaimp-java-home
                      (javaimp--get-jdk-classes javaimp-java-home))
                    ;; module dependencies
                    (when module
                      (javaimp--get-module-deps-classes module))
                    ;; current module or source tree
                    (when javaimp-parse-current-module
                      (seq-mapcat #'javaimp--get-directory-classes
                                  (javaimp--get-current-source-dirs module)))))
          (completion-regexp-list
           (and (not current-prefix-arg)
                (symbol-at-point)
                (list (rx (and symbol-start
                               (literal (symbol-name (symbol-at-point)))
                               eol))))))
     (list (completing-read "Import: " classes nil t nil nil
                            (symbol-name (symbol-at-point))))))
  (javaimp-organize-imports (list (cons classname 'normal))))

(defun javaimp--get-jdk-classes (java-home)
  "If 'jmods' subdirectory exists in JAVA-HOME (Java 9+), read all
.jmod files in it.  Else, if 'jre/lib' subdirectory exists in
JAVA-HOME (earlier Java versions), read all .jar files in it."
  (let ((dir (concat (file-name-as-directory java-home) "jmods")))
    (if (file-directory-p dir)
        (seq-mapcat #'javaimp--get-jar-classes
                    (directory-files dir t "\\.jmod\\'"))
      (setq dir (mapconcat #'file-name-as-directory
                           `(,java-home "jre" "lib") nil))
      (if (file-directory-p dir)
          (seq-mapcat #'javaimp--get-jar-classes
                      (directory-files dir t "\\.jar\\'"))
        (user-error "Could not load JDK classes")))))

(defun javaimp--get-module-deps-classes (module)
  ;; We're not caching full list of classes coming from
  ;; module dependencies because jars may change
  (let (jar-errors)
    (prog1
        (seq-mapcat
         (lambda (jar)
           (condition-case err
               (javaimp--get-jar-classes jar)
             (t
              (push (concat jar ": " (error-message-string err))
                    jar-errors)
              nil)))
         (javaimp-module-dep-jars module))
      (when jar-errors
        (with-output-to-temp-buffer "*Javaimp Jar errors*"
          (princ javaimp--jar-error-header)
          (terpri)
          (dolist (err (nreverse jar-errors))
            (princ err)
            (terpri)))))))

(defun javaimp--get-current-source-dirs (module)
  "Return list of source directories for inspection for Java
sources.  If MODULE is non-nil then result is module source dirs
and additional source dirs.  Otherwise, try to determine the root
of source tree from 'package' directive in the current buffer.
If there's no such directive, then the last resort is just
`default-directory'."
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
    (when javaimp-verbose
      (message "Parsing files in %s..." dir))
    (seq-mapcat #'javaimp--get-file-classes
                (seq-filter (lambda (file)
                              (not (file-symlink-p file)))
                            (directory-files-recursively dir "\\.java\\'")))))

(defun javaimp--get-file-classes (file)
  (if-let ((buf (get-file-buffer file)))
      ;; Don't use cache, just collect what we have in buffer
      (with-current-buffer buf
        (save-excursion
          (save-restriction
            (widen)
            (javaimp--get-buffer-classes))))
    (javaimp--get-file-classes-cached
     file
     'javaimp-source-file-cache
     #'javaimp--read-source-classes)))

(defun javaimp--read-source-classes (file)
  (with-temp-buffer
    (insert-file-contents file)
    ;; We need only class-likes, and this is temp buffer, so for
    ;; efficiency avoid parsing anything else
    (let ((javaimp--parse-scope-hook #'javaimp--parse-scope-class))
      (javaimp--get-buffer-classes))))

(defun javaimp--get-buffer-classes ()
  "Return fully-qualified names of all class-like scopes in the
current buffer.  Anonymous classes are not included."
  (let ((package (javaimp--parse-get-package))
        (scopes (javaimp--parse-get-all-scopes
                 nil nil (javaimp--defun-scope-pred))))
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
(defun javaimp-organize-imports (&optional add-alist)
  "Group import statements according to the value of
`javaimp-import-group-alist' (which see) and print resulting
groups putting one blank line between groups.

If buffer already contains some import statements, put imports at
that same place.  Else, if there's a package directive, put
imports below it, separated by one line.  Else, just put them at
bob.

Classes within a single group are sorted lexicographically.
Imports not matched by any regexp in `javaimp-import-group-alist'
are assigned a default order defined by
`javaimp-import-default-order'.  Duplicate imports are elided.

Additionally, merge imports from ADD-ALIST, an alist of the same
form as CLASS-ALIST in return value of
`javaimp--parse-get-imports'."
  (interactive)
  (barf-if-buffer-read-only)
  (save-excursion
    (save-restriction
      (widen)
      (let ((parsed (javaimp--parse-get-imports)))
        (when (or (cdr parsed) add-alist)
          (javaimp--parse-without-hook
            (javaimp--position-for-insert-imports (car parsed))
            (let ((with-order
		   (mapcar
		    (lambda (import)
		      (let ((order
                             (or (assoc-default (car import)
                                                javaimp-import-group-alist
					        'string-match)
			         javaimp-import-default-order)))
		        (cons import order)))
                    (delete-dups (append (cdr parsed) add-alist))))
                  by-type)
	      (setq with-order
		    (sort with-order
			  (lambda (first second)
			    ;; sort by order then name
			    (if (/= (cdr first) (cdr second))
                                (< (cdr first) (cdr second))
			      (string< (caar first) (caar second))))))
              (setq by-type (seq-group-by #'cdar with-order))
              (javaimp--insert-import-group
               (cdr (assq 'normal by-type)) "import %s;\n")
              (javaimp--insert-import-group
               (cdr (assq 'static by-type)) "import static %s;\n"))
            ;; Make sure there's only one blank line after
            (forward-line -2)
            (delete-blank-lines)
            (end-of-line)
            (insert ?\n)))))))

(defun javaimp--position-for-insert-imports (old-region)
  (if old-region
      (progn
        (delete-region (car old-region) (cdr old-region))
        (goto-char (car old-region)))
    (if (javaimp--parse-get-package)
        (insert "\n\n")
      ;; As a last resort, go to bob and skip comments
      (goto-char (point-min))
      (forward-comment (buffer-size))
      (skip-chars-backward " \t\n")
      (unless (bobp)
        (insert "\n\n")))))

(defun javaimp--insert-import-group (imports fmt)
  (let (prev-order)
    (dolist (import imports)
      ;; If adjacent imports have different order value, insert a
      ;; newline between them
      (and prev-order
	   (/= (cdr import) prev-order)
	   (insert ?\n))
      (insert (format fmt (caar import)))
      (setq prev-order (cdr import)))
    (when imports
      (insert ?\n))))


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
           (or (functionp (nth 2 res))  ; imenu entry
               (cdr res)))              ; non-empty sub-alist
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
  (let* ((defun-scopes
          (javaimp--parse-get-all-scopes
           nil nil (javaimp--defun-scope-pred '(method))))
         (methods (seq-filter
                   (lambda (scope)
                     (eq (javaimp-scope-type scope) 'method))
                   defun-scopes))
         (classes (seq-filter
                   (lambda (scope)
                     (not (eq (javaimp-scope-type scope) 'method)))
                   defun-scopes))
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


;; Show scopes

(defvar javaimp-show-scopes-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map "\C-m" #'javaimp-show-scopes-goto-scope)
    (define-key map [mouse-2] #'javaimp-show-scopes-goto-scope)
    (define-key map "n" #'next-error-no-select)
    (define-key map "p" #'previous-error-no-select)
    (define-key map "l" #'recenter-current-error)
    map)
  "Javaimp Show Scopes keymap.")

;; TODO handle mouse-1
(defun javaimp-show-scopes-goto-scope (event &optional to-start)
  "Go to the opening brace (`javaimp-scope-open-brace') of the scope.
Target scope is determined by location of mouse EVENT, if it's
non-nil.  Else, take the scope at current line.  When TO-START is
non-nil, go to scope start (`javaimp-scope-start') instead of the
opening brace."
  (interactive (list last-nonmenu-event current-prefix-arg))
  (let ((buf (current-buffer))
        (scopes-buf-pos
         (if event
             (cons (window-buffer (posn-window (event-end event)))
                   (posn-point (event-end event)))
           (cons (current-buffer)
                 (point))))
        source-file scope)
    (with-current-buffer (car scopes-buf-pos)
      (setq source-file
            (get-text-property (point-min) 'javaimp-show-scopes-file)
            scope
            (get-text-property (cdr scopes-buf-pos) 'javaimp-show-scopes-scope)))
    (unless (and source-file scope)
      (user-error "No target scope"))
    (pop-to-buffer (find-file-noselect source-file))
    (goto-char (if to-start
                   (javaimp-scope-start scope)
                 (javaimp-scope-open-brace scope)))
    (next-error-found buf (current-buffer))))


(put 'javaimp-show-scopes-mode 'mode-class 'special)
(define-derived-mode javaimp-show-scopes-mode special-mode "Javaimp Show Scopes"
  (setq next-error-function #'javaimp-show-scopes-next-error))

(defun javaimp-show-scopes ()
  "Show scopes in *javaimp-scopes* buffer."
  (interactive)
  (let ((scopes
         (save-excursion
           (save-restriction
             (widen)
             (javaimp--parse-get-all-scopes
              nil nil (javaimp--defun-scope-pred '(method anon-class))))))
        (source-buf (current-buffer))
        (source-default-dir default-directory)
        (buf (get-buffer-create "*javaimp-scopes*")))
    (with-current-buffer buf
      (setq default-directory source-default-dir)
      (javaimp-show-scopes-mode)
      (let ((inhibit-read-only t)
            (buffer-undo-list t))
        (erase-buffer)
        (insert (propertize (format "%s\n\n" (buffer-file-name source-buf))
                            'javaimp-show-scopes-file (buffer-file-name source-buf)))
        (dolist (scope scopes)
          (let ((depth 0)
                (tmp scope))
            (while (setq tmp (javaimp-scope-parent tmp))
              (setq depth (1+ depth)))
            (insert (propertize
                     (format "%5d %2d %s: %s%s"
                             (with-current-buffer source-buf
                               (line-number-at-pos (javaimp-scope-start scope)))
                             depth
                             (cdr (assq (javaimp-scope-type scope)
                                        javaimp--show-scopes-scope-type-abbrevs))
                             (make-string (* 2 depth) ? )
                             (javaimp-scope-name scope))
                     'mouse-face 'highlight
                     'help-echo "mouse-2: go to this scope"
                     'javaimp-show-scopes-scope scope
                     'follow-link t)
                    ?\n)))
        (insert (format "\nTotal: %d scopes\n" (length scopes)))
        (goto-char (point-min))
        (setq next-error-last-buffer buf)
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)))
    (display-buffer buf)))

(defun javaimp-show-scopes-next-error (&optional argp reset)
  "Compatibility function for \\[next-error] invocations."
  (interactive "p")
  (or argp (setq argp 1))
  (when reset
    (if-let ((pos (next-single-property-change
                   (point-min) 'javaimp-show-scopes-scope)))
        (progn
          (goto-char pos)
          (forward-line -1))
      (user-error "No scopes")))
  (forward-line argp)
  (unless (get-text-property (point) 'javaimp-show-scopes-scope)
    (user-error "No more scopes"))
  ;; In case the buffer is visible in a nonselected window.
  (if-let ((win (get-buffer-window (current-buffer) t)))
    (set-window-point win (point)))
  (javaimp-show-scopes-goto-scope nil))



;; Navigation

(defun javaimp-beginning-of-defun (arg)
  "Function to be used as `beginning-of-defun-function'."
  (if (zerop arg)
      t
    (when (> arg 0) (setq arg (1- arg)))
    (let* ((ctx (javaimp--get-sibling-context))
           (prev-idx (or (nth 2 ctx) -1))
           (siblings (nthcdr 3 ctx))
           (target-idx (- prev-idx arg)))
      (cond ((or (not siblings) (< target-idx 0))
             (goto-char (nth 0 ctx))
             nil)
            ((>= target-idx (length siblings))
             (goto-char (nth 1 ctx))
             nil)
            (t
             (goto-char (javaimp-scope-open-brace
                         (nth target-idx siblings))))))))

(defun javaimp-end-of-defun ()
  "Function to be used as `end-of-defun-function'."
  (when (javaimp-scope-p
         (get-text-property (point) 'javaimp-parse-scope))
    (ignore-errors
      (goto-char
       (scan-lists (point) 1 0)))))

(defun javaimp--get-sibling-context ()
  "Return list of the form (FLOOR CEILING PREV-INDEX . SIBLINGS),
where SIBLINGS is a list of all sibling defun scopes.  PREV-INDEX
is the index of the \"previous\" (relative to point) scope in
this list, or nil.  FLOOR and CEILING are positions before and
after this group of defuns."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((pos (point))
             (defun-pred (javaimp--defun-scope-pred '(method anon-class)))
             (enc (javaimp--parse-get-enclosing-scope defun-pred))
             (parent
              (if (and enc (eq (javaimp-scope-type enc) 'method))
                  ;; We're inside a method, and need to look at
                  ;; sibling defuns within same parent (it's ok for
                  ;; parent to be nil)
                  (javaimp-scope-parent enc)
                ;; We're either inside a type (but not within its
                ;; nested defuns), or just at top-level.  Look at
                ;; defuns whose parent is enc.
                enc))
             (parent-beg (and parent (javaimp-scope-open-brace parent)))
             (parent-end (and parent
                              (ignore-errors
                                (scan-lists
                                 (javaimp-scope-open-brace parent) 1 0))))
             (sibling-pred (javaimp--scope-same-parent-pred parent))
             (siblings
              (javaimp--parse-get-all-scopes
               ;; beg/end are not strictly needed, pred is enough, but
               ;; provide them for effectiveness
               parent-beg parent-end
               (lambda (s)
                 (and (funcall defun-pred s)
                      (funcall sibling-pred s)))))
             (prev
              (if (and enc (eq (javaimp-scope-type enc) 'method))
                  enc
                ;; try to find previous defun
                (seq-find (lambda (s)
                            (< (javaimp-scope-open-brace s) pos))
                          (reverse siblings)))))
        (nconc
         (list
          (or parent-beg (point-min))
          (or parent-end (point-max))
          (and prev
               (seq-position siblings prev
                             (lambda (s1 s2)
                               (= (javaimp-scope-open-brace s1)
                                  (javaimp-scope-open-brace s2))))))
         siblings)))))



;; Misc

(defun javaimp-flush-cache ()
  "Flush all caches."
  (interactive)
  (setq javaimp-jar-file-cache nil
        javaimp-source-file-cache nil))

(provide 'javaimp)

;;; javaimp.el ends here
