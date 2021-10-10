;;; javaimp-gradle.el --- javaimp gradle support  -*- lexical-binding: t; -*-

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

(require 'javaimp-util)

(defconst javaimp--gradle-task-body
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "gradleTaskBody.inc.kts" javaimp--basedir))
    (buffer-string))
  "Task body, uses syntax which can be used both in Groovy and Kotlin")

(defun javaimp--gradle-visit (file)
  "Calls gradle on FILE to get various project information.

Passes specially crafted init file as -I argument to gradle and
invokes task contained in it.  This task returns all needed
information."
  (message "Visiting Gradle build file %s..." file)
  (let* ((alists (javaimp--gradle-call file
                                       javaimp--gradle-task-body
                                       #'javaimp--gradle-handler
                                       "javaimpTask"))
         (modules (mapcar (lambda (alist)
                            (javaimp--gradle-module-from-alist alist file))
                          alists)))
    ;; first module is always root
    (message "Building tree for root: %s"
             (javaimp-print-id (javaimp-module-id (car modules))))
    (list
     (javaimp--build-tree (car modules) modules
	                  ;; more or less reliable way to find children
	                  ;; is to look for modules with "this" as the
	                  ;; parent
                          (lambda (el tested)
                            (equal (javaimp-module-parent-id tested)
                                   (javaimp-module-id el)))))))

(defun javaimp--gradle-handler ()
  "Parse current buffer into list of project descriptors, each of
which is an alist of attributes (NAME . VALUE).  Each attribute
occupies one line, and is of the form 'NAME=VALUE'.  See file
'gradleTaskBody.inc.kts' for the script which generates such
output.  Attribute 'id' signifies the start of another
descriptor."
  (goto-char (point-min))
  (let (alist res sym val)
    (while (re-search-forward "^\\([[:alnum:]-]+\\)=\\(.*\\)$" nil t)
      (setq sym (intern (match-string 1))
            val (match-string 2))
      (if (string-blank-p val)
          (setq val nil))
      (when (and (eq sym 'id) alist)    ;start of next module
        (push alist res)
        (setq alist nil))
      (push (cons sym val) alist))
    (when alist                         ;last module
      (push alist res))
    (nreverse res)))

(defun javaimp--gradle-module-from-alist (alist file-orig)
  "Make `javaimp-module' structure out of attribute alist ALIST."
  (make-javaimp-module
   :id (javaimp--gradle-id-from-semi-separated
        (cdr (assq 'id alist)))
   :parent-id (javaimp--gradle-id-from-semi-separated
               (cdr (assq 'parent-id alist)))
   :file (cdr (assq 'file alist))
   :file-orig file-orig
   ;; jar/war supported
   :final-name (when-let ((final-name (javaimp-cygpath-convert-maybe
                                       (cdr (assq 'final-name alist)))))
                 (and (member (file-name-extension final-name) '("jar" "war"))
                      final-name))
   :source-dirs (mapcar #'file-name-as-directory
                        (javaimp--split-native-path
                         (cdr (assq 'source-dirs alist))))
   :build-dir (file-name-as-directory
               (javaimp-cygpath-convert-maybe
                (cdr (assq 'build-dir alist))))
   :dep-jars (javaimp--split-native-path (cdr (assq 'dep-jars alist)))
   :load-ts (current-time)
   :dep-jars-path-fetcher #'javaimp--gradle-fetch-dep-jars-path
   :raw nil))

(defun javaimp--gradle-id-from-semi-separated (str)
  (when str
    (let ((parts (split-string str ";" t))
          artifact)
      (unless (= (length parts) 3)
        (error "Invalid project id: %s" str))
      (setq artifact (nth 1 parts))
      (if (equal artifact ":")
          (setq artifact "<root>")
        ;; convert "[:]foo:bar:baz" into "foo.bar.baz"
        (setq artifact (replace-regexp-in-string
                        ":" "." (string-remove-prefix ":" artifact))))
      (make-javaimp-id :group (nth 0 parts)
                       :artifact artifact
                       :version (nth 2 parts)))))

(defun javaimp--gradle-fetch-dep-jars-path (module)
  ;; Always invoke on root file becase module's file may not exist,
  ;; even if reported by Gradle as project.buildFile
  (javaimp--gradle-call (javaimp-module-file-orig module)
                        javaimp--gradle-task-body
                        (lambda ()
                          (re-search-forward "^dep-jars=\\(.*\\)$")
                          (match-string 1))
                        (concat (if (javaimp-module-parent-id module)
                                    (concat ":" (javaimp-id-artifact (javaimp-module-id module))))
                                ":javaimpTask")))

(defun javaimp--gradle-call (file init-script-body handler task)
  (let* ((is-kotlin (equal (file-name-extension file) "kts"))
         (init-file
          (make-temp-file "javaimp" nil
                          (if is-kotlin ".kts")
                          (funcall (if is-kotlin
                                       #'javaimp--gradle-init-script-kotlin
                                     #'javaimp--gradle-init-script)
                                   init-script-body)))
         (local-gradlew (concat (file-name-directory file) "gradlew")))
    (javaimp--call-build-tool
     (if (file-exists-p local-gradlew)
         local-gradlew
       javaimp-gradle-program)
     handler
     "-q"
     "-b" (javaimp-cygpath-convert-maybe file)
     "-I" (javaimp-cygpath-convert-maybe init-file)
     task)))


(defun javaimp--gradle-init-script (body)
  (concat "
import java.io.File
import java.util.stream.Collectors
allprojects {
  task javaimpTask {"
          body
          "} }"))

(defun javaimp--gradle-init-script-kotlin (body)
  (concat "
import java.io.File
import java.util.stream.Collectors
allprojects {
  tasks.register(\"javaimpTask\") {"
          body
          "} }"))

(provide 'javaimp-gradle)
