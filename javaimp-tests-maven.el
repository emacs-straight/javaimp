;;; javaimp-tests-maven.el --- javaimp Maven tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2021  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

(require 'ert)
(require 'javaimp)

;; Tests for Maven project parsing.

;; In the untarred directory, use this command to run Maven manually,
;; if needed for debugging:
;;
;; `mvn -U -s settings.xml -f single/pom.xml help:effective-pom'.
;;
;; Dummy settings.xml file:
;;
;; `<?xml version="1.0" encoding="UTF-8"?><settings/>'
;;

(ert-deftest javaimp-test--maven-visit ()
  (let* ((tmpdir (file-name-as-directory (make-temp-file "javaimp" t)))
         (rc (call-process "tar" nil nil nil
                           "-x"
                           "-f" (concat javaimp--basedir
                                        "testdata/maven-projects.tar.gz")
                           "-C" tmpdir)))
    (unless (= rc 0)
      (error "Cannot untar test data: %d" rc))
    ;;
    ;; Single-module project
    (should
     (equal
      (javaimp-test--maven-get-tree
       (concat tmpdir (file-name-as-directory "single")))
      '((("single:org.example:1.0.0" . "pom.xml")))))
    ;;
    ;; Multi-module project
    (should
     (equal
      (javaimp-test--maven-get-tree
       (concat tmpdir (file-name-as-directory "multi")))
      '(;; Main tree:
        (("multi:org.example:1.0.0" . "pom.xml")
         (("child:org.example:1.0.0" . "child/pom.xml"))
         ;; inherited group/version
         (("child-only-artifact:org.example:1.0.0" . "child-only-artifact/pom.xml"))
         ;; aggregator1 has no parent, but its child has
         (("aggregator1-child-of-parent:org.example:1.0.0" .
           "aggregator1/aggregator1-child-of-parent/pom.xml"))
         (("aggregator2:org.example:1.0.0" . "aggregator2/pom.xml")
          (("aggregator2-child-of-aggregator2:org.example:1.0.0" .
            "aggregator2/aggregator2-child-of-aggregator2/pom.xml")))
         ;; contained in aggregator2, but parent is above
         (("aggregator2-child-of-parent:org.example:1.0.0" .
           "aggregator2/aggregator2-child-of-parent/pom.xml")))
        ;;
        ;; Some projects end up outside of main tree:
        ;; - because no parent
        (("submodule-no-parent:org.example:1.0.0" . "submodule-no-parent/pom.xml"))
        ;; - because parent is outside
        (("with-parent-outside:org.example:1.0.0" . "with-parent-outside/pom.xml"))
        ;; - because no parent
        (("aggregator1:org.example:1.0.0" . "aggregator1/pom.xml")
         (("aggregator1-child-of-aggregator1:org.example:1.0.0" .
           "aggregator1/aggregator1-child-of-aggregator1/pom.xml")))
        ;;
        ;; And "dangling-parent-link" project is not present at all,
        ;; because we have no way of knowing about it
        )))))

(defun javaimp-test--maven-get-tree (project-dir)
  (javaimp--map-nodes
   (lambda (mod)
     (cons t
           (cons (javaimp-print-id (javaimp-module-id mod))
                 (file-relative-name (javaimp-module-file mod) project-dir))))
   #'always
   (javaimp--maven-visit (concat project-dir "pom.xml"))))

(provide 'javaimp-tests-maven)
