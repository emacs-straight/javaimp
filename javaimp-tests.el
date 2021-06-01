;;; javaimp-tests.el --- javaimp tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2021  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

(require 'ert)
(require 'javaimp)

(ert-deftest javaimp-test--maven-projects-from-xml--project ()
  (with-temp-buffer
    (insert "<project/>")
    (let ((projects (javaimp--maven-projects-from-xml
		     (xml-parse-region (point-min) (point-max)))))
      (should (eql (length projects) 1)))))

(ert-deftest javaimp-test--maven-projects-from-xml--projects ()
  (with-temp-buffer
    (insert "<projects><project/><project/></projects>")
    (let ((projects (javaimp--maven-projects-from-xml
		     (xml-parse-region (point-min) (point-max)))))
      (should (eql (length projects) 2)))))


(ert-deftest javaimp-test--parse-get-package ()
  (with-temp-buffer
    (insert "//package org.commented1;
/*package org.commented2;*/
  package org.foo;")
    (should (equal (javaimp--parse-get-package) "org.foo"))))

(ert-deftest javaimp-test--parse-get-file-classes ()
  (should (equal (javaimp--parse-get-file-classes
                  (concat javaimp--basedir "testdata/test-get-file-classes-1.java"))
                 '("org.foo.Top"
                   "org.foo.Top.CInner1"
                   "org.foo.Top.CInner1.CInner1_CInner1"
                   "org.foo.Top.IInner1"
                   "org.foo.Top.IInner1.IInner1_IInner1"
                   "org.foo.Top.IInner1.IInner1_CInner1"
                   "org.foo.Top.EInner1"
                   "org.foo.Top.EInner1.EInner1_EInner1"))))
