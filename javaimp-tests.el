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


(ert-deftest javaimp-test--parse-arglist ()
  (dolist (data '(("")
                  ("  ")
                  ("int i"
                   ("int" . "i"))
                  (" List<? extends Comparable<? super T>> list, T... elements"
                   ("List<? extends Comparable<? super T>>" . "list")
                   ("T..." . "elements"))
                  ("org.foo.Map <? extends K,   \n? extends V> m, String [] array, int i"
                   ;; TODO remove extra ws within
                   ("org.foo.Map <? extends K, ? extends V>" . "m")
                   ("String []" . "array")
                   ("int" . "i"))
                  (" Bi_Function<? super K, ? super V, ? extends V> function "
                   ("Bi_Function<? super K, ? super V, ? extends V>" . "function"))
                  ("@Annotation1 final int i,
@org.foo.Annotation_2(value_1 = \"value1 , (){}\", value2 = -2.3) String[][] arr"
                   ("int" . "i")
                   ("String[][]" . "arr"))
                  ))
    (with-temp-buffer
      (java-mode)
      (insert (car data))
      (should (equal (javaimp--parse-arglist (point-min) (point-max))
                     (cdr data))))))

(ert-deftest javaimp-test--parse-arglist-throws ()
  (dolist (data '(("")
                  ("  ")
                  ("Exception1"
                   ("Exception1"))
                  (" Exception1 , org.foo_bar_3.Exception_2 "
                   ("Exception1")
                   ("org.foo_bar_3.Exception_2"))
                  ("Exception_1<? extends org.foo.Exception_2<? super Exception3>,
  Exception4<? super Exception5>>, \nException6,Exception7<Exception8>"
                   ("Exception_1<? extends org.foo.Exception_2<? super Exception3>, \
Exception4<? super Exception5>>")
                   ("Exception6")
                   ("Exception7<Exception8>"))))
    (with-temp-buffer
      (java-mode)
      (insert (car data))
      (should (equal (javaimp--parse-arglist (point-min) (point-max) t)
                     (cdr data))))))

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
