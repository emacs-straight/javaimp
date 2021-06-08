;;; javaimp-tests.el --- javaimp tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2021  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

(require 'ert)
(require 'javaimp)

;; (ert-deftest javaimp-test--maven-projects-from-xml--project ()
;;   (with-temp-buffer
;;     (insert "<project/>")
;;     (let ((projects (javaimp--maven-projects-from-xml
;; 		     (xml-parse-region (point-min) (point-max)))))
;;       (should (eql (length projects) 1)))))

;; (ert-deftest javaimp-test--maven-projects-from-xml--projects ()
;;   (with-temp-buffer
;;     (insert "<projects><project/><project/></projects>")
;;     (let ((projects (javaimp--maven-projects-from-xml
;; 		     (xml-parse-region (point-min) (point-max)))))
;;       (should (eql (length projects) 2)))))


(defun javaimp-test--check-scope (parse-hook test-data)
  (declare (indent 1))
  (dolist (data test-data)
    (with-temp-buffer
      (insert (nth 0 data))
      (java-mode)
      (let* ((parse-sexp-ignore-comments t) ;FIXME remove with major mode
             (parse-sexp-lookup-properties nil)
             (javaimp--parse-scope-hook parse-hook)
             (scope (car (javaimp--parse-scopes 1))))
        (should-not (null scope))
        (should (eq (javaimp-scope-type scope) (nth 1 data)))
        (should (equal (javaimp-scope-name scope) (nth 2 data)))))))

(ert-deftest javaimp-test--parse-scope-class ()
  (javaimp-test--check-scope #'javaimp--parse-scope-class
    '(("public class Foo<Bar, Baz> {"
       class "Foo")
      ("interface Foo<Bar, Baz> {"
       interface "Foo")
      ("private enum Foo {"
       enum "Foo"))))

(ert-deftest javaimp-test--parse-scope-anonymous-class ()
  (javaimp-test--check-scope #'javaimp--parse-scope-anonymous-class
    '((" = new Object<Class1, Class2>(1 + 1, baz) {"
       anonymous-class "Anon_Object<Class1, Class2>")
      (" = (obj.getField()).new Object<Class1, Class2>(1, baz) {"
       anonymous-class "Anon_Object<Class1, Class2>")
      (" = obj.new Object<>(1, baz) {"
       anonymous-class "Anon_Object<>"))))

(ert-deftest javaimp-test--parse-scope-method-or-stmt ()
  (javaimp-test--check-scope #'javaimp--parse-scope-method-or-stmt
    '(("static void foo_bar(String a, int b) {"
       method "foo_bar(String a, int b)")
      ("void foo_bar(String a, int b) throws E1, E2 {"
       method "foo_bar(String a, int b) throws E1, E2")
      ("if (foo_bar(a, b) < 2) {"
       statement "if"))))

(ert-deftest javaimp-test--parse-scope-simple-stmt ()
  (javaimp-test--check-scope #'javaimp--parse-scope-simple-stmt
    '(("try {"
       simple-statement "try")
      ;; static initializer also falls in this category
      ("static {"
       simple-statement "static"))))


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


(ert-deftest javaimp-test--get-file-classes ()
  (should (equal (javaimp--get-file-classes
                  (concat javaimp--basedir "testdata/test-get-file-classes-1.java"))
                 '("org.foo.Top"
                   "org.foo.Top.CInner1"
                   "org.foo.Top.CInner1.CInner1_CInner1"
                   "org.foo.Top.IInner1"
                   "org.foo.Top.IInner1.IInner1_IInner1"
                   "org.foo.Top.IInner1.IInner1_CInner1"
                   "org.foo.Top.EInner1"
                   "org.foo.Top.EInner1.EInner1_EInner1"))))

(provide 'javaimp-tests)
