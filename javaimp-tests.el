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


(defun javaimp-test--check-scope (parse-hook &rest test-items)
  (declare (indent 1))
  (dolist (item test-items)
    (with-temp-buffer
      (insert (nth 0 item))
      (java-mode)
      (let* ((parse-sexp-ignore-comments t) ;FIXME remove with major mode
             (parse-sexp-lookup-properties nil)
             (javaimp--parse-scope-hook parse-hook)
             (scope (car (javaimp--parse-scopes 1))))
        (should-not (null scope))
        (should (eq (javaimp-scope-type scope) (nth 1 item)))
        (should (equal (javaimp-scope-name scope) (nth 2 item)))))))

(ert-deftest javaimp-test--parse-scope-class ()
  (javaimp-test--check-scope #'javaimp--parse-scope-class
    '("class Foo {"
      class "Foo")
    '("class Foo extends Bar {"
      class "Foo")
    '("class Foo implements Bar {"
      class "Foo")
    '("class Foo implements Bar, Baz {"
      class "Foo")
    '("public class Foo extends Bar implements Baz1 , Baz2 {"
      class "Foo")
    `(,(subst-char-in-string
        ?  ?\n
        "public class Foo extends Bar implements Baz1 , Baz2 {")
      class "Foo")
    '("class Foo<Bar, Baz> extends FooSuper<Bar, Baz> \
implements Interface1<Bar, Baz>, Interface2 {"
      class "Foo")
    '("class Foo<E extends Bar> {"
      class "Foo")
    '("class Foo<Enum<?>> {"
      class "Foo")
    '("class Foo<T extends Baz<? extends Baz2>> \
extends Bar<? extends Baz<? extends Baz2>> {"
      class "Foo")
    '("interface Foo<Bar, Baz> {"
      interface "Foo")
    '("private enum Foo {"
      enum "Foo")))

(ert-deftest javaimp-test--parse-scope-anonymous-class ()
  (javaimp-test--check-scope #'javaimp--parse-scope-anonymous-class
    '(" = new Object < Class1 , Class2 > ( 1 + 1 , baz ) {"
      anonymous-class "Object")
    `(,(subst-char-in-string
        ?  ?\n
        " = new Object < Class1 , Class2 > ( 1 + 1 , baz ) {")
      anonymous-class "Object")
    '(" = (obj.getField()).new Object<Class1, Class2>(1, baz) {"
      anonymous-class "Object")
    '(" = obj.new Object<>(1, baz) {"
      anonymous-class "Object")))

(ert-deftest javaimp-test--parse-scope-method-or-stmt ()
  (javaimp-test--check-scope #'javaimp--parse-scope-method-or-stmt
    '("static void foo_bar ( String a , int b ) {"
      method "foo_bar(String a, int b)")
    `(,(subst-char-in-string
        ?  ?\n
        "static void foo_bar ( String a , int b ) {")
      method "foo_bar(String a, int b)")
    '("void foo_bar(String a, int b) throws E1, E2 {"
      method "foo_bar(String a, int b) throws E1, E2")
    '("void foo_bar()
throws E1 {"
      method "foo_bar() throws E1")
    '("if (foo_bar(a, b) < 2) {"
      statement "if")))

(ert-deftest javaimp-test--parse-scope-simple-stmt ()
  (javaimp-test--check-scope #'javaimp--parse-scope-simple-stmt
    '(" try {"
      simple-statement "try")
    `(,(subst-char-in-string ?  ?\n " try {")
      simple-statement "try")
    ;; static initializer
    '("static {"
      simple-statement "static")
    ;; lambda
    '("it -> {"
      simple-statement "lambda")
    '("(x, y) -> {"
      simple-statement "lambda")
    ))

(ert-deftest javaimp-test--parse-scope-array ()
  (javaimp-test--check-scope #'javaimp--parse-scope-array
    '("new String[] {"
      array "")
    '("new Object[][] { {"
      array "")
    '("new int[] {{1, 2}, {"
      array "")))

(ert-deftest javaimp-test--parse-arglist ()
  (dolist (data '(("")
                  ("  ")
                  ("int i"
                   ("int" . "i"))
                  ("\nint\ni\n,\nint\nj\n"
                   ("int" . "i")
                   ("int" . "j"))
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
                  ("\nEx1\n,\nEx2\n"
                   ("Ex1")
                   ("Ex2"))
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
