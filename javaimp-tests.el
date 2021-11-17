;;; javaimp-tests.el --- javaimp tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2021  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

(require 'ert)
(require 'javaimp)

;; Use this selector to run all tests which do not invoke build tool:
;;
;; (and "^javaimp-" (not (tag :runs-build-tool)))
;;

;; Tests for low-level helpers of scope parsers.

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
      (insert (car data))
      (should (equal (javaimp--parse-arglist (point-min) (point-max) t)
                     (cdr data))))))



;; Tests for single scope parsers, which should be in
;; `javaimp--parse-scope-hook'.

(ert-deftest javaimp-test--parse-scope-class ()
  (javaimp-test--single-parser #'javaimp--parse-scope-class
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
      enum "Foo")
    ))

(ert-deftest javaimp-test--parse-scope-anonymous-class ()
  (javaimp-test--single-parser #'javaimp--parse-scope-anonymous-class
    '(" = new Object < Class1 , Class2 > ( 1 + 1 , baz ) {"
      anonymous-class "Object")
    `(,(subst-char-in-string
        ?  ?\n
        " = new Object < Class1 , Class2 > ( 1 + 1 , baz ) {")
      anonymous-class "Object")
    '(" = (obj.getField()).new Object<Class1, Class2>(1, baz) {"
      anonymous-class "Object")
    '(" = obj.new Object<>(1, baz) {"
      anonymous-class "Object")
    ))

(ert-deftest javaimp-test--parse-scope-method-or-stmt ()
  (javaimp-test--single-parser #'javaimp--parse-scope-method-or-stmt
    '("static void foo_bar ( String a , int b ) {"
      method "foo_bar(String,int)")
    `(,(subst-char-in-string
        ?  ?\n
        "static void foo_bar ( String a , int b ) {")
      method "foo_bar(String,int)")
    '("void foo_bar(String a, int b) throws E1, E2 {"
      method "foo_bar(String,int)")
    '("void foo_bar()
throws E1 {"
      method "foo_bar()")
    '("if (foo_bar(a, b) < 2) {"
      statement "if")
    ))

(ert-deftest javaimp-test--parse-scope-simple-stmt ()
  (javaimp-test--single-parser #'javaimp--parse-scope-simple-stmt
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
  (javaimp-test--single-parser #'javaimp--parse-scope-array
    '("new String[] {"
      array "")
    ;; TODO fix
    ;; '("new Object[][] { {"
    ;;   array "")
    ;; '("new int[] {{1, 2}, {"
    ;;   array "")
    ))

(defun javaimp-test--single-parser (parser &rest test-items)
  (declare (indent 1))
  (dolist (item test-items)
    (with-temp-buffer
      (insert (nth 0 item))
      (let* ((javaimp--parse-scope-hook parser)
             (scopes (javaimp--parse-get-all-scopes)))
        (should (= 1 (length scopes)))
        (should (eq (javaimp-scope-type (car scopes)) (nth 1 item)))
        (should (equal (javaimp-scope-name (car scopes)) (nth 2 item)))))))


;; Tests for parsing

(ert-deftest javaimp-test--parse-get-package ()
  (with-temp-buffer
    (insert "  package  foo.bar.baz  ;
//package commented.line;
/*
package commented.block;
*/")
    (should (equal (javaimp--parse-get-package) "foo.bar.baz"))))

(ert-deftest javaimp-test--parse-get-all-scopes ()
  (with-temp-buffer
    (insert-file-contents
     (concat javaimp--basedir "testdata/test1-misc-classes.java"))
    ;; parse full buffer
    (javaimp-test--check-named-scopes)
    ;;
    ;; reparse half of buffer
    (setq javaimp--parse-dirty-pos (/ (- (point-max) (point-min)) 2))
    (javaimp-test--check-named-scopes)
    ;;
    ;; don't reparse
    (javaimp-test--check-named-scopes)))

(defun javaimp-test--check-named-scopes ()
  (let* ((scopes (javaimp--parse-get-all-scopes
                  (lambda (s)
                    (memq (javaimp-scope-type s) '(class interface enum method)))
                  (lambda (s)
                    (memq (javaimp-scope-type s) '(class interface enum method)))))
         (actual (mapcar
                  (lambda (s)
                    (let (res)
                      (while s
                        (push (list (javaimp-scope-type s)
                                    (javaimp-scope-name s))
                              res)
                        (setq s (javaimp-scope-parent s)))
                      (nreverse res)))
                  scopes))
         (expected
          '(((class "Top"))
            ((class "CInner1") (class "Top"))
            ((method "foo()") (class "CInner1") (class "Top"))
            ((class "CInner1_CLocal1")
             (method "foo()") (class "CInner1") (class "Top"))
            ((method "foo()")
             (class "CInner1_CLocal1")
             (method "foo()") (class "CInner1") (class "Top"))
            ((class "CInner1_CLocal1_CLocal1")
             (method "foo()")
             (class "CInner1_CLocal1")
             (method "foo()") (class "CInner1") (class "Top"))
            ((method "foo()")
             (class "CInner1_CLocal1_CLocal1")
             (method "foo()")
             (class "CInner1_CLocal1")
             (method "foo()") (class "CInner1") (class "Top"))

            ((class "CInner1_CLocal2")
             (method "foo()") (class "CInner1") (class "Top"))
            ((method "foo()")
             (class "CInner1_CLocal2")
             (method "foo()") (class "CInner1") (class "Top"))

            ((method "toString()")
             (class "CInner1") (class "Top"))

            ((class "CInner1_CInner1") (class "CInner1") (class "Top"))
            ((method "foo()")
             (class "CInner1_CInner1") (class "CInner1") (class "Top"))
            ((method "bar()")
             (class "CInner1_CInner1") (class "CInner1") (class "Top"))

            ((interface "IInner1") (class "Top"))
            ((method "foo()") (interface "IInner1") (class "Top"))
            ((class "IInner1_CInner1") (interface "IInner1") (class "Top"))
            ((method "foo()")
             (class "IInner1_CInner1") (interface "IInner1") (class "Top"))
            ((method "defaultMethod(String)")
             (interface "IInner1") (class "Top"))

            ((interface "IInner1_IInner1") (interface "IInner1") (class "Top"))
            ((method "defaultMethod(String)")
             (interface "IInner1_IInner1") (interface "IInner1") (class "Top"))

            ((enum "EnumInner1") (class "Top"))
            ((method "EnumInner1()") (enum "EnumInner1") (class "Top"))
            ((method "foo()") (enum "EnumInner1") (class "Top"))
            ((enum "EnumInner1_EInner1") (enum "EnumInner1") (class "Top"))

            ((class "ColocatedTop"))
            ((method "foo()") (class "ColocatedTop"))
            ((method "bar(String,String)") (class "ColocatedTop")))))
    (should (= (length expected) (length actual)))
    (dotimes (i (length expected))
      (should (equal (nth i expected) (nth i actual))))
    ;;
    (let ((data
         `((,(nth 0 scopes) "Top" 26 36)
           (,(nth 16 scopes) "foo()" 1798 1804)
           (,(nth 23 scopes) "EnumInner1_EInner1" 2462 2486)
           (,(nth 25 scopes) "foo()" 2554 2560))))
      (dolist (elt data)
        (let ((scope (nth 0 elt)))
          (should (equal (nth 1 elt) (javaimp-scope-name scope)))
          (should (equal (nth 2 elt) (javaimp-scope-start scope)))
          (should (equal (nth 3 elt) (javaimp-scope-open-brace scope))))))))



;; Tests for javaimp--get-buffer-classes

(ert-deftest javaimp-test--get-buffer-classes ()
  (with-temp-buffer
    (insert-file-contents
     (concat javaimp--basedir "testdata/test1-misc-classes.java"))
    (should (equal (javaimp--get-buffer-classes)
                   '("org.foo.Top"
                     "org.foo.Top.CInner1"
                     "org.foo.Top.CInner1.CInner1_CInner1"
                     "org.foo.Top.IInner1"
                     "org.foo.Top.IInner1.IInner1_CInner1"
                     "org.foo.Top.IInner1.IInner1_IInner1"
                     "org.foo.Top.EnumInner1"
                     "org.foo.Top.EnumInner1.EnumInner1_EInner1"
                     "org.foo.ColocatedTop")))))


;; Tests for imenu function

(ert-deftest javaimp-test--imenu ()
  (let ((actual (with-temp-buffer
                  (insert-file-contents
                   (concat javaimp--basedir "testdata/test1-misc-classes.java"))
                  (let ((imenu-use-markers nil))
                    (javaimp-imenu-create-index))))
        (expected-names
         '("foo() [Top.CInner1]"
           "foo() [Top.CInner1.CInner1_CInner1]"
           "abstract_method() [Top.CInner1.CInner1_CInner1]"
           "bar()"
           "baz() [Top.CInner1.CInner1_CInner1]"
           "foo() [Top.IInner1]"
           "abstract_method() [Top.IInner1]"
           "foo() [Top.IInner1.IInner1_CInner1]"
           "baz() [Top.IInner1]"
           "defaultMethod(String) [Top.IInner1]"
           "foo() [Top.IInner1.IInner1_IInner1]"
           "defaultMethod(String) [Top.IInner1.IInner1_IInner1]"
           "baz() [Top.IInner1.IInner1_IInner1]"
           "EnumInner1()"
           "foo() [Top.EnumInner1]"
           "foo() [ColocatedTop]"
           "bar(String,String)")))
    (should (= (length expected-names) (length actual)))
    (dotimes (i (length expected-names))
      (should (equal (nth i expected-names) (car (nth i actual)))))))

(ert-deftest javaimp-test--imenu-use-sub-alists ()
  (let ((actual (with-temp-buffer
                  (insert-file-contents
                   (concat javaimp--basedir "testdata/test1-misc-classes.java"))
                  (let ((imenu-use-markers nil)
                        (javaimp-imenu-use-sub-alists t))
                    (javaimp-imenu-create-index))))
        (expected
         '(("Top"
            ("CInner1"
             ("foo()" . 98)
             ("CInner1_CInner1"
              ("foo()" . 1099)
              ("abstract_method()" . 1148)
              ("bar()" . 1192)
              ("baz()" . 1281)))
            ("IInner1"
             ("foo()" . 1603)
             ("abstract_method()" . 1715)
             ("IInner1_CInner1"
              ("foo()" . 1798))
             ("baz()" . 1934)
             ("defaultMethod(String)" . 1963)
             ("IInner1_IInner1"
              ("foo()" . 2122)
              ("defaultMethod(String)" . 2157)
              ("baz()" . 2258)))
            ("EnumInner1"
             ("EnumInner1()" . 2353)
             ("foo()" . 2399)
             ;; "EnumInner1_EInner1" omitted because no methods inside
             ))
           ("ColocatedTop"
            ("foo()" . 2554)
            ("bar(String,String)" . 2578)))))
    (javaimp-test--imenu-simplify-entries actual)
    (should (equal expected actual))))

(defun javaimp-test--imenu-simplify-entries (alist)
  (dolist (elt alist)
    (if (and (= (length elt) 4)
             (functionp (nth 2 elt)))
        (setcdr elt (nth 1 elt))
      (javaimp-test--imenu-simplify-entries (cdr elt)))))


;; Utility

(defun javaimp-test--with-data (filename handler)
  "Untars testdata/FILENAME into temporary directory and runs
HANDLER, supplying temp directory name as the only arg."
  (let ((tmpdir (file-name-as-directory (make-temp-file "javaimp" t))))
    (unwind-protect
        (let ((rc (call-process
                   "tar" nil nil nil
                   "-x"
                   "-f" (concat javaimp--basedir
                                (file-name-as-directory "testdata")
                                filename)
                   "-C" tmpdir)))
          (unless (= rc 0)
            (error "Cannot untar test data %s: %d" filename rc))
          (funcall handler tmpdir))
      (delete-directory tmpdir t))))

(provide 'javaimp-tests)
