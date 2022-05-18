;;; tests/imenu.el --- javaimp Imenu tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2022  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

(require 'ert)
(require 'javaimp)
(require 'javaimp-tests)

(defun javaimp-test-imenu--simplify-entries (alist)
  (dolist (elt alist)
    (if (and (= (length elt) 4)
             (functionp (nth 2 elt)))
        (setcdr elt nil)
      (javaimp-test-imenu--simplify-entries (cdr elt)))))


(ert-deftest javaimp-imenu-create-index ()
  (let ((actual (javaimp-with-temp-buffer "test1.java"
                  (javaimp-imenu-create-index)))
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

(ert-deftest javaimp-imenu-create-index-use-sub-alists ()
  (let ((actual (javaimp-with-temp-buffer "test1.java"
                  (let ((javaimp-imenu-use-sub-alists t))
                    (javaimp-imenu-create-index))))
        (expected
         '(("Top"
            ("CInner1"
             ("foo()")
             ("CInner1_CInner1"
              ("foo()")
              ("abstract_method()")
              ("bar()")
              ("baz()")))
            ("IInner1"
             ("foo()")
             ("abstract_method()")
             ("IInner1_CInner1"
              ("foo()"))
             ("baz()")
             ("defaultMethod(String)")
             ("IInner1_IInner1"
              ("foo()")
              ("defaultMethod(String)")
              ("baz()")))
            ("EnumInner1"
             ("EnumInner1()")
             ("foo()")
             ;; "EnumInner1_EInner1" omitted because no methods inside
             ))
           ("ColocatedTop"
            ("foo()")
            ("bar(String,String)")))))
    (javaimp-test-imenu--simplify-entries actual)
    (should (equal expected actual))))
