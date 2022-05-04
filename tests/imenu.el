;;; tests/imenu.el --- javaimp Imenu tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2022  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

(require 'ert)
(require 'ert-x)
(require 'javaimp)

(defun javaimp-test-imenu--simplify-entries (alist)
  (dolist (elt alist)
    (if (and (= (length elt) 4)
             (functionp (nth 2 elt)))
        (setcdr elt (nth 1 elt))
      (javaimp-test-imenu--simplify-entries (cdr elt)))))


(ert-deftest javaimp-imenu-create-index ()
  (let ((actual (with-temp-buffer
                  (insert-file-contents (ert-resource-file "test1.java"))
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

(ert-deftest javaimp-imenu-create-index-use-sub-alists ()
  (let ((actual (with-temp-buffer
                  (insert-file-contents (ert-resource-file "test1.java"))
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
    (javaimp-test-imenu--simplify-entries actual)
    (should (equal expected actual))))
