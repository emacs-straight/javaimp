;;; tests/tests.el --- javaimp tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2022  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

(require 'ert)
(require 'ert-x)
(require 'javaimp)

(ert-deftest javaimp-collect-idents ()
  (with-temp-buffer
    (insert-file-contents (ert-resource-file "test1.java"))
    (should (equal (mapcar #'javaimp--ident-to-fqcn
                           (javaimp--collect-idents
                            (javaimp-scope-defun-p) (current-buffer) t))
                   '("org.foo.Top"
                     "org.foo.Top.CInner1"
                     "org.foo.Top.CInner1.CInner1_CInner1"
                     "org.foo.Top.IInner1"
                     "org.foo.Top.IInner1.IInner1_CInner1"
                     "org.foo.Top.IInner1.IInner1_IInner1"
                     "org.foo.Top.EnumInner1"
                     "org.foo.Top.EnumInner1.EnumInner1_EInner1"
                     "org.foo.ColocatedTop")))))
