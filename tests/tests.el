;;; tests/tests.el --- javaimp tests  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2022  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

(require 'ert)
(require 'javaimp)

(ert-deftest javaimp-get-buffer-classes ()
  (with-temp-buffer
    (insert-file-contents
     (file-name-concat javaimp-basedir "tests" "data" "test1-misc-classes.java"))
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
