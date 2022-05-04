;;; javaimp-tests.el --- javaimp test util  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2022  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

(require 'ert-x)

(defun javaimp-call-with-data (filename handler)
  "Untar FILENAME into temporary directory and call HANDLER,
supplying that directory name as the only arg."
  (ert-with-temp-directory tmpdir
    :prefix "javaimp-test"
    (let ((rc (call-process
               "tar" nil nil nil
               "-x"
               "-f" filename
               "-C" tmpdir)))
      (if (= rc 0)
          (funcall handler tmpdir)
        (error "Cannot untar test data file %s: %d" filename rc)))))

(provide 'javaimp-tests)
