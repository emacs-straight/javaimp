;;; javaimp-tests.el --- javaimp test util  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2022  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

(defun javaimp-call-with-data (filename handler)
  "Untar FILENAME into temporary directory and call HANDLER,
supplying that directory name as the only arg."
  (let ((tmpdir (file-name-as-directory (make-temp-file "javaimp" t))))
    (unwind-protect
        (let ((rc (call-process
                   "tar" nil nil nil
                   "-x"
                   "-f" filename
                   "-C" tmpdir)))
          (unless (= rc 0)
            (error "Cannot untar test data %s: %d" filename rc))
          (funcall handler tmpdir))
      (delete-directory tmpdir t))))

(provide 'javaimp-tests)
