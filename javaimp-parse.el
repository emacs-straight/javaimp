;;; javaimp-parse.el --- javaimp parsing  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Filipp Gunbin <fgunbin@fastmail.fm>
;; Maintainer: Filipp Gunbin <fgunbin@fastmail.fm>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 'seq)

(cl-defstruct javaimp-scope
  type ; one of anonymous-class, class, interface, enum, local-class, unknown
  name
  start
  open-brace)

(defsubst javaimp--parse-is-class (scope)
  (memq (javaimp-scope-type scope) '(class interface enum)))

(defconst javaimp--parse-class-re
  (concat
   (regexp-opt '("class" "interface" "enum") 'words)
   (rx (and (+ (syntax whitespace))
            (group (+ (any alnum ?_)))))))


(defun javaimp--parse-get-package ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward "^\\s *package\\s +\\([^;]+\\)\\s *;" nil t)
          (let ((state (syntax-ppss)))
            (unless (syntax-ppss-context state)
              (throw 'found (match-string 1)))))))))


(defvar javaimp--parse-scope-hook
  '(javaimp--parse-scope-class
    javaimp--parse-scope-anonymous-class
    javaimp--parse-scope-unknown      ;fallback
    ))

(defun javaimp--parse-scope-class (state)
  (save-excursion
    (if (and (re-search-backward javaimp--parse-class-re nil t)
             ;; if there's no paren in between - assume we're looking at
             ;; class declaration
             (not (save-match-data
                    (search-forward "(" (nth 1 state) t))))
        (make-javaimp-scope :type (intern (match-string 1))
                            :name (match-string 2)
                            :start (match-beginning 1)
                            :open-brace (nth 1 state)))))

(defun javaimp--parse-scope-anonymous-class (state)
  (save-excursion
    (let (end)
      (if (and (re-search-backward "\\<new\\s +" nil t)
               ;; skip arg list and ws
               (setq end (save-excursion
                           (ignore-errors
                             (goto-char
                              (scan-lists (nth 1 state) -1 0))
                             (skip-syntax-backward "-")
                             (point))))
               (not (save-match-data
                      (search-forward "(" end t))))
          (make-javaimp-scope :type 'anonymous-class
                              :name (concat
                                     "Anon_"
                                     (buffer-substring-no-properties
                                      (match-end 0) end))
                              :start (match-beginning 0)
                              :open-brace (nth 1 state))))))

(defun javaimp--parse-scope-unknown (state)
  (make-javaimp-scope :type 'unknown
                      :name "unknown"
                      :start nil
                      :open-brace (nth 1 state)))

(defun javaimp--parse-scopes (count)
  (let ((state (syntax-ppss))
        res)
    (unless (syntax-ppss-context state)
      (save-excursion
        (while (and (nth 1 state)
                    (or (not count)
                        (>= (setq count (1- count)) 0)))
          ;; find innermost enclosing open-bracket
          (goto-char (nth 1 state))
          (when (= (char-after) ?{)
            (let ((scope (run-hook-with-args-until-success
                          'javaimp--parse-scope-hook state)))
              (push scope res)
              (if (javaimp-scope-start scope)
                  (goto-char (javaimp-scope-start scope)))))
          (setq state (syntax-ppss)))))
    ;; if a class is enclosed in anything other than a class, then it
    ;; should be local
    (let ((tmp res)
          in-local)
      (while tmp
        (if (javaimp--parse-is-class (car tmp))
            (if in-local
                (setf (javaimp-scope-type (car tmp)) 'local-class))
          (setq in-local t))
        (setq tmp (cdr tmp))))
    res))

(defun javaimp--parse-get-file-classes (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let ((parse-sexp-ignore-comments t)
          (package (javaimp--parse-get-package))
          res)
      (while (re-search-forward javaimp--parse-class-re nil t)
        (when (and (ignore-errors
                     (goto-char (scan-lists (point) 1 -1)))
                   (= (char-before) ?{))
          (let ((scopes (javaimp--parse-scopes nil))
                curr)
            (when (seq-every-p #'javaimp--parse-is-class scopes)
              (setq curr (mapconcat #'javaimp-scope-name scopes "."))
              (if package
                  (setq curr (concat package "." curr)))
              (push curr res)))))
      (nreverse res))))

(provide 'javaimp-parse)
