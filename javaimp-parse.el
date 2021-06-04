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
(require 'cc-mode)                      ;for java-mode-syntax-table

(defcustom javaimp-parse-format-method-name
  #'javaimp--parse-format-method-name-full
  "Function to format method name, invoked with 3 arguments:
NAME, ARGS and THROWS-ARGS.  The last two are lists with elements
of the form (TYPE . NAME).  For THROWS-ARGS, only TYPE is
present."
  :group 'javaimp
  :type 'function)

(cl-defstruct javaimp-scope
  type ; one of anonymous-class, class, interface, enum, local-class,
       ; method, statement, unknown
  name
  start
  open-brace)

(defconst javaimp--parse-class-keywords
  '("class" "interface" "enum"))
(defconst javaimp--parse-stmt-keywords
  '("if" "for" "while" "switch" "try" "catch" "finally"
    "static"                            ;static initializer block
    ))

(defun javaimp--parse-block-re (keywords)
  (concat
   (regexp-opt keywords 'words)
   (rx (and (+ (syntax whitespace))
            (group (+ (any alnum ?_)))))))

(defconst javaimp--parse-class-re
  (javaimp--parse-block-re javaimp--parse-class-keywords))
(defconst javaimp--parse-stmt-re
  (javaimp--parse-block-re javaimp--parse-stmt-keywords))

(defsubst javaimp--parse-is-class (scope)
  (member (symbol-name (javaimp-scope-type scope)) javaimp--parse-class-keywords))

(defvar javaimp--arglist-syntax-table
  (let ((st (make-syntax-table java-mode-syntax-table)))
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")<" st)
    (modify-syntax-entry ?. "_" st) ; separates parts of fully-qualified type
    st)
  "Enables parsing angle brackets as lists")



;;; Arglist

(defun javaimp--parse-arglist (beg end &optional only-type)
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (with-syntax-table javaimp--arglist-syntax-table ;skip generics like lists
        (goto-char (point-max))
        (ignore-errors
          (let (res)
            (while (not (bobp))
              (push (javaimp--parse-arglist-one-arg only-type) res)
              ;; move back to the previous argument, if any
              (when (javaimp--parse-arglist-until (lambda ()
                                                    (and (not (bobp))
                                                         (= (char-before) ?,))))
                (backward-char)))
            res))))))

(defun javaimp--parse-arglist-one-arg (only-type)
  ;; Parse one argument as type and name backwards starting from point
  ;; and return it in the form (TYPE . NAME).  Name is skipped if
  ;; ONLY-TYPE is non-nil.  Leave point at where the job is done:
  ;; skipping further backwards is done by the caller.
  (let ((limit (progn
                 (skip-syntax-backward "-")
                 (point)))
        name)
    ;; Parse name
    (unless only-type
      (if (= 0 (skip-syntax-backward "w_"))
          (error "Cannot parse argument name"))
      (setq name (buffer-substring-no-properties (point) limit))
      (skip-syntax-backward "-")
      (setq limit (point)))
    ;; Parse type: allow anything, but stop at the word boundary which
    ;; is not inside list (this is presumably the type start..)
    (if (javaimp--parse-arglist-until (lambda ()
                                        (save-excursion
                                          (skip-syntax-forward "-" limit)
                                          (looking-at "\\_<"))))
        (progn
          (skip-syntax-forward "-")     ;undo skipping by ..-until
          (let ((type (replace-regexp-in-string
                       "[[:space:]\n]+" " "
                       (buffer-substring-no-properties (point) limit))))
            (cons type name)))
      (error "Cannot parse argument type"))))

(defun javaimp--parse-arglist-until (stop-p)
  ;; Goes backwards until position at which STOP-P returns non-nil.
  ;; Returns non-nil if stopped on this condition, nil if reached bob.
  ;; STOP-P is invoked (possibly at the bob) without arguments and
  ;; should not move point.  Backward movement also skips any
  ;; whitespace, so STOP-P looking forward should be prepared to
  ;; see leading whitespace.
  (catch 'done
    (while t
      (skip-syntax-backward "-")
      (let ((state (syntax-ppss)))
        (cond ((syntax-ppss-context state)
               ;; move out of comment/string if in one
               (goto-char (nth 8 state)))
              ((funcall stop-p)
               (throw 'done t))
              ((bobp)
               (throw 'done nil))
              ((= (char-syntax (char-before)) ?\))
               (backward-list))
              (t
               (backward-char)))))))



;;; Formatting

(defsubst javaimp--parse-format-method-name-full (name args throws-args)
  "Outputs NAME, ARGS (name and type) and THROWS-ARGS (only type)."
  (concat name
          "("
          (mapconcat (lambda (arg)
                       (concat (car arg) " " (cdr arg)))
                     args
                     ", ")
          ")"
          (if throws-args
              (concat " throws "
                      (mapconcat #'car throws-args ", ")))
          ))

(defsubst javaimp--parse-format-method-name-types (name args throws-args)
  "Outputs NAME and ARGS (only type)."
  (concat name
          "("
          (mapconcat #'car args ", ")
          ")"
          ))


;;; Scopes

(defvar javaimp--parse-scope-hook
  '(;; should be before method/stmt because looks similar, but with
    ;; "new " in front
    javaimp--parse-scope-anonymous-class
    javaimp--parse-scope-method-or-stmt
    javaimp--parse-scope-class
    javaimp--parse-scope-unknown        ; catch-all
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
    ;; skip arg-list and ws
    (when (and (progn
                 (skip-syntax-backward "-")
                 (= (char-before) ?\)))
               (ignore-errors
                 (goto-char
                  (scan-lists (point) -1 0))))
      (skip-syntax-backward "-")
      (let ((end (point)))
        (when (and (re-search-backward "\\<new\\s-+" nil t)
                   (not (save-match-data
                          (search-forward "(" end t))))
          (make-javaimp-scope :type 'anonymous-class
                              :name (concat "Anon_"
                                            (buffer-substring-no-properties
                                             (match-end 0) end))
                              :start (match-beginning 0)
                              :open-brace (nth 1 state)))))))

(defun javaimp--parse-scope-method-or-stmt (state)
  (save-excursion
    (let ((throws-args (javaimp--parse-scope-method-throws state)))
      (when (and (not (eq throws-args t))
                 (progn
                   (skip-syntax-backward "-")
                   (= (char-before) ?\)))
                 (ignore-errors
                   (goto-char
                    (scan-lists (point) -1 0))))
        (let* (;; leave open/close parens out
               (arglist-region (cons (1+ (point))
                                     (1- (scan-lists (point) 1 0))))
               (count (progn
                        (skip-syntax-backward "-")
                        (skip-syntax-backward "w_")))
               (name (and (< count 0)
                          (buffer-substring-no-properties
                           (point) (+ (point) (abs count)))))
               (type (when name
                       (if (and (member name javaimp--parse-stmt-keywords)
                                (not throws-args))
                           'statement 'method))))
          (when type
            (make-javaimp-scope
             :type type
             :name (if (eq type 'statement)
                       name
                     (funcall javaimp-parse-format-method-name
                              name
                              (javaimp--parse-arglist (car arglist-region)
                                                      (cdr arglist-region))
                              throws-args))
             :start (point)
             :open-brace (nth 1 state))))))))

(defun javaimp--parse-scope-method-throws (state)
  "Subroutine of `javaimp--parse-scope-method-or-stmt'.  Attempts
to parse throws clause backwards, returning THROWS-ARGS in the
same format as `javaimp--parse-arglist' (but here, only TYPE
element component will be present).  Point is left at the
position from where method signature parsing may be continued.
Returns t if parsing failed and should not be continued."
  (let ((pos (point)))
    (when (re-search-backward "\\<throws\\s-+" nil t)
      (if (ignore-errors
            ;; Does our found throws belong to the right block?
            (= (scan-lists (match-end 0) 1 -1)
               (1+ (nth 1 state))))
          (let ((res (save-match-data
                       (javaimp--parse-arglist (match-end 0)
                                               (save-excursion
                                                 (goto-char pos)
                                                 (skip-syntax-backward "-")
                                                 (point))
                                               t))))
            (if res
                (goto-char (match-beginning 0))
              ;; Something's wrong here: tell the caller to stop
              ;; parsing
              (setq res t))
            res)
        ;; just return to where we started
        (goto-char pos)
        nil))))


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



;; Main

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
