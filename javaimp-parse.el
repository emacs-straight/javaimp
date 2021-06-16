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
(require 'javaimp-util)

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
       ; method, statement, simple-statement, array, unknown
  name
  start
  open-brace)

(defconst javaimp--parse-class-keywords
  '("class" "interface" "enum"))
(defconst javaimp--parse-stmt-keywords
  '("if" "for" "while" "switch" "try" "catch" "finally"
    "static"                            ;static initializer block
    ))

(defsubst javaimp--parse-is-class (scope)
  (member (symbol-name (javaimp-scope-type scope)) javaimp--parse-class-keywords))

(defvar javaimp--arglist-syntax-table
  (let ((st (make-syntax-table java-mode-syntax-table))) ;TODO don't depend
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")<" st)
    (modify-syntax-entry ?. "_" st) ; separates parts of fully-qualified type
    st)
  "Enables parsing angle brackets as lists")


(defun javaimp--parse-arglist (beg end &optional only-type)
  "Parse arg list between BEG and END, of the form 'TYPE NAME,
...'.  Return list of conses (TYPE . NAME).  If ONLY-TYPE is
non-nil, then name parsing is skipped."
  (save-excursion
    (save-restriction
      (syntax-ppss-flush-cache beg)
      (narrow-to-region beg end)
      (prog1
          (with-syntax-table javaimp--arglist-syntax-table
            (goto-char (point-max))
            (ignore-errors
              (let (res)
                (while (progn
                         (javaimp--parse-skip-back-until)
                         (not (bobp)))
                  (push (javaimp--parse-arglist-one-arg only-type) res)
                  ;; move back to the previous argument, if any
                  (when (javaimp--parse-skip-back-until
                         (lambda (_last-what _last-pos)
                           (and (not (bobp))
                                (= (char-before) ?,))))
                    (backward-char)))   ; skip comma
                res)))
        (syntax-ppss-flush-cache beg)))))

(defun javaimp--parse-arglist-one-arg (only-type)
  "Parse one argument as type and name backwards starting from
point and return it in the form (TYPE . NAME).  Name is skipped
if ONLY-TYPE is non-nil.  Leave point at where the job is done:
skipping further backwards is done by the caller."
  (let ((limit (progn
                 (javaimp--parse-skip-back-until)
                 (point)))
        name)
    ;; Parse name
    (unless only-type
      (if (= 0 (skip-syntax-backward "w_"))
          (error "Cannot parse argument name")
        (setq name (buffer-substring-no-properties (point) limit))
        (javaimp--parse-skip-back-until)
        (setq limit (point))))
    ;; Parse type: allow anything, but stop at the word boundary which
    ;; is not inside list (this is presumably the type start..)
    (if-let ((last-skip
              (javaimp--parse-skip-back-until
               (lambda (_last-what last-pos)
                 (save-excursion
                   (if last-pos (goto-char last-pos))
                   (looking-at "\\_<"))))))
        (progn
          (unless (eq last-skip t)
            (goto-char (cdr last-skip))) ;undo skipping by ..-until
          (let ((type (replace-regexp-in-string
                       "[[:space:]\n]+" " "
                       (buffer-substring-no-properties (point) limit))))
            (cons type name)))
      (error "Cannot parse argument type"))))

(defun javaimp--parse-skip-back-until (&optional stop-p)
  "Goes backwards until position at which STOP-P returns non-nil, or reaching bob.

STOP-P is invoked with two arguments which describe the last
non-ws thing skipped: LAST-WHAT (symbol - either 'list' or
'char') and LAST-POS.  If STOP-P returns non-nil, then the return
value is also non-nil: either (LAST-WHAT . LAST-POS) if both are
non-nil or t.  Otherwise the return value is nil.

If STOP-P wants to look forward, it should be prepared to see
whitespace / comments, this is because backward movement skips
them before invoking STOP-P.  It should not move point.  If
omitted, it defaults to `always', in this case the effect of the
function is to just skip whitespace / comments."
  (or stop-p (setq stop-p #'always))
  (catch 'done
    (let (last-what last-pos)
      (while t
        (skip-syntax-backward " ")
        (let ((state (syntax-ppss)))
          (cond ((syntax-ppss-context state)
                 ;; move out of comment/string if in one
                 (goto-char (nth 8 state)))
                ((and (not (bobp))
                      ;; FIXME use syntax-after instead
                      (member (char-syntax (char-before)) '(?> ?!)))
                 (backward-char))
                ((funcall stop-p last-what last-pos)
                 (throw 'done (if (and last-what last-pos)
                                  (cons last-what last-pos)
                                t)))
                ((bobp)
                 (throw 'done nil))
                ((= (char-syntax (char-before)) ?\))
                 (backward-list)
                 (setq last-what 'list
                       last-pos (point)))
                (t
                 (backward-char)
                 (setq last-what 'char
                       last-pos (point)))))))))

(defun javaimp--parse-preceding (regexp scope-start &optional skip-count)
  "Returns non-nil if a match for REGEXP is found before point.
Matches inside comments / strings are skipped.  Potential match
is checked to be SKIP-COUNT lists away from the SCOPE-START (1 is
for scope start itself, so if you want to skip one additional
list, use 2 etc.).  If a match is found, then match-data is set,
as for `re-search-backward'."
  (and (javaimp--rsb-outside-context regexp nil t)
       (ignore-errors
         ;; Does our match belong to the right block?
         (= (scan-lists (match-end 0) (or skip-count 1) -1)
            (1+ scope-start)))))

(defun javaimp--parse-decl-suffix (regexp state &optional bound)
  "Subroutine of scope parsers.  Attempts to parse declaration
suffix backwards (but not farther than BOUND), returning ARGS in
the same format as `javaimp--parse-arglist' (but here, only TYPE
element component will be present).  Point is left at the
position from where scope parsing may be continued.  Returns t if
parsing failed and should not be continued."
  (let ((pos (point)))
    (when (javaimp--rsb-outside-context regexp bound t)
      (if (ignore-errors             ;As in javaimp--parse-preceding
            (= (scan-lists (match-end 0) 1 -1)
               (1+ (nth 1 state))))
          (let ((res (save-match-data
                       (javaimp--parse-arglist (match-end 0) pos t))))
            (if res
                (goto-char (match-beginning 0))
              ;; Something's wrong here: tell the caller to stop
              ;; parsing
              (setq res t))
            res)
        ;; just return to where we started
        (goto-char pos)
        nil))))



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

(defsubst javaimp--parse-format-method-name-types (name args _throws-args)
  "Outputs NAME and ARGS (only type)."
  (concat name
          "("
          (mapconcat #'car args ", ")
          ")"
          ))


;;; Scopes

(defvar javaimp--parse-scope-hook
  '(javaimp--parse-scope-array
    ;; anonymous-class should be before method/stmt because it looks
    ;; similar, but with "new" in front
    javaimp--parse-scope-anonymous-class
    javaimp--parse-scope-class
    javaimp--parse-scope-simple-stmt
    javaimp--parse-scope-method-or-stmt
    javaimp--parse-scope-unknown
    ))

(defun javaimp--parse-scope-class (state)
  "Attempts to parse 'class' / 'interface' / 'enum' scope.  Some of
those may later become 'local-class' (see `javaimp--parse-scopes')."
  (save-excursion
    (if (javaimp--parse-preceding (regexp-opt javaimp--parse-class-keywords 'words)
                                  (nth 1 state))
        (let* ((keyword-start (match-beginning 1))
               (keyword-end (match-end 1))
               (decl-suffix (progn
                              (goto-char (nth 1 state))
                              (or (javaimp--parse-decl-suffix "\\<extends\\>"
                                                              state keyword-end)
                                  (javaimp--parse-decl-suffix "\\<implements\\>"
                                                              state keyword-end))))
               arglist)
          (unless (eq decl-suffix t)
            ;; we either skipped back over the valid declaration
            ;; suffix(-es), or there isn't any
            (setq arglist (javaimp--parse-arglist keyword-end (point) t))
            (when (= (length arglist) 1)
              (make-javaimp-scope :type (intern
                                         (buffer-substring-no-properties
                                          keyword-start keyword-end))
                                  :name (caar arglist)
                                  :start keyword-start
                                  :open-brace (nth 1 state))))))))

(defun javaimp--parse-scope-simple-stmt (state)
  "Attempts to parse `simple-statement' scope."
  (save-excursion
    (and (javaimp--parse-skip-back-until)
         (looking-back (concat
                        (regexp-opt javaimp--parse-stmt-keywords 'words)
                        "\\|->")
                       nil t)
         (make-javaimp-scope
          :type 'simple-statement
          :name (or (match-string 1)
                    "lambda")
          :start (or (match-beginning 1)
                     (- (point) 2))
          :open-brace (nth 1 state)))))

(defun javaimp--parse-scope-anonymous-class (state)
  "Attempts to parse `anonymous-class' scope."
  (save-excursion
    ;; skip arg-list and ws
    (when (and (progn
                 (javaimp--parse-skip-back-until)
                 (= (char-before) ?\)))
               (ignore-errors
                 (goto-char
                  (scan-lists (point) -1 0))))
      (let ((end (point))
            start arglist)
        (when (javaimp--parse-preceding "\\<new\\>" (nth 1 state) 2)
          (setq start (match-beginning 0)
                arglist (javaimp--parse-arglist (match-end 0) end t))
          (when (= (length arglist) 1)
            (make-javaimp-scope :type 'anonymous-class
                                :name (caar arglist)
                                :start start
                                :open-brace (nth 1 state))))))))

(defun javaimp--parse-scope-method-or-stmt (state)
  "Attempts to parse `method' or `statement' scope."
  (save-excursion
    (let ((throws-args (javaimp--parse-decl-suffix "\\<throws\\>" state)))
      (when (and (not (eq throws-args t))
                 (progn
                   (javaimp--parse-skip-back-until)
                   (= (char-before) ?\)))
                 (ignore-errors
                   (goto-char
                    (scan-lists (point) -1 0))))
        (let* (;; leave open/close parens out
               (arglist-region (cons (1+ (point))
                                     (1- (scan-lists (point) 1 0))))
               (count (progn
                        (javaimp--parse-skip-back-until)
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

(defun javaimp--parse-scope-array (state)
  "Attempts to parse 'array' scope."
  (save-excursion
    (and (javaimp--parse-skip-back-until)
         (member (char-before) '(?, ?{ ?\]))
         (make-javaimp-scope :type 'array
                             :name ""
                             :start nil
                             :open-brace (nth 1 state)))))

(defun javaimp--parse-scope-unknown (state)
  "Catch-all parser which produces `unknown' scope."
  (make-javaimp-scope :type 'unknown
                      :name "unknown"
                      :start nil
                      :open-brace (nth 1 state)))

(defun javaimp--parse-scopes (count)
  "Attempts to parse COUNT enclosing scopes at point.  If COUNT is
nil then goes all the way up."
  (let ((state (syntax-ppss)) res)
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
  (goto-char (point-max))
  (when (javaimp--rsb-outside-context
         "^\\s-*package\\s-+\\([^;\n]+\\)\\s-*;" nil t 1)
    (match-string 1)))

(defun javaimp--parse-get-file-classes ()
  (goto-char (point-max))
  (let (res)
    (while (javaimp--rsb-outside-context
            (regexp-opt javaimp--parse-class-keywords 'words) nil t)
      (save-excursion
        (let ((parse-sexp-ignore-comments t) ; FIXME remove with major mode
              (parse-sexp-lookup-properties nil))
          (when (and (ignore-errors
                       (goto-char (scan-lists (point) 1 -1)))
                     (= (char-before) ?{))
            (let ((scopes (javaimp--parse-scopes nil)))
              (when (seq-every-p #'javaimp--parse-is-class scopes)
                (push (mapconcat #'javaimp-scope-name scopes ".") res)))))))
    res))

(provide 'javaimp-parse)
