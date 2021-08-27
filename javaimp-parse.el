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

(require 'javaimp-util)

(defconst javaimp--parse-classlike-keywords
  (mapcar #'symbol-name
          javaimp--classlike-scope-types))

(defconst javaimp--parse-stmt-keywords
  '("if" "else" "for" "while" "do" "switch" "try" "catch" "finally"
    "static"                            ; static initializer block
    ))
(defconst javaimp--parse-stmt-keyword-maxlen
  (seq-max (mapcar #'length javaimp--parse-stmt-keywords)))

(defvar-local javaimp--parse-dirty-pos nil
  "Buffer position after which all parsed information should be
considered as stale.  Usually set by modification change hooks.
Should be set to (point-min) in major mode hook.")

(defsubst javaimp--parse-substr-before-< (str)
  (let ((end (string-search "<" str)))
    (if end
        (string-trim (substring str 0 end))
      str)))

(defun javaimp--parse-rsb-keyword (regexp &optional bound noerror count)
  "Like `re-search-backward', but count only occurences outside
syntactic context as given by `syntax-ppss-context'.  Assumes
point is outside of any context initially."
  (or count (setq count 1))
  (let ((step (if (>= count 0) 1 -1))
        (case-fold-search nil)
        res)
    (dotimes (_ (abs count))
      (while (and (setq res (re-search-backward regexp bound noerror step))
                  (syntax-ppss-context (syntax-ppss)))))
    res))

(defun javaimp--parse-arglist (beg end &optional only-type)
  "Parse arg list between BEG and END, of the form 'TYPE NAME,
...'.  Return list of conses (TYPE . NAME).  If ONLY-TYPE is
non-nil, then name parsing is skipped."
  (let ((substr (buffer-substring-no-properties beg end)))
    (with-temp-buffer
      (insert substr)
      (with-syntax-table javaimp--arglist-syntax-table
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
                (backward-char)))       ; skip comma
            res))))))

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
  (and (javaimp--parse-rsb-keyword regexp nil t)
       (ignore-errors
         ;; Does our match belong to the right block?
         (= (scan-lists (match-end 0) (or skip-count 1) -1)
            (1+ scope-start)))))

(defun javaimp--parse-decl-suffix (regexp state &optional bound)
  "Attempts to parse declaration suffix backwards from point (but
not farther than BOUND), returning non-nil on success.  More
precisely, the value is the end of the match for REGEXP.  Point
is left before the match.  Otherwise, the result is nil and point
is unchanged."
  (let ((pos (point)))
    (catch 'found
      (while (javaimp--parse-rsb-keyword regexp bound t)
        (let ((scan-pos (match-end 0)))
          (with-syntax-table javaimp--arglist-syntax-table
            ;; Skip over any number of lists, which may be exceptions
            ;; in "throws", or something like that
            (while (and scan-pos (<= scan-pos (nth 1 state)))
              (if (ignore-errors
                    (= (scan-lists scan-pos 1 -1) ;As in javaimp--parse-preceding
                       (1+ (nth 1 state))))
                  (progn
                    (goto-char (match-beginning 0))
                    (throw 'found (match-end 0)))
                (setq scan-pos (ignore-errors
                                 (scan-lists scan-pos 1 0))))))))
      ;; just return to start
      (goto-char pos)
      nil)))


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
    (if (javaimp--parse-preceding (regexp-opt javaimp--parse-classlike-keywords 'words)
                                  (nth 1 state))
        (let* ((keyword-start (match-beginning 1))
               (keyword-end (match-end 1))
               arglist)
          (goto-char (nth 1 state))
          (or (javaimp--parse-decl-suffix "\\<extends\\>" state keyword-end)
              (javaimp--parse-decl-suffix "\\<implements\\>" state keyword-end)
              (javaimp--parse-decl-suffix "\\<permits\\>" state keyword-end))
          ;; we either skipped back over the valid declaration
          ;; suffix(-es), or there wasn't any
          (setq arglist (javaimp--parse-arglist keyword-end (point) t))
          (when (= (length arglist) 1)
            (make-javaimp-scope :type (intern
                                       (buffer-substring-no-properties
                                        keyword-start keyword-end))
                                :name (javaimp--parse-substr-before-< (caar arglist))
                                :start keyword-start
                                :open-brace (nth 1 state)))))))

(defun javaimp--parse-scope-simple-stmt (state)
  "Attempts to parse 'simple-statement' scope."
  (save-excursion
    (and (javaimp--parse-skip-back-until)
         (or (and (= (char-before (1- (point))) ?-) ; ->
                  (= (char-before) ?>))
             (looking-back (regexp-opt javaimp--parse-stmt-keywords 'words)
                           (- (point) javaimp--parse-stmt-keyword-maxlen) nil))
         (make-javaimp-scope
          :type 'simple-statement
          :name (or (match-string 1)
                    "lambda")
          :start (or (match-beginning 1)
                     (- (point) 2))
          :open-brace (nth 1 state)))))

(defun javaimp--parse-scope-anonymous-class (state)
  "Attempts to parse 'anonymous-class' scope."
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
                                :name (javaimp--parse-substr-before-< (caar arglist))
                                :start start
                                :open-brace (nth 1 state))))))))

(defun javaimp--parse-scope-method-or-stmt (state)
  "Attempts to parse 'method' or 'statement' scope."
  (save-excursion
    (let (;; take the closest preceding closing paren as the bound
          (search-bound (save-excursion
                          (when (javaimp--parse-rsb-keyword ")" nil t 1)
                            (1+ (point))))))
      (when search-bound
        (let ((throws-args
               (let ((pos (javaimp--parse-decl-suffix
                           "\\<throws\\>" state search-bound)))
                 (when pos
                   (or (javaimp--parse-arglist pos (nth 1 state) t)
                       t)))))
          (when (and (not (eq throws-args t))
                     (progn
                       (javaimp--parse-skip-back-until)
                       (= (char-before) ?\)))
                     (ignore-errors
                       ;; for method this is arglist
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
                         (funcall javaimp-format-method-name
                                  name
                                  (javaimp--parse-arglist (car arglist-region)
                                                          (cdr arglist-region))
                                  throws-args))
                 :start (point)
                 :open-brace (nth 1 state))))))))))

(defun javaimp--parse-scope-array (state)
  "Attempts to parse 'array' scope."
  (save-excursion
    (and (javaimp--parse-skip-back-until)
         (member (char-before) '(?, ?\]))
         (make-javaimp-scope :type 'array
                             :name ""
                             :start nil
                             :open-brace (nth 1 state)))))

(defun javaimp--parse-scope-unknown (state)
  "Catch-all parser which produces 'unknown' scope."
  (make-javaimp-scope :type 'unknown
                      :name "unknown"
                      :start nil
                      :open-brace (nth 1 state)))

(defun javaimp--parse-scopes (count)
  "Attempts to parse COUNT enclosing scopes at point.  If COUNT is
nil then goes all the way up.  Examines and sets property
'javaimp-parse-scope' at each scope's open brace."
  (let ((state (syntax-ppss))
        res)
    (unless (syntax-ppss-context state)
      (while (and (nth 1 state)
                  (or (not count)
                      (>= (setq count (1- count)) 0)))
        ;; find innermost enclosing open-bracket
        (goto-char (nth 1 state))
        (when (= (char-after) ?{)
          (let ((scope (get-text-property (point) 'javaimp-parse-scope)))
            (unless scope
              (setq scope (run-hook-with-args-until-success
                           'javaimp--parse-scope-hook state))
              (put-text-property (point) (1+ (point))
                                 'javaimp-parse-scope scope))
            (push scope res)
            (if (javaimp-scope-start scope)
                (goto-char (javaimp-scope-start scope)))))
        (setq state (syntax-ppss))))
    ;; if a class is enclosed in anything other than a class, then it
    ;; should be local
    (let ((tmp res)
          in-local parent)
      (while tmp
        (if (javaimp--is-classlike (car tmp))
            (when in-local
              (setf (javaimp-scope-type (car tmp)) 'local-class))
          (setq in-local t))
        (setf (javaimp-scope-parent (car tmp)) parent)
        (setq parent (car tmp))
        (setq tmp (cdr tmp))))
    res))

(defun javaimp--parse-all-scopes ()
  "Entry point to the scope parsing.  Parses scopes in this
buffer which are after `javaimp--parse-dirty-pos', if it is
non-nil.  Resets this variable after parsing is done."
  ;; FIXME Set parse-sexp-.. vars, as well as syntax-table and
  ;; syntax-ppss-table, in major mode function.  Remove let-binding of
  ;; inhibit-modification-hooks here.
  (when javaimp--parse-dirty-pos
    (let ((inhibit-modification-hooks t))
      (remove-text-properties javaimp--parse-dirty-pos (point-max)
                              '(javaimp-parse-scope nil))
      (goto-char (point-max))
      (let ((parse-sexp-ignore-comments t)
            (parse-sexp-lookup-properties nil))
        (with-syntax-table javaimp-syntax-table
          (while (javaimp--parse-rsb-keyword "{" javaimp--parse-dirty-pos t)
            (save-excursion
              (forward-char)
              ;; Set props at this brace and all the way up
              (javaimp--parse-scopes nil)))))
      (setq javaimp--parse-dirty-pos nil))))



;; Functions intended to be called from other parts of javaimp.

(defun javaimp--parse-get-package ()
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (when (javaimp--parse-rsb-keyword
             "^[ \t]*package[ \t]+\\([^ \t;\n]+\\)[ \t]*;" nil t 1)
        (match-string 1)))))

(defun javaimp--parse-get-all-classlikes ()
  (mapcar (lambda (scope)
            (let ((name (javaimp-scope-name scope))
                  (parent-names (javaimp--concat-scope-parents scope)))
              (if (string-empty-p parent-names)
                  name
                (concat parent-names "." name))))
          (javaimp--parse-get-all-scopes #'javaimp--is-classlike)))

(defun javaimp--parse-get-imenu-forest ()
  (let* ((methods (javaimp--parse-get-all-scopes
                   #'javaimp--is-imenu-included-method #'javaimp--is-classlike))
         (classes (javaimp--parse-get-all-scopes #'javaimp--is-classlike))
         (top-classes (seq-filter (lambda (s)
                                    (null (javaimp-scope-parent s)))
                                  classes)))
    (mapcar
     (lambda (top-class)
       (message "Building tree for top-level class-like scope: %s"
                (javaimp-scope-name top-class))
       (javaimp--build-tree top-class (append methods classes)
                            (lambda (el tested)
                              (equal el (javaimp-scope-parent tested)))
                            nil
                            (lambda (s1 s2)
                              (< (javaimp-scope-start s1)
                                 (javaimp-scope-start s2)))))
     top-classes)))

(defun javaimp--parse-get-all-scopes (&optional pred parent-pred)
  "Return all scopes in the current buffer, optionally filtering
them with PRED, and their parents with PARENT-PRED.  Neither of
them should move point."
  (save-excursion
    (save-restriction
      (widen)
      (javaimp--parse-all-scopes)
      (let ((pos (point-max))
            scope res)
        (while (setq pos (previous-single-property-change pos 'javaimp-parse-scope))
          (setq scope (get-text-property pos 'javaimp-parse-scope))
          (when (and scope
                     (or (null pred)
                         (funcall pred scope)))
            (setq scope (javaimp--copy-scope scope))
            (when parent-pred
              (javaimp--filter-scope-parents scope parent-pred))
            (push scope res)))
        res))))

(defun javaimp--parse-update-dirty-pos (beg _end _old-len)
  "Function to add to `after-change-functions' hook."
  (when (or (not javaimp--parse-dirty-pos)
            (< beg javaimp--parse-dirty-pos))
    (setq javaimp--parse-dirty-pos beg)))

(provide 'javaimp-parse)
