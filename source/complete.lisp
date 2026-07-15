;;;; -- Completion --
;;;
;;; Tab completion for command names, file paths and Lisp symbols.
;;; COMPLETE-LINE inspects the buffer around the cursor and returns
;;; full replacement candidates; the line editor owns presentation.

(in-package #:cclsh)

(defun string-prefix-p (prefix string)
  "True when STRING starts with PREFIX."
  (and (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun path-command-names ()
  "All executable names found in PATH, sorted and deduplicated.
   Cached until PATH changes or REHASH clears the cache."
  (let ((path (or (getenv "PATH") "")))
    (unless (and *path-command-names*
                 (equal path *path-command-names-source*))
      (let ((names nil))
        (dolist (directory (path-directories))
          (multiple-value-bind (files subdirectories)
              (directory-entry-names directory)
            (declare (ignore subdirectories))
            (dolist (file files)
              (push file names))))
        (setf *path-command-names*
              (sort (remove-duplicates names :test #'string=) #'string<))
        (setf *path-command-names-source* path))))
  *path-command-names*)

(defun shell-command-names ()
  "Names of the COMMAND instances currently visible, downcased."
  (let ((names nil))
    (do-symbols (symbol *package*)
      (when (and (boundp symbol)
                 (typep (symbol-value symbol) 'command))
        (pushnew (string-downcase (symbol-name symbol)) names
                 :test #'string=)))
    names))

(defparameter *completion-escaped-characters*
  (list #\space #\" #\' #\\ #\$ #\* #\? #\( #\) #\&)
  "Characters that would split or expand a completed word: whitespace,
   quotes, backslashes, variable references, glob wildcards, Lisp
   substitution parens and the background marker.")

(defun completion--terminal-control-p (char)
  "True when CHAR is a C0, DEL or C1 terminal control character."
  (let ((code (char-code char)))
    (or (< code 32)
        (= code 127)
        (<= 128 code 159))))

(defun completion--write-controlled-text (text stream)
  "Write a command expression for TEXT to STREAM without emitting any
   control characters. The expression expands back to the exact string."
  (format stream "$((map 'string #'code-char '~s))"
          (map 'list #'char-code text)))

(defun completion--escape (text &key (escape-leading-tilde t))
  "Backslash escape the special characters in TEXT so it survives
   being typed back into a command line. Terminal controls are written
   as Lisp substitutions so neither the edit buffer nor its rendering
   contains the raw control character. ESCAPE-LEADING-TILDE protects a
   literal command or filename; file paths under ~/ leave it active."
  (with-output-to-string (escaped)
    (cond ((and (not escape-leading-tilde)
                (string-prefix-p "~/" text)
                (find-if #'completion--terminal-control-p text))
           (write-string "~/" escaped)
           (completion--write-controlled-text (subseq text 2) escaped))
          ((find-if #'completion--terminal-control-p text)
           (completion--write-controlled-text text escaped))
          (t
           (loop for char across text
                 for index from 0
                 do (when (or (member char *completion-escaped-characters*)
                              (and escape-leading-tilde
                                   (zerop index)
                                   (char= char #\~)))
                      (write-char #\\ escaped))
                    (write-char char escaped))))))

(defun completion--display (text)
  "Render TEXT without terminal controls, retaining a recognizable name."
  (with-output-to-string (display)
    (loop for char across text
          for code = (char-code char)
          do (cond ((< code 32)
                    (write-char #\^ display)
                    (write-char (code-char (+ code 64)) display))
                   ((= code 127)
                    (write-string "^?" display))
                   ((<= 128 code 159)
                    (format display "\\u~4,'0x" code))
                   (t
                    (write-char char display))))))

(defun completion--common-prefix (candidates)
  "Longest safely insertable common prefix of the CANDIDATES."
  (let* ((prefix
           (reduce (lambda (first second)
                     (subseq first 0
                             (or (mismatch first second) (length first))))
                   candidates))
         (control-expression
           (search "$((map 'string #'code-char '" prefix)))
    ;; A partial generated expression cannot be executed, and a lone
    ;; backslash would escape whatever the user types next.
    (when control-expression
      (setf prefix (subseq prefix 0 control-expression)))
    (when (oddp (loop for index downfrom (1- (length prefix)) to 0
                      while (char= (char prefix index) #\\)
                      count 1))
      (setf prefix (subseq prefix 0 (1- (length prefix)))))
    prefix))


;;; Span detection

(defun completion--command-span (buffer cursor)
  "Locate the command mode word being completed at CURSOR.
   Returns (values start prefix command-position-p); START is NIL when
   the cursor sits inside a quoted token, which is not completed."
  (let* ((tokens  (lex-command-line buffer))
         (groups  (token-groups tokens))
         (current nil))
    (dolist (token tokens)
      (when (and (not (eq (token-type token) ':space))
                 (< (token-start token) cursor)
                 (<= cursor (token-end token)))
        (setf current token)))
    (cond ((and current (not (eq (token-type current) ':word)))
           (values nil nil nil))
          (current
           (values (token-start current)
                   (subseq buffer (token-start current) cursor)
                   (eq current (first (first groups)))))
          (t
           (values cursor ""
                   (or (null groups)
                       (>= (token-start (first (first groups))) cursor)))))))

(defun completion--lisp-span (buffer cursor)
  "Locate the Lisp atom being completed at CURSOR.
   Returns (values start prefix)."
  (let ((start cursor))
    (loop while (and (plusp start)
                     (not (lisp-symbol-boundary-p (char buffer (1- start)))))
          do (decf start))
    (values start (subseq buffer start cursor))))


;;; Candidate generation

(defun completion--commands (prefix)
  "Command name candidates matching PREFIX.
   Returns (values candidates displays)."
  (let* ((clean   (escape-remove prefix))
         (matches (sort (remove-duplicates
                         (remove-if-not (lambda (name)
                                          (string-prefix-p clean name))
                                        (append (shell-command-names)
                                                (path-command-names)))
                         :test #'string=)
                        #'string<)))
    (values (mapcar #'completion--escape matches)
            (mapcar #'completion--display matches))))

(defun completion--files (prefix)
  "File path candidates matching PREFIX. Directories complete with a
   trailing slash. Returns (values candidates displays)."
  (let* ((clean          (escape-remove prefix))
         (slash          (position #\/ clean :from-end t))
         (directory-part (if slash (subseq clean 0 (1+ slash)) ""))
         (base           (if slash (subseq clean (1+ slash)) clean))
         (list-root      (tilde-expand (if (string= directory-part "")
                                           "."
                                           directory-part)))
         (pairs          nil))
    (flet ((consider (name directory-p)
             (when (and (string-prefix-p base name)
                        (or (not (string-prefix-p "." name))
                            (string-prefix-p "." base)))
               (let* ((tail    (if directory-p "/" ""))
                      (escaped (completion--escape
                                (concatenate 'string directory-part name)
                                :escape-leading-tilde
                                (string= directory-part ""))))
                 (push (cons (concatenate 'string escaped tail)
                             (concatenate 'string
                                          (completion--display name)
                                          tail))
                       pairs)))))
      (multiple-value-bind (files subdirectories)
          (directory-entry-names list-root)
        (dolist (file files)
          (consider file nil))
        (dolist (subdirectory subdirectories)
          (consider subdirectory t))))
    (let ((sorted (sort pairs #'string< :key #'cdr)))
      (values (mapcar #'first sorted)
              (mapcar #'rest sorted)))))

(defun completion--command-heads (prefix)
  "Command and directory candidates matching command-position PREFIX.

Directories retain the trailing slash that makes them explicit implicit-cd
paths. Ordinary non-executable files are excluded."
  (multiple-value-bind (commands command-displays)
      (completion--commands prefix)
    (multiple-value-bind (paths path-displays)
        (completion--files prefix)
      (let* ((command-pairs (mapcar #'cons commands command-displays))
             (directory-pairs
               (loop for path in paths
                     for display in path-displays
                     when (and (plusp (length path))
                               (char= (char path (1- (length path))) #\/))
                       collect (cons path display)))
             (pairs
               (sort (remove-duplicates
                      (append command-pairs directory-pairs)
                      :test #'string=
                      :key #'car)
                     #'string<
                     :key #'cdr)))
        (values (mapcar #'car pairs)
                (mapcar #'cdr pairs))))))

(defun completion--symbols (prefix)
  "Lisp symbol candidates matching PREFIX, downcased. Keyword prefixes
   complete against the keyword package."
  (when (plusp (length prefix))
    (let* ((keyword-p (char= (char prefix 0) #\:))
           (lookup    (string-upcase (if keyword-p (subseq prefix 1) prefix)))
           (package   (if keyword-p (find-package '#:keyword) *package*))
           (matches   nil))
      (when (plusp (length lookup))
        (do-symbols (symbol package)
          (let ((name (symbol-name symbol)))
            (when (string-prefix-p lookup name)
              (pushnew (concatenate 'string
                                    (if keyword-p ":" "")
                                    (string-downcase name))
                       matches :test #'string=))))
        (sort matches #'string<)))))


;;; Entry point

(defun completion--token-at (tokens cursor)
  "The non-space token containing CURSOR, or NIL."
  (let ((found nil))
    (dolist (token tokens found)
      (when (and (not (eq (token-type token) ':space))
                 (< (token-start token) cursor)
                 (<= cursor (token-end token)))
        (setf found token)))))

(defun completion--lisp-matches (buffer cursor)
  "Symbol completion at CURSOR, shared by Lisp mode and :lisp
   substitutions inside command lines."
  (multiple-value-bind (start prefix)
      (completion--lisp-span buffer cursor)
    (let ((matches (completion--symbols prefix)))
      (values start matches matches))))

(defun complete-line (buffer cursor)
  "Compute completions at CURSOR. Returns (values start candidates
   displays); each candidate replaces BUFFER between START and CURSOR."
  (when (line-comment-p buffer)
    (return-from complete-line (values cursor nil nil)))
  (if (line-lisp-p buffer)
      (completion--lisp-matches buffer cursor)
      (let ((current (completion--token-at (lex-command-line buffer) cursor)))
        (if (and current (eq (token-type current) ':lisp))
            (completion--lisp-matches buffer cursor)
            (multiple-value-bind (start prefix command-position-p)
                (completion--command-span buffer cursor)
              (cond ((null start)
                     (values cursor nil nil))
                    ((and command-position-p (not (find #\/ prefix)))
                     (completion--commands-with-start start prefix))
                    (t
                     (multiple-value-bind (candidates displays)
                         (completion--files prefix)
                       (values start candidates displays)))))))))

(defun completion--commands-with-start (start prefix)
  "Command and directory candidates wrapped with replacement START."
  (multiple-value-bind (candidates displays)
      (completion--command-heads prefix)
    (values start candidates displays)))
