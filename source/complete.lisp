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

(defun completion--escape (text)
  "Backslash escape characters that would split or expand TEXT when it
   is typed back into a command line."
  (with-output-to-string (escaped)
    (loop for char across text
          do (when (find char " 	\"'\\$*?")
               (write-char #\\ escaped))
             (write-char char escaped))))

(defun completion--common-prefix (candidates)
  "Longest common prefix of the CANDIDATES."
  (reduce (lambda (first second)
            (subseq first 0 (or (mismatch first second) (length first))))
          candidates))


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
  (let ((matches (sort (remove-duplicates
                        (remove-if-not (lambda (name)
                                         (string-prefix-p prefix name))
                                       (append (shell-command-names)
                                               (path-command-names)))
                        :test #'string=)
                       #'string<)))
    (values matches matches)))

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
               (let ((tail (if directory-p "/" "")))
                 (push (cons (concatenate 'string
                                          (completion--escape
                                           (concatenate 'string
                                                        directory-part name))
                                          tail)
                             (concatenate 'string name tail))
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

(defun complete-line (buffer cursor)
  "Compute completions at CURSOR. Returns (values start candidates
   displays); each candidate replaces BUFFER between START and CURSOR."
  (if (line-lisp-p buffer)
      (multiple-value-bind (start prefix)
          (completion--lisp-span buffer cursor)
        (let ((matches (completion--symbols prefix)))
          (values start matches matches)))
      (multiple-value-bind (start prefix command-position-p)
          (completion--command-span buffer cursor)
        (cond ((null start)
               (values cursor nil nil))
              ((and command-position-p (not (find #\/ prefix)))
               (completion--commands-with-start start prefix))
              (t
               (multiple-value-bind (candidates displays)
                   (completion--files prefix)
                 (values start candidates displays)))))))

(defun completion--commands-with-start (start prefix)
  "Command candidates wrapped with their replacement START."
  (multiple-value-bind (candidates displays)
      (completion--commands prefix)
    (values start candidates displays)))
