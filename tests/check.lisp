;;;; -- Regression checks --

(require :asdf)

(asdf:load-asd (truename "cclsh.asd"))
(asdf:load-system "cclsh")

(defvar *check-failures* nil
  "Descriptions of failed regression checks.")

(defun check-equal (name expected actual)
  "Record a failure under NAME unless EXPECTED and ACTUAL are EQUAL."
  (unless (equal expected actual)
    (push (format nil "~a: expected ~s, got ~s" name expected actual)
          *check-failures*)))

(defun check-history (&rest entries)
  "Return a history vector containing ENTRIES."
  (make-array (length entries)
              :adjustable       t
              :fill-pointer     (length entries)
              :initial-contents entries))


;;;; -- Multiline Lisp --

(let ((lisp-line (format nil "(progn~% ; ignored )~% 42)"))
      (command   (format nil "echo (progn ; ignored )~% 42)")))
  (check-equal "complete multiline Lisp comment"
               nil
               (cclsh::input-line-open-p lisp-line))
  (check-equal "complete multiline substitution comment"
               nil
               (cclsh::input-line-open-p command))
  (check-equal "multiline highlighting preserves text"
               lisp-line
               (cclsh::ansi-strip (cclsh::highlight-line lisp-line))))


;;;; -- History --

(let* ((multiline (format nil "echo one~%echo two"))
       (history   (check-history "git status" "echo longer" "echo")))
  (check-equal "newest longer prefix"
               "echo newest"
               (cclsh::history-suggestion
                "ec" (check-history "echo older" "echo newest")))
  (check-equal "skip equal newest entry"
               "echo longer"
               (cclsh::history-suggestion "echo" history))
  (check-equal "empty input has no suggestion"
               nil
               (cclsh::history-suggestion "" history))
  (check-equal "history matching is case-sensitive"
               nil
               (cclsh::history-suggestion "E" history))
  (check-equal "multiline suggestion remains exact"
               multiline
               (cclsh::history-suggestion
                "echo one" (check-history multiline)))
  (check-equal "multiline recall remains exact"
               multiline
               (cclsh::editor--history-entry
                (check-history multiline) 0))
  (check-equal "printed history string round-trips"
               multiline
               (read-from-string
                (with-output-to-string (stream)
                  (prin1 multiline stream)))))


;;;; -- Cursor movement and layout --

(check-equal "right moves within input"
             '("abc" 2)
             (multiple-value-list
              (cclsh::editor--move-right "abc" 1 "abcdef")))
(check-equal "right accepts at input end"
             '("abcdef" 6)
             (multiple-value-list
              (cclsh::editor--move-right "abc" 3 "abcdef")))
(check-equal "right without suggestion stays at end"
             '("abc" 3)
             (multiple-value-list
              (cclsh::editor--move-right "abc" 3 nil)))

(let ((cases `((""                    (0 2 nil))
               ("a"                   (0 3 nil))
               ("ab"                  (1 0 t))
               (,(format nil "ab~%")   (1 0 nil))
               (,(format nil "ab~%~%") (2 0 nil))
               (,(format nil "abc~%")  (2 0 nil)))))
  (dolist (case cases)
    (destructuring-bind (text expected) case
      (check-equal (format nil "screen position for ~s" text)
                   expected
                   (multiple-value-list
                    (cclsh::editor--screen-position text 2 4))))))

(let ((text (format nil "a~%b")))
  (check-equal "cursor before newline"
               '(0 1 nil)
               (multiple-value-list
                (cclsh::editor--screen-position text 0 4 :end 1)))
  (check-equal "cursor after newline"
               '(1 0 nil)
               (multiple-value-list
                (cclsh::editor--screen-position text 0 4 :end 2)))
  (check-equal "display writes carriage return after newline"
               (format nil "a~%~cb" #\return)
               (with-output-to-string (*standard-output*)
                 (cclsh::editor--write-display text))))


;;;; -- Result --

(cond (*check-failures*
       (format *error-output* "~d regression check~:p failed:~%"
               (length *check-failures*))
       (dolist (failure (nreverse *check-failures*))
         (format *error-output* "  ~a~%" failure))
       (ccl:quit 1))
      (t
       (format t "All cclsh regression checks passed.~%")
       (ccl:quit 0)))
