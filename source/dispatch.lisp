;;;; -- Dispatch --
;;;
;;; Turns one complete input line into action: Lisp evaluation for
;;; lines starting with an open paren, command execution otherwise.
;;; Errors never reach the CCL debugger; they print in red and the
;;; shell keeps going.

(in-package #:cclsh)

(defun dispatch-report-error (condition)
  "Print CONDITION in red on standard error."
  (format *error-output* "~a~%"
          (ansi-colorize (format nil "cclsh: ~a" condition) ':red))
  (force-output *error-output*)
  (values))

(defun dispatch--undefined-function-hint (form)
  "When FORM is a call whose head symbol names no function, macro or
   special operator, report it with close completions and return true.
   The form is then not evaluated: argument evaluation would surface a
   less useful error first, and would run side effects for a call that
   cannot succeed."
  (let ((head (and (consp form) (first form))))
    (when (and head
               (symbolp head)
               (not (special-operator-p head))
               (not (macro-function head))
               (not (fboundp head)))
      (let ((candidates
              (remove-if-not
               (lambda (name)
                 (multiple-value-bind (symbol found)
                     (find-symbol (string-upcase name) *package*)
                   (and found (fboundp symbol))))
               (completion--symbols (string-downcase (symbol-name head))))))
        (format *error-output* "~a~%"
                (ansi-colorize
                 (format nil "cclsh: undefined function ~(~a~)~
                              ~@[, did you mean ~{~a~^, ~}?~]"
                         head
                         (and candidates
                              (subseq candidates
                                      0 (min 3 (length candidates)))))
                 ':red))
        (force-output *error-output*))
      t)))

(defun dispatch-lisp (line)
  "Evaluate LINE as Lisp forms, print the values REPL style and update
   the * ** *** variables. Returns an exit status."
  (handler-case
      (let ((position 0)
            (eof      (list nil)))
        (loop
          (multiple-value-bind (form next)
              (read-from-string line nil eof :start position)
            (when (eq form eof)
              (return 0))
            (setf position next)
            (when (dispatch--undefined-function-hint form)
              (return 1))
            (let ((values (multiple-value-list (eval form))))
              (setf *** **
                    **  *
                    *   (first values))
              (terminal-fresh-line)
              (dolist (value values)
                (format t "~s~%" value))))))
    (serious-condition (condition)
      (dispatch-report-error condition)
      1)))

(defun dispatch--lone-value-status (line)
  "REPL fallback once command resolution has failed: a lone word that
   names a bound variable or keyword, or is a number literal,
   evaluates and prints as Lisp. Works on the raw LINE so glob
   characters in names like *earmuffed* variables cannot mangle it.
   Returns the exit status, or NIL when the fallback does not apply."
  (let ((trimmed (string-trim *whitespace-characters* line)))
    (when (and (plusp (length trimmed))
               (notany #'whitespace-char-p trimmed)
               (word-evaluates-alone-p trimmed))
      (dispatch-lisp trimmed))))

(defun dispatch-command (line)
  "Execute LINE as a shell command line. Returns an exit status."
  (handler-case
      (let ((words (command-line-words line)))
        (if (null words)
            *last-status*
            (multiple-value-bind (kind target)
                (command-resolve-fresh (first words))
              (ecase kind
                (:builtin  (command-execute-builtin target (rest words)))
                (:external (command-execute-external target (rest words)))
                (:unknown  (or (dispatch--lone-value-status line)
                               (error 'command-not-found-error
                                      :name (first words))))))))
    (shell-error (condition)
      (dispatch-report-error condition)
      127)
    (serious-condition (condition)
      (dispatch-report-error condition)
      1)))

(defun dispatch-line (line)
  "Execute LINE and return its exit status, recording *LAST-STATUS*."
  (let ((trimmed (string-trim *whitespace-characters* line)))
    (setf *last-status*
          (cond ((zerop (length trimmed))
                 *last-status*)
                ((and (>= (length trimmed) 2)
                      (string= "#!" trimmed :end2 2))
                 *last-status*)
                ((line-lisp-p trimmed)
                 (dispatch-lisp line))
                (t
                 (dispatch-command line))))))
