;;;; -- Main loop --
;;;
;;; Startup, the read loop with Lisp continuation lines, and the entry
;;; points for a REPL session and the saved application.

(in-package #:cclsh)

(defun terminal-encoding-setup ()
  "Switch the terminal streams to UTF-8."
  (let ((format (make-external-format :character-encoding ':utf-8
                                      :line-termination   ':unix)))
    (dolist (stream (list *terminal-io* *standard-input*
                          *standard-output* *error-output*))
      (ignore-errors
        (setf (stream-external-format stream) format))))
  (values))

(defun environment-setup ()
  "Prepare environment variables for prompt rendering and children.
   STARSHIP_SHELL is blanked so starship emits plain ANSI without
   shell-specific escape wrappers."
  (setenv "STARSHIP_SHELL" "")
  (setenv "PWD" (directory-namestring-clean (current-directory)))
  (values))

(defun startup-file ()
  "Return the path of the user's startup file."
  (concatenate 'string (config-directory) "startup.lisp"))

(defun startup-load ()
  "Load the user's startup.lisp when present. Errors are reported and
   otherwise ignored so a broken startup file never bricks the shell."
  (let ((file (startup-file)))
    (when (probe-file file)
      (handler-case
          (load file :verbose nil)
        (serious-condition (condition)
          (dispatch-report-error condition)))))
  (values))


;;; Reading complete inputs

(defun shell-read-interactive (duration-milliseconds)
  "Read one complete input with the line editor, following unfinished
   Lisp forms onto continuation lines. Returns (values line kind)."
  (multiple-value-bind (rows columns)
      (terminal-size)
    (declare (ignore rows))
    (let ((prompt (prompt-render *last-status* duration-milliseconds columns)))
      (multiple-value-bind (line kind)
          (edit-line prompt)
        (if (not (eq kind ':line))
            (values line kind)
            (let ((accumulated line))
              (loop while (and (line-lisp-p accumulated)
                               (lisp-line-open-p accumulated))
                    do (multiple-value-bind (continuation continuation-kind)
                           (edit-line (ansi-colorize "... " ':bright-black))
                         (unless (eq continuation-kind ':line)
                           (return-from shell-read-interactive
                             (values nil ':abort)))
                         (setf accumulated
                               (concatenate 'string accumulated
                                            (string #\newline)
                                            continuation))))
              (values accumulated ':line)))))))

(defun shell-read-plain ()
  "Read one complete input without the editor, for piped input.
   Returns (values line kind)."
  (let ((line (read-line *standard-input* nil nil)))
    (if (null line)
        (values nil ':eof)
        (let ((accumulated line))
          (loop while (and (line-lisp-p accumulated)
                           (lisp-line-open-p accumulated))
                do (let ((continuation (read-line *standard-input* nil nil)))
                     (when (null continuation)
                       (return))
                     (setf accumulated
                           (concatenate 'string accumulated
                                        (string #\newline)
                                        continuation))))
          (values accumulated ':line)))))


;;; Entry points

(defun main ()
  "Run the shell until exit or end of input."
  (terminal-encoding-setup)
  (terminal-signals-setup)
  (environment-setup)
  (history-load)
  (let ((*package*   (find-package '#:cclsh-user))
        (interactive (terminal-tty-p))
        (duration    0))
    (startup-load)
    (loop
      (catch 'cclsh-toplevel
        (let ((*break-hook* (lambda (&rest arguments)
                              (declare (ignore arguments))
                              (throw 'cclsh-toplevel nil))))
          (multiple-value-bind (line kind)
              (if interactive
                  (shell-read-interactive duration)
                  (shell-read-plain))
            (ecase kind
              (:eof
               (quit *last-status*))
              (:abort
               (setf *last-status* 130))
              (:line
               (when interactive
                 (history-append line))
               (let ((started (get-internal-real-time)))
                 (dispatch-line line)
                 (setf duration
                       (round (* 1000 (- (get-internal-real-time) started))
                              internal-time-units-per-second)))))))))))

(defun shell-toplevel ()
  "Entry point for the saved cclsh application."
  (handler-case
      (main)
    (serious-condition (condition)
      (dispatch-report-error condition)
      (quit 70))))
