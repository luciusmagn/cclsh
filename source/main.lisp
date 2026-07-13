;;;; -- Main loop --
;;;
;;; Startup, the read loop with Lisp continuation lines, and the entry
;;; points for a REPL session and the saved application. The saved
;;; application is written to survive login shell duty: -c is handled
;;; for ssh, scp, rsync and git, unknown flags are ignored instead of
;;; refusing to start, a broken startup file or history never prevents
;;; a prompt, and CCLSH_SAFE=1 skips user state entirely.

(in-package #:cclsh)

(defparameter *cclsh-version* "1.0.0"
  "The cclsh version reported by --version.")

(defvar *cclsh-build-commit* nil
  "Git commit the running binary was built from, stamped into the
   image by scripts/build.lisp. NIL in plain REPL sessions.")

(defun terminal--encoding-leaves (&rest roots)
  "Return the distinct base streams below composite stream ROOTS."
  (let ((pending (copy-list roots))
        (seen nil)
        (leaves nil))
    (loop while pending
          for stream = (pop pending)
          unless (member stream seen :test #'eq)
            do (push stream seen)
               (cond ((typep stream 'synonym-stream)
                      (push (symbol-value (synonym-stream-symbol stream))
                            pending))
                     ((typep stream 'two-way-stream)
                      (push (two-way-stream-input-stream stream) pending)
                      (push (two-way-stream-output-stream stream) pending))
                     ((typep stream 'echo-stream)
                      (push (echo-stream-input-stream stream) pending)
                      (push (echo-stream-output-stream stream) pending))
                     ((typep stream 'broadcast-stream)
                      (setf pending
                            (append (broadcast-stream-streams stream)
                                    pending)))
                     ((typep stream 'concatenated-stream)
                      (setf pending
                            (append (concatenated-stream-streams stream)
                                    pending)))
                     (t
                      (push stream leaves))))
    (nreverse leaves)))

(defun terminal-encoding-setup ()
  "Make UTF-8 the default and switch every terminal stream to it."
  (setf ccl:*default-file-character-encoding* ':utf-8
        ccl:*default-external-format*
        '(:character-encoding :utf-8 :line-termination :unix)
        ccl:*terminal-character-encoding-name* ':utf-8)
  (let ((format (make-external-format :character-encoding ':utf-8
                                      :line-termination   ':unix)))
    (dolist (stream (terminal--encoding-leaves
                     *terminal-io* *standard-input*
                     *standard-output* *error-output*))
      (setf (stream-external-format stream) format)))
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
          (load file :verbose nil :external-format ':utf-8)
        (serious-condition (condition)
          (dispatch-report-error condition)))))
  (values))

(defun shell-safe-mode-p ()
  "True when CCLSH_SAFE is set, which skips startup.lisp and history."
  (let ((value (getenv "CCLSH_SAFE")))
    (and value (plusp (length value)) t)))


;;; Reading complete inputs

(defun shell-read-interactive (duration-milliseconds)
  "Read one complete input with the line editor, following unfinished
   Lisp forms onto continuation lines. Returns (values line kind)."
  (multiple-value-bind (rows columns)
      (terminal-size)
    (declare (ignore rows))
    (terminal-fresh-line)
    (let ((prompt (prompt-render *last-status* duration-milliseconds columns)))
      (multiple-value-bind (line kind)
          (edit-line prompt)
        (if (not (eq kind ':line))
            (values line kind)
            (let ((accumulated line))
              (loop while (input-line-open-p accumulated)
                    do (multiple-value-bind (continuation continuation-kind)
                           (edit-line (ansi-colorize "... " ':bright-black))
                         (unless (eq continuation-kind ':line)
                           (return-from shell-read-interactive
                             (values nil ':abort)))
                         (setf accumulated
                               (input-line-join accumulated continuation))))
              (values accumulated ':line)))))))

(defun shell-read-plain ()
  "Read one complete input without the editor, for piped input.
   Returns (values line kind)."
  (let ((line (read-line *standard-input* nil nil)))
    (if (null line)
        (values nil ':eof)
        (let ((accumulated line))
          (loop while (input-line-open-p accumulated)
                do (let ((continuation (read-line *standard-input* nil nil)))
                     (when (null continuation)
                       (return))
                     (setf accumulated
                           (input-line-join accumulated continuation))))
          (values accumulated ':line)))))


;;; Entry points

(defun main ()
  "Run the shell until exit or end of input. Iteration errors are
   reported and survived; only a long unbroken run of failures makes
   the shell give up, so a login session is never lost to one bad
   prompt render or a flaky read."
  (terminal-encoding-setup)
  (terminal-signals-setup)
  (terminal-shell-attributes-save)
  (environment-setup)
  (let ((safe (shell-safe-mode-p)))
    (unless safe
      (history-load))
    (let ((*package*   (find-package '#:cclsh-user))
          (interactive (terminal-tty-p))
          (duration    0)
          (failures    0))
      (unless safe
        (startup-load))
      (loop
        (catch 'cclsh-toplevel
          (handler-case
              (let ((*break-hook* (lambda (&rest arguments)
                                    (declare (ignore arguments))
                                    (throw 'cclsh-toplevel nil))))
                (multiple-value-bind (line kind)
                    (if interactive
                        (progn
                          (jobs-notify)
                          (shell-read-interactive duration))
                        (shell-read-plain))
                  (ecase kind
                    (:eof
                     (let ((*jobs-exit-confirmed*
                             (shiftf *jobs-exit-warned* nil)))
                       (if (jobs-exit-blocked-p)
                           (setf *last-status* 1)
                           (shell-quit *last-status*))))
                    (:abort
                     (setf *last-status* 130))
                    (:line
                     (when interactive
                       (history-append line))
                     (let ((started (get-internal-real-time)))
                       (dispatch-line line)
                       (setf duration
                             (round (* 1000 (- (get-internal-real-time)
                                               started))
                                    internal-time-units-per-second))))))
                (setf failures 0))
            (serious-condition (condition)
              (terminal-restore)
              (dispatch-report-error condition)
              (incf failures)
              (when (>= failures 25)
                (format *error-output*
                        "cclsh: too many consecutive errors, giving up~%")
                (shell-quit 70)))))))))


;;; Command line arguments

(defun shell--execute-command-string (command)
  "Run COMMAND as one shell input and exit with its status. This is
   the -c mode used by ssh, scp, rsync, git and $SHELL callers. It
   skips startup.lisp and history so user state can never break remote
   access."
  (terminal-encoding-setup)
  (terminal-signals-setup)
  (terminal-shell-attributes-save)
  (environment-setup)
  (let ((*package* (find-package '#:cclsh-user)))
    (dispatch-line command))
  (shell-quit *last-status*))

(defun shell--run-script (path)
  "Run the file at PATH as a cclsh script and exit with the last
   status. Like -c, scripts load no user state; this is also the
   shebang entry point since the kernel passes the script path as the
   first argument."
  (terminal-encoding-setup)
  (terminal-signals-setup)
  (terminal-shell-attributes-save)
  (environment-setup)
  (let ((*package* (find-package '#:cclsh-user)))
    (handler-case
        (with-open-file (stream path
                                :direction :input
                                :external-format ':utf-8)
          (let ((*standard-input* stream))
            (loop
              (multiple-value-bind (line kind)
                  (shell-read-plain)
                (when (eq kind ':eof)
                  (return))
                (dispatch-line line)))))
      (error (condition)
        (dispatch-report-error condition)
        (shell-quit 127))))
  (shell-quit *last-status*))

(defun shell--process-arguments (arguments)
  "Handle command line ARGUMENTS. Returns only when the shell should
   start its normal read loop; -c, --version, --help and script files
   exit the process themselves. Unknown flags are deliberately ignored
   so an exotic login invocation can never lock anyone out."
  (loop with remaining = arguments
        while remaining
        do (let ((argument (pop remaining)))
             (cond ((string= argument "-c")
                    (if remaining
                        (shell--execute-command-string (pop remaining))
                        (progn
                          (format *error-output*
                                  "cclsh: -c requires an argument~%")
                          (shell-quit 2))))
                   ((string= argument "--version")
                    (format t "cclsh ~a~@[ (~a)~] (~a ~a)~%"
                            *cclsh-version*
                            *cclsh-build-commit*
                            (lisp-implementation-type)
                            (lisp-implementation-version))
                    (shell-quit 0))
                   ((string= argument "--help")
                    (format t "usage: cclsh [-c command] [script] ~
                               [--version] [--help]~%")
                    (shell-quit 0))
                   ((string= argument "--")
                    nil)
                   ((and (plusp (length argument))
                         (char= (char argument 0) #\-))
                    nil)
                   (t
                    (shell--run-script argument)))))
  (values))

(defun shell--executable-path ()
  "Resolved path of the running executable, or NIL."
  (ignore-errors
    (namestring (truename "/proc/self/exe"))))

(defun shell-toplevel ()
  "Entry point for the saved cclsh application. Sets SHELL to the
   running binary so $SHELL callers land back in cclsh; a REPL session
   through MAIN leaves SHELL alone."
  (handler-case
      (progn
        (let ((executable (shell--executable-path)))
          (when executable
            (setenv "SHELL" executable)))
        (shell--process-arguments (rest *command-line-argument-list*))
        (main))
    (serious-condition (condition)
      (dispatch-report-error condition)
      (shell-quit 70))))
