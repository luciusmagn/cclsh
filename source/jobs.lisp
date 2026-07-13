;;;; -- External execution and jobs --
;;;
;;; Running external programs and controlling them as jobs. A job is
;;; one foreground or background command or pipeline; every process
;;; CCL spawns lives in its own process group, so a multi-stage job
;;; owns several groups and stop and continue signals are carried to
;;; all of them. CCL's process monitor reaps exit statuses but never
;;; notices SIGCONT, so the live distinction between a running and a
;;; stopped process comes from /proc; CCL's status is authoritative
;;; once a child is reaped.

(in-package #:cclsh)

;;; The job table

(defstruct (job (:constructor job-make (&key processes command)))
  "One shell job. ID is the table number, NIL until the job is
   registered. PROCESSES are the CCL external processes in pipeline
   order. COMMAND is the display text. STATUS is the shell's view,
   :RUNNING, :STOPPED or :DONE, and REPORTED the status last shown to
   the user. TOUCHED orders the current and previous marks. ATTRIBUTES
   is the termios snapshot taken when the job stopped. PROPAGATED is
   true while a stop is being carried across the process groups.
   CLEANUPS are closures run once when the job completes."
  (id         nil)
  (processes  nil)
  (command    "")
  (status     ':running)
  (reported   ':running)
  (touched    0)
  (attributes nil)
  (propagated nil)
  (cleanups   nil))

(defvar *jobs* nil
  "Registered jobs, most recently registered first.")

(defvar *jobs-touch-counter* 0
  "Monotonic recency source for the current and previous job marks.")

(defvar *job-command-label* nil
  "Display text for the job a command execution is about to start,
   bound around dispatch so stopped jobs list the line as typed.")

(defun job-touch (job)
  "Stamp JOB as the most recently handled job."
  (setf (job-touched job) (incf *jobs-touch-counter*))
  (values))

(defun job-register (job)
  "Enter JOB into the job table under the next free job id."
  (setf (job-id job)
        (1+ (reduce #'max *jobs* :key #'job-id :initial-value 0)))
  (job-touch job)
  (push job *jobs*)
  job)

(defun job-unregister (job)
  "Remove JOB from the job table."
  (setf *jobs* (delete job *jobs*))
  (values))

(defun jobs--ordered ()
  "Jobs ordered as the current and previous marks see them: stopped
   jobs before the rest, most recently touched first."
  (sort (copy-list *jobs*)
        (lambda (first-job second-job)
          (let ((first-stopped  (eq (job-status first-job) ':stopped))
                (second-stopped (eq (job-status second-job) ':stopped)))
            (if (eq first-stopped second-stopped)
                (> (job-touched first-job) (job-touched second-job))
                first-stopped)))))

(defun job-current ()
  "The current job, the one % and fg and bg without arguments mean."
  (first (jobs--ordered)))

(defun job-mark (job)
  "The + mark of the current job, the - mark of the previous one, or
   a space."
  (let ((ordered (jobs--ordered)))
    (cond ((eq job (first ordered))
           "+")
          ((eq job (second ordered))
           "-")
          (t
           " "))))


;;; Job status

(defun process-run-state (pid)
  "Live state of PID read from /proc: :RUNNING, :STOPPED or :GONE.
   Zombies count as gone because only their reaped status matters.
   The state letter follows the last close paren of the stat line, so
   process names containing parens cannot confuse the parse."
  (let ((line (with-open-file (stat (format nil "/proc/~d/stat" pid)
                                    :direction         ':input
                                    :if-does-not-exist nil)
                (when stat
                  (ignore-errors (read-line stat nil nil))))))
    (if (null line)
        ':gone
        (let* ((close (position #\) line :from-end t))
               (state (and close
                           (< (+ close 2) (length line))
                           (char line (+ close 2)))))
          (case state
            ((#\T #\t)      ':stopped)
            ((#\Z #\X #\x)  ':gone)
            ((nil)          ':gone)
            (t              ':running))))))

(defun process-live-state (process)
  "State of one job PROCESS: :DONE, :STOPPED or :RUNNING. An exited
   process not yet reaped by CCL counts as running so callers keep
   polling until its exit status is real."
  (multiple-value-bind (status code)
      (external-process-status process)
    (declare (ignore code))
    (case status
      ((:exited :signaled)
       ':done)
      (t
       (case (process-run-state (external-process-id process))
         (:stopped
          ':stopped)
         (t
          ':running))))))

(defun job--finish (job)
  "Run the cleanup closures attached to JOB exactly once."
  (dolist (cleanup (shiftf (job-cleanups job) nil))
    (ignore-errors (funcall cleanup)))
  (values))

(defun job-refresh (job)
  "Recompute and return the status of JOB. A job whose processes are
   all reaped is done, one whose live processes all stopped is
   stopped, and anything still running keeps the job running. A stop
   seen while other stages still run is carried to every process
   group once, standing in for the single process group a Unix shell
   would give a pipeline."
  (unless (eq (job-status job) ':done)
    (let ((states (mapcar #'process-live-state (job-processes job))))
      (cond ((every (lambda (state) (eq state ':done)) states)
             (setf (job-status job) ':done)
             (job--finish job))
            ((notany (lambda (state) (eq state ':stopped)) states)
             (setf (job-propagated job) nil)
             (setf (job-status job) ':running))
            ((notany (lambda (state) (eq state ':running)) states)
             (setf (job-status job) ':stopped))
            (t
             (unless (job-propagated job)
               (setf (job-propagated job) t)
               (loop for process in (job-processes job)
                     for state in states
                     when (eq state ':running)
                       do (process-group-stop
                           (external-process-id process))))
             (setf (job-status job) ':running)))))
  (job-status job))

(defun job-exit-status (job)
  "Shell exit status of JOB, the status of its last process."
  (external-process-exit-status (first (last (job-processes job)))))


;;; Display

(defparameter *job-signal-names*
  '((1  . "Hangup")
    (2  . "Interrupt")
    (3  . "Quit")
    (6  . "Aborted")
    (9  . "Killed")
    (11 . "Segmentation fault")
    (13 . "Broken pipe")
    (15 . "Terminated"))
  "Report strings for the signals that commonly end jobs.")

(defun job--status-text (job)
  "The status column of JOB in the job table."
  (ecase (job-status job)
    (:running
     "Running")
    (:stopped
     "Stopped")
    (:done
     (multiple-value-bind (status code)
         (external-process-status (first (last (job-processes job))))
       (cond ((eq status ':signaled)
              (or (rest (assoc code *job-signal-names*))
                  (format nil "Signal ~d" code)))
             ((and (eq status ':exited) (plusp (or code 0)))
              (format nil "Exit ~d" code))
             (t
              "Done"))))))

(defun job-print (job stream &key show-pid)
  "Print the job table line of JOB to STREAM: id, mark, status and
   command, preceded by the head process group id under SHOW-PID.
   Running jobs display with the trailing & they run under."
  (format stream "[~d]~a  ~@[~d ~]~24a~a~@[ &~]~%"
          (job-id job)
          (job-mark job)
          (and show-pid (external-process-id (first (job-processes job))))
          (job--status-text job)
          (job-command job)
          (eq (job-status job) ':running))
  (values))

(defun jobs-notify ()
  "Report jobs whose status changed since the last report and drop
   finished jobs from the table. Runs before each interactive prompt."
  (dolist (job (sort (copy-list *jobs*) #'< :key #'job-id))
    (let ((status (job-refresh job)))
      (unless (eq status (job-reported job))
        (setf (job-reported job) status)
        (job-print job *error-output*))
      (when (eq status ':done)
        (job-unregister job))))
  (force-output *error-output*)
  (values))


;;; Foreground execution

(defun job--hand-terminal (process-group)
  "Make PROCESS-GROUP the foreground group, retrying briefly: a fresh
   child only becomes a group leader once it reaches its own setpgid
   between fork and exec."
  (loop repeat 50
        until (terminal-foreground process-group)
        do (sleep 0.001))
  (values))

(defun job--continue-processes (job)
  "Send SIGCONT to every live process group of JOB, then wait briefly
   until the stops have lifted so the caller's wait loop cannot
   mistake the old stop for a fresh Ctrl-Z."
  (dolist (process (job-processes job))
    (unless (eq (process-live-state process) ':done)
      (process-group-continue (external-process-id process))))
  (loop repeat 100
        while (find ':stopped (job-processes job) :key #'process-live-state)
        do (sleep 0.001))
  (setf (job-propagated job) nil)
  (values))

(defun job--suspend (job)
  "Book a stopped foreground JOB back into the shell: remember the
   terminal attributes the job was using, restore the shell's own,
   register a fresh job and announce the stop. Returns the shell
   status of a stopped job."
  (setf (job-attributes job) (terminal-attributes))
  (terminal-attributes-apply *terminal-shell-attributes*)
  (unless (job-id job)
    (job-register job))
  (job-touch job)
  (setf (job-status job) ':stopped)
  (setf (job-reported job) ':stopped)
  (terminal-fresh-line)
  (job-print job *error-output*)
  (force-output *error-output*)
  (+ 128 +sigtstp+))

(defun job--wait (job interactive)
  "Poll JOB until it completes, or until it stops when INTERACTIVE.
   Returns the exit status, or :STOPPED for a stop."
  (loop
    (case (job-refresh job)
      (:done
       (return (job-exit-status job)))
      (:stopped
       (when interactive
         (return ':stopped))))
    (sleep 0.005)))

(defun job-run-foreground (job &key continue)
  "Run JOB as the terminal's foreground job until it finishes or, on
   an interactive session, stops. With CONTINUE the job resumes from
   a stop: its terminal attributes are reapplied and every process
   group receives SIGCONT. Returns the exit status, or 148 when the
   job stopped and returned to the table."
  (let ((interactive (terminal-tty-p))
        (shell-group (terminal-own-process-group))
        (head-group  (external-process-id (first (job-processes job))))
        (outcome     nil))
    (unwind-protect
        (progn
          (when (and interactive head-group)
            (when continue
              (terminal-attributes-apply (job-attributes job)))
            (job--hand-terminal head-group)
            (if continue
                (job--continue-processes job)
                (process-group-continue head-group)))
          (setf outcome (job--wait job interactive)))
      (when interactive
        (terminal-foreground shell-group)))
    (cond ((eq outcome ':stopped)
           (job--suspend job))
          (t
           (when (job-id job)
             (job-unregister job))
           outcome))))


;;; Spawning

(defun external-process-exit-status (process)
  "Translate the status of PROCESS into a shell exit code."
  (multiple-value-bind (status code)
      (external-process-status process)
    (case status
      (:exited   (or code 0))
      (:signaled (+ 128 (or code 0)))
      (t         1))))

(defun external-wait (process)
  "Wait for PROCESS to terminate and return its shell exit code."
  (loop
    (multiple-value-bind (status code)
        (external-process-status process)
      (declare (ignore code))
      (unless (member status '(:running :stopped))
        (return (external-process-exit-status process))))
    (sleep 0.005)))

(defun job--label (path arguments)
  "Display text for a job: the command line as typed when dispatch
   provided one, the program path and arguments otherwise."
  (or *job-command-label*
      (format nil "~{~a~^ ~}" (cons path arguments))))

(defun command-execute-external (path arguments)
  "Run the program at PATH with ARGUMENTS sharing the terminal as the
   foreground job. Returns the exit status, 148 after a Ctrl-Z stop."
  (let ((process (with-child-signal-defaults
                   (run-program path arguments
                                :input  t
                                :output t
                                :error  t
                                :wait   nil))))
    (job-run-foreground
     (job-make :processes (list process)
               :command   (job--label path arguments)))))

(defun command-execute-background (path arguments)
  "Start the program at PATH with ARGUMENTS as a background job and
   announce its job id and process group. Returns zero, the status of
   a successful launch."
  (let* ((process (with-child-signal-defaults
                    (run-program path arguments
                                 :input  t
                                 :output t
                                 :error  t
                                 :wait   nil)))
         (job     (job-register
                   (job-make :processes (list process)
                             :command   (job--label path arguments)))))
    (when (terminal-tty-p)
      (format *error-output* "[~d] ~d~%"
              (job-id job) (external-process-id process))
      (force-output *error-output*))
    0))

;;; Job builtins

(defun jobs-count ()
  "Number of jobs in the job table, reported to the prompt."
  (length *jobs*))

(defun job-find (spec)
  "Find a job by SPEC: NIL, % and %+ mean the current job, %- the
   previous one, an integer (with optional %) a job id, and any other
   %text the most current job whose command starts with text. Returns
   NIL when nothing matches."
  (let ((text (and spec (princ-to-string spec))))
    (cond ((or (null text)
               (string= text "%")
               (string= text "%+")
               (string= text "%%"))
           (job-current))
          ((string= text "%-")
           (second (jobs--ordered)))
          (t
           (let ((bare (if (and (plusp (length text))
                                (char= (char text 0) #\%))
                           (subseq text 1)
                           text)))
             (multiple-value-bind (id end)
                 (parse-integer bare :junk-allowed t)
               (if (and id (= end (length bare)))
                   (find id *jobs* :key #'job-id)
                   (find-if (lambda (job)
                              (let ((command (job-command job)))
                                (and (<= (length bare) (length command))
                                     (string= bare command
                                              :end2 (length bare)))))
                            (jobs--ordered)))))))))

(defun job--complain (builtin spec)
  "Report a job lookup failure for BUILTIN and return its exit status."
  (format *error-output* "~a~%"
          (ansi-colorize (if spec
                             (format nil "~a: no such job: ~a" builtin spec)
                             (format nil "~a: no current job" builtin))
                         ':red))
  (force-output *error-output*)
  1)

(defcommand jobs (&rest arguments)
  "List active jobs: id, the current + and previous - marks, status
   and command. jobs -l adds each job's process group id."
  (let ((show-pid (equal arguments '("-l"))))
    (cond ((and arguments (not show-pid))
           (format *error-output* "~a~%"
                   (ansi-colorize "jobs: the only supported option is -l"
                                  ':red))
           1)
          (t
           (dolist (job (sort (copy-list *jobs*) #'< :key #'job-id))
             (let ((status (job-refresh job)))
               (job-print job *standard-output* :show-pid show-pid)
               (setf (job-reported job) status)
               (when (eq status ':done)
                 (job-unregister job))))
           0))))

(defcommand fg (&optional spec)
  "Resume a stopped or background job in the foreground. SPEC picks
   the job like jobs shows it: %1, 1, %- or a command prefix; without
   it the current job resumes."
  (let ((job (job-find spec)))
    (cond ((null job)
           (job--complain "fg" spec))
          ((eq (job-refresh job) ':done)
           (job--complain "fg" (or spec (job-id job))))
          (t
           (job-touch job)
           (format t "~a~%" (job-command job))
           (force-output)
           (job-run-foreground job :continue t)))))

(defcommand bg (&optional spec)
  "Resume a stopped job in the background, as if it had been launched
   with a trailing &. SPEC picks the job like fg."
  (let ((job (job-find spec)))
    (cond ((null job)
           (job--complain "bg" spec))
          ((eq (job-refresh job) ':done)
           (job--complain "bg" (or spec (job-id job))))
          ((eq (job-status job) ':running)
           (format *error-output* "~a~%"
                   (ansi-colorize
                    (format nil "bg: job [~d] is already running"
                            (job-id job))
                    ':red))
           (force-output *error-output*)
           0)
          (t
           (job-touch job)
           (job--continue-processes job)
           (setf (job-status job) ':running)
           (setf (job-reported job) ':running)
           (format t "[~d]~a ~a &~%"
                   (job-id job) (job-mark job) (job-command job))
           (force-output)
           0))))

(defun run (program &rest arguments)
  "Run PROGRAM with ARGUMENTS in the foreground and return its exit
   status. PROGRAM is a symbol or a string, arguments are stringified
   with PRINC-TO-STRING. Signals COMMAND-NOT-FOUND-ERROR."
  (let ((name  (command-designator-name program))
        (words (mapcar #'princ-to-string arguments)))
    (multiple-value-bind (kind target)
        (command-resolve-fresh name)
      (let ((*job-command-label* (format nil "~{~a~^ ~}" (cons name words))))
        (setf *last-status*
              (ecase kind
                (:builtin  (command-execute-builtin target words))
                (:external (command-execute-external target words))
                (:unknown  (error 'command-not-found-error :name name))))))))
