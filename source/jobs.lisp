;;;; -- External execution and jobs --
;;;
;;; Running external programs: spawning through CCL's run-program,
;;; handing the terminal to the child's process group for foreground
;;; commands and waiting for completion.

(in-package #:cclsh)

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

(defun command--run-foreground (process)
  "Wait for PROCESS as the terminal's foreground job and take the
   terminal back afterwards. CCL gives every child its own process
   group, so without this handoff a child touching the terminal would
   stop with SIGTTIN or SIGTTOU."
  (let ((interactive (terminal-tty-p))
        (shell-group (terminal-own-process-group))
        (child-group (external-process-id process)))
    (unwind-protect
        (progn
          (when (and interactive child-group)
            (terminal-foreground child-group)
            (process-group-continue child-group))
          (external-wait process))
      (when interactive
        (terminal-foreground shell-group)))))

(defun command-execute-external (path arguments)
  "Run the program at PATH with ARGUMENTS sharing the terminal.
   Returns the exit status."
  (let ((process (run-program path arguments
                              :input  t
                              :output t
                              :error  t
                              :wait   nil)))
    (command--run-foreground process)))

(defun run (program &rest arguments)
  "Run PROGRAM with ARGUMENTS in the foreground and return its exit
   status. PROGRAM is a symbol or a string, arguments are stringified
   with PRINC-TO-STRING. Signals COMMAND-NOT-FOUND-ERROR."
  (let ((name  (command-designator-name program))
        (words (mapcar #'princ-to-string arguments)))
    (multiple-value-bind (kind target)
        (command-resolve-fresh name)
      (setf *last-status*
            (ecase kind
              (:builtin  (command-execute-builtin target words))
              (:external (command-execute-external target words))
              (:unknown  (error 'command-not-found-error :name name)))))))
