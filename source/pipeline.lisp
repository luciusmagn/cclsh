;;;; -- Pipelines and sequences --
;;;
;;; Lisp helpers over shell commands: PIPE connects stages with Unix
;;; style pipes, SEQ runs stages one after another, ALL and ANY are the
;;; && and || equivalents. A stage is written (name argument...) where
;;; NAME resolves exactly like the first word of a command line and the
;;; arguments are evaluated Lisp expressions.
;;;
;;; Intermediate pipe streams use ISO-8859-1 so every byte round-trips
;;; unchanged through the Lisp-side copier threads.

(in-package #:cclsh)

(defparameter *pipe-external-format*
  (make-external-format :character-encoding ':iso-8859-1
                        :line-termination   ':unix)
  "Byte-transparent external format for intermediate pipe streams.")

(defun pipeline--stage-form (stage)
  "Translate one (name argument...) STAGE into a runtime stage form."
  (destructuring-bind (head &rest arguments) stage
    `(list ,(command-designator-name head)
           ,@(mapcar (lambda (argument)
                       `(princ-to-string ,argument))
                     arguments))))

(defmacro pipe (&rest stages)
  "Run STAGES as a pipeline: (pipe (ls \"-la\") (grep \"lisp\")).
   Returns the exit status of the last stage."
  `(pipeline-run (list ,@(mapcar #'pipeline--stage-form stages))))

(defmacro seq (&rest stages)
  "Run STAGES in order regardless of exit statuses, like cmd1; cmd2.
   Returns the exit status of the last stage."
  `(stage-sequence-run (list ,@(mapcar #'pipeline--stage-form stages))
                       ':always))

(defmacro all (&rest stages)
  "Run STAGES in order, stopping at the first failure, like cmd1 && cmd2.
   Returns the first nonzero exit status, or zero."
  `(stage-sequence-run (list ,@(mapcar #'pipeline--stage-form stages))
                       ':while-successful))

(defmacro any (&rest stages)
  "Run STAGES in order, stopping at the first success, like cmd1 || cmd2.
   Returns the first zero exit status, or the last status."
  `(stage-sequence-run (list ,@(mapcar #'pipeline--stage-form stages))
                       ':until-successful))


;;; Runtime

(defun pipeline--resolve-stages (stages)
  "Resolve every stage head up front. Returns a list of (kind target
   arguments) triples and signals COMMAND-NOT-FOUND-ERROR early."
  (loop for (name . arguments) in stages
        collect (multiple-value-bind (kind target)
                    (command-resolve name)
                  (when (eq kind ':unknown)
                    (error 'command-not-found-error :name name))
                  (list kind target arguments))))

(defun pipeline--copy-stream (input output)
  "Copy characters from INPUT to OUTPUT in a background thread, closing
   both ends when INPUT is exhausted."
  (process-run-function "cclsh pipeline copier"
                        (lambda ()
                          (ignore-errors
                            (loop for char = (read-char input nil nil)
                                  while char
                                  do (write-char char output)
                                  unless (listen input)
                                    do (force-output output)))
                          (ignore-errors (force-output output))
                          (ignore-errors (close output))
                          (ignore-errors (close input)))))

(defun pipeline--spawn-external (target arguments input last-stage-p)
  "Spawn one external stage. INPUT is NIL for the first stage (inherit
   the terminal) or a Lisp stream carrying the previous stage's output.
   Returns (values process output-stream)."
  (let ((process (run-program target arguments
                              :input           (if input ':stream t)
                              :output          (if last-stage-p t ':stream)
                              :error           t
                              :wait            nil
                              :external-format *pipe-external-format*)))
    (when input
      (pipeline--copy-stream input (external-process-input-stream process)))
    (values process
            (unless last-stage-p
              (external-process-output-stream process)))))

(defun pipeline--run-builtin (target arguments input last-stage-p)
  "Run one builtin stage inline. Output is buffered when the stage is
   not last so the next stage can stream it. Returns (values status
   output-stream)."
  (if last-stage-p
      (let ((*standard-input* (or input *standard-input*)))
        (values (command-execute-builtin target arguments) nil))
      (let ((buffer (make-string-output-stream)))
        (let ((*standard-input*  (or input *standard-input*))
              (*standard-output* buffer))
          (let ((status (command-execute-builtin target arguments)))
            (values status
                    (make-string-input-stream
                     (get-output-stream-string buffer))))))))

(defun pipeline-run (stages)
  "Run resolved STAGES as a pipeline and return the last exit status.
   When the first stage is external it reads the terminal, so it gets
   the terminal's foreground process group for the pipeline's duration;
   later stages only touch pipes."
  (let ((resolved     (pipeline--resolve-stages stages))
        (interactive  (terminal-tty-p))
        (shell-group  (terminal-own-process-group))
        (processes    nil)
        (status       0)
        (carried      nil)
        (stage-index  0)
        (foreground-p nil))
    (unwind-protect
        (progn
          (loop for (kind target arguments) in resolved
                for remaining on resolved
                for last-stage-p = (null (rest remaining))
                do (ecase kind
                     (:external
                      (multiple-value-bind (process output)
                          (pipeline--spawn-external target arguments carried
                                                    last-stage-p)
                        (when (and interactive (zerop stage-index))
                          (terminal-foreground (external-process-id process))
                          (process-group-continue (external-process-id process))
                          (setf foreground-p t))
                        (push process processes)
                        (setf carried output)))
                     (:builtin
                      (multiple-value-bind (builtin-status output)
                          (pipeline--run-builtin target arguments carried
                                                 last-stage-p)
                        (setf status builtin-status)
                        (setf carried output))))
                   (incf stage-index))
          (let ((exit-statuses (mapcar #'external-wait (nreverse processes))))
            (when (and exit-statuses
                       (eq (first (first (last resolved))) ':external))
              (setf status (first (last exit-statuses))))))
      (when foreground-p
        (terminal-foreground shell-group)))
    (setf *last-status* status)))

(defun stage-sequence-run (stages mode)
  "Run STAGES one after another in the foreground. MODE is :ALWAYS,
   :WHILE-SUCCESSFUL (stop on failure) or :UNTIL-SUCCESSFUL (stop on
   success). Returns the deciding exit status."
  (let ((status 0))
    (loop for (name . arguments) in stages
          do (multiple-value-bind (kind target)
                 (command-resolve name)
               (setf status
                     (ecase kind
                       (:builtin  (command-execute-builtin target arguments))
                       (:external (command-execute-external target arguments))
                       (:unknown  (error 'command-not-found-error :name name))))
               (setf *last-status* status)
               (case mode
                 (:while-successful
                  (unless (zerop status)
                    (return)))
                 (:until-successful
                  (when (zerop status)
                    (return))))))
    status))
