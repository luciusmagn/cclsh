;;;; -- Pipelines and sequences --
;;;
;;; Lisp helpers over shell commands: PIPE connects stages with Unix
;;; style pipes, SEQ runs stages one after another, ALL and ANY are the
;;; && and || equivalents, and CAPTURE returns a pipeline's output as a
;;; string. A stage is written (name argument...) where NAME resolves
;;; exactly like the first word of a command line and the arguments are
;;; evaluated Lisp expressions.
;;;
;;; Redirection is spelled as stages recognized by name inside PIPE and
;;; CAPTURE: (from "file") as the first stage feeds standard input,
;;; (to "file") or (append-to "file") as the last stage writes it.
;;;
;;; Intermediate pipe streams use ISO-8859-1 so every byte round-trips
;;; unchanged through the Lisp-side copier threads; CAPTURE re-decodes
;;; the collected bytes as UTF-8.

(in-package #:cclsh)

(defparameter *pipe-external-format*
  (make-external-format :character-encoding ':iso-8859-1
                        :line-termination   ':unix)
  "Byte-transparent external format for intermediate pipe streams.")

(define-condition pipeline-syntax-error (shell-error)
  ((message :initarg :message :reader pipeline-syntax-error-message))
  (:documentation "Signaled for malformed pipeline redirections.")
  (:report (lambda (condition stream)
             (format stream "~a" (pipeline-syntax-error-message condition)))))

(defun pipeline--stage-form (stage)
  "Translate one (name argument...) STAGE into a runtime stage form."
  (destructuring-bind (head &rest arguments) stage
    `(list ,(command-designator-name head)
           ,@(mapcar (lambda (argument)
                       `(princ-to-string ,argument))
                     arguments))))

(defmacro pipe (&rest stages)
  "Run STAGES as a pipeline: (pipe (ls \"-la\") (grep \"lisp\")).
   (from \"file\") first feeds standard input, (to \"file\") or
   (append-to \"file\") last redirects the output. Returns the exit
   status of the last command stage."
  `(pipeline-run (list ,@(mapcar #'pipeline--stage-form stages))))

(defmacro capture (&rest stages)
  "Run STAGES as a pipeline and return its standard output as a string
   with trailing newlines removed, so (capture (git \"rev-parse\"
   \"HEAD\")) is sh's $(git rev-parse HEAD). Returns (values string
   status) and records *LAST-STATUS*."
  `(pipeline-capture (list ,@(mapcar #'pipeline--stage-form stages))))

(defmacro cmd (name &rest arguments)
  "Run one command from the middle of Lisp code: (cmd git \"status\").
   NAME resolves like the first word of a command line and ARGUMENTS
   are evaluated Lisp expressions, stringified like pipe stages.
   Returns the exit status."
  `(stage-sequence-run (list ,(pipeline--stage-form (list* name arguments)))
                       ':always))

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
  "Resolve every stage up front: command stages to (kind target
   arguments), redirect stages to (:redirect-in path) and
   (:redirect-out path append). Signals COMMAND-NOT-FOUND-ERROR and
   PIPELINE-SYNTAX-ERROR early."
  (loop for (name . arguments) in stages
        collect
        (flet ((redirect-path ()
                 (let ((path (first arguments)))
                   (when (or (null path) (zerop (length path)))
                     (error 'pipeline-syntax-error
                            :message (format nil "~a needs a file path"
                                             name)))
                   path)))
          (cond ((string= name "from")
                 (list ':redirect-in (redirect-path) nil))
                ((string= name "to")
                 (list ':redirect-out (redirect-path) nil))
                ((string= name "append-to")
                 (list ':redirect-out (redirect-path) t))
                (t
                 (multiple-value-bind (kind target)
                     (command-resolve-fresh name)
                   (when (eq kind ':unknown)
                     (error 'command-not-found-error :name name))
                   (list kind target arguments)))))))

(defun pipeline--split-redirects (resolved)
  "Strip and validate redirect stages. Returns (values commands
   input-path output-path append)."
  (let ((input-path  nil)
        (output-path nil)
        (append      nil)
        (commands    resolved))
    (when (and commands (eq (first (first commands)) ':redirect-in))
      (setf input-path (second (first commands)))
      (setf commands (rest commands)))
    (let ((final (first (last commands))))
      (when (and final (eq (first final) ':redirect-out))
        (setf output-path (second final))
        (setf append (third final))
        (setf commands (butlast commands))))
    (when (find-if (lambda (entry)
                     (member (first entry) '(:redirect-in :redirect-out)))
                   commands)
      (error 'pipeline-syntax-error
             :message "from must be the first stage, to and append-to the last"))
    (values commands input-path output-path append)))

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

(defun pipeline--copy-inline (input output)
  "Copy INPUT to OUTPUT in this thread until end of input."
  (loop for char = (read-char input nil nil)
        while char
        do (write-char char output))
  (force-output output)
  (values))

(defun pipeline--argument-encode (argument)
  "Re-encode ARGUMENT so the byte-transparent latin-1 argv encoding of
   a pipe spawn emits its UTF-8 bytes; run-program's :external-format
   also encodes the argument vector, not only the streams."
  (map 'string #'code-char
       (ccl:encode-string-to-octets argument :external-format ':utf-8)))

(defun pipeline--spawn-external (target arguments input last-stage-p)
  "Spawn one external stage. INPUT is NIL for the first stage (inherit
   the terminal) or a Lisp stream carrying the previous stage's output.
   Returns (values process output-stream)."
  (let ((process (run-program target
                              (mapcar #'pipeline--argument-encode arguments)
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

(defun pipeline--string-decode (string)
  "Reinterpret a byte-transparent latin-1 STRING as UTF-8, falling
   back to the raw string when it does not decode."
  (or (ignore-errors
        (ccl:decode-string-from-octets
         (map '(vector (unsigned-byte 8)) #'char-code string)
         :external-format ':utf-8))
      string))

(defun pipeline--execute (resolved capture)
  "Run RESOLVED stages: redirect stages feed or receive files, and
   with CAPTURE the final output is collected instead of written to
   the terminal. Returns (values status captured-string)."
  (multiple-value-bind (commands input-path output-path append)
      (pipeline--split-redirects resolved)
    (when (and capture output-path)
      (error 'pipeline-syntax-error
             :message "capture cannot combine with to or append-to"))
    (let ((interactive  (terminal-tty-p))
          (shell-group  (terminal-own-process-group))
          (redirected   (or output-path capture))
          (opened-input nil)
          (sink         nil)
          (captured     nil)
          (processes    nil)
          (status       0)
          (carried      nil)
          (stage-index  0)
          (foreground-p nil))
      (unwind-protect
          (progn
            (when input-path
              (setf opened-input
                    (open input-path
                          :direction       :input
                          :external-format *pipe-external-format*))
              (setf carried opened-input))
            (loop for (kind target arguments) in commands
                  for remaining on commands
                  for last-stage-p = (and (null (rest remaining))
                                          (not redirected))
                  do (ecase kind
                       (:external
                        (multiple-value-bind (process output)
                            (pipeline--spawn-external target arguments carried
                                                      last-stage-p)
                          (when (and interactive (zerop stage-index))
                            (terminal-foreground (external-process-id process))
                            (process-group-continue
                             (external-process-id process))
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
            (when redirected
              (cond (capture
                     (setf captured
                           (with-output-to-string (collected)
                             (when carried
                               (pipeline--copy-inline carried collected)))))
                    (t
                     (setf sink
                           (open output-path
                                 :direction         :output
                                 :if-exists         (if append
                                                        ':append
                                                        ':supersede)
                                 :if-does-not-exist :create
                                 :external-format   *pipe-external-format*))
                     (when carried
                       (pipeline--copy-inline carried sink)))))
            (let ((exit-statuses (mapcar #'external-wait
                                         (nreverse processes))))
              (when (and exit-statuses
                         (eq (first (first (last commands))) ':external))
                (setf status (first (last exit-statuses))))))
        (when foreground-p
          (terminal-foreground shell-group))
        (when sink
          (ignore-errors (close sink)))
        (when opened-input
          (ignore-errors (close opened-input))))
      (setf *last-status* status)
      (values status
              (and captured
                   (string-right-trim '(#\newline)
                                      (pipeline--string-decode captured)))))))

(defun pipeline-run (stages)
  "Run STAGES as a pipeline and return the last command's exit status.
   The first command stage owns the terminal, so Ctrl-C interrupts the
   pipeline from its head; later stages only touch pipes."
  (values (pipeline--execute (pipeline--resolve-stages stages) nil)))

(defun pipeline-capture (stages)
  "Run STAGES as a pipeline capturing standard output. Returns
   (values string status)."
  (multiple-value-bind (status captured)
      (pipeline--execute (pipeline--resolve-stages stages) t)
    (values (or captured "") status)))

(defun stage-sequence-run (stages mode)
  "Run STAGES one after another in the foreground. MODE is :ALWAYS,
   :WHILE-SUCCESSFUL (stop on failure) or :UNTIL-SUCCESSFUL (stop on
   success). Returns the deciding exit status."
  (let ((status 0))
    (loop for (name . arguments) in stages
          do (multiple-value-bind (kind target)
                 (command-resolve-fresh name)
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
