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
;;; (to "file") or (append-to "file") as the last stage writes it, and
;;; anywhere in the pipeline (error-to "file"), (error-append-to
;;; "file") or (merge-error) direct the standard error of every stage
;;; to a file or into the ordinary output.
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
   (append-to \"file\") last redirects the output, and (error-to
   \"file\"), (error-append-to \"file\") or (merge-error) direct
   standard error. Returns the exit status of the last command stage."
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
                ((string= name "error-to")
                 (list ':redirect-error (redirect-path) nil))
                ((string= name "error-append-to")
                 (list ':redirect-error (redirect-path) t))
                ((string= name "merge-error")
                 (when arguments
                   (error 'pipeline-syntax-error
                          :message "merge-error takes no arguments"))
                 (list ':merge-error nil nil))
                (t
                 (multiple-value-bind (kind target)
                     (command-resolve-fresh name)
                   (when (eq kind ':unknown)
                     (error 'command-not-found-error :name name))
                   (list kind target arguments
                         (format nil "~{~a~^ ~}" (cons name arguments)))))))))

(defun pipeline--split-redirects (resolved)
  "Strip and validate redirect stages. Returns (values commands
   input-path output-path append error-path error-append merge-error)."
  (let ((error-path   nil)
        (error-append nil)
        (merge-error  nil)
        (remaining    nil))
    (dolist (entry resolved)
      (case (first entry)
        (:redirect-error
         (when (or error-path merge-error)
           (error 'pipeline-syntax-error
                  :message "error-to, error-append-to and merge-error are mutually exclusive"))
         (setf error-path (second entry))
         (setf error-append (third entry)))
        (:merge-error
         (when (or error-path merge-error)
           (error 'pipeline-syntax-error
                  :message "error-to, error-append-to and merge-error are mutually exclusive"))
         (setf merge-error t))
        (t
         (push entry remaining))))
    (let ((input-path  nil)
          (output-path nil)
          (append      nil)
          (commands    (nreverse remaining)))
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
      (values commands input-path output-path append
              error-path error-append merge-error))))

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

(defun pipeline--copy-signaled (input output semaphore)
  "Copy INPUT into the shared OUTPUT sink in a background thread,
   closing only INPUT and signaling SEMAPHORE when drained, so the
   caller can join every error copier before closing the sink."
  (process-run-function "cclsh stderr copier"
                        (lambda ()
                          (ignore-errors
                            (loop for char = (read-char input nil nil)
                                  while char
                                  do (write-char char output)
                                  unless (listen input)
                                    do (force-output output)))
                          (ignore-errors (force-output output))
                          (ignore-errors (close input))
                          (ccl:signal-semaphore semaphore))))

(defun pipeline--string-encode (text)
  "Re-encode TEXT so writing it through a byte-transparent latin-1
   channel emits its UTF-8 bytes. Used for spawn argument vectors
   (run-program's :external-format also encodes those) and for builtin
   error output going into a shared error sink."
  (map 'string #'code-char
       (ccl:encode-string-to-octets text :external-format ':utf-8)))

(defun pipeline--spawn-external (target arguments input last-stage-p
                                 error-mode)
  "Spawn one external stage. INPUT is NIL for the first stage (inherit
   the terminal) or a Lisp stream carrying the previous stage's
   output. ERROR-MODE is NIL for the terminal, :OUTPUT to merge
   standard error into the ordinary output, or :STREAM to expose the
   process error stream for the caller's collector. Returns (values
   process output-stream)."
  (let ((process (with-child-signal-defaults
                   (run-program target
                                (mapcar #'pipeline--string-encode arguments)
                                :input           (if input ':stream t)
                                :output          (if last-stage-p t ':stream)
                                :error           (case error-mode
                                                   ((nil)    t)
                                                   (:output  ':output)
                                                   (:stream  ':stream))
                                :wait            nil
                                :external-format *pipe-external-format*))))
    (when input
      (pipeline--copy-stream input (external-process-input-stream process)))
    (values process
            (unless last-stage-p
              (external-process-output-stream process)))))

(defun pipeline--run-builtin (target arguments input last-stage-p
                              error-sink merge-error)
  "Run one builtin stage inline. Output is buffered when the stage is
   not last so the next stage can stream it; *ERROR-OUTPUT* follows
   the pipeline's error redirection like the external stages' standard
   error, with builtin error text UTF-8 encoded into the
   byte-transparent ERROR-SINK. Returns (values status output-stream)."
  (labels ((invoke (output error)
             (let ((*standard-input*  (or input *standard-input*))
                   (*standard-output* output)
                   (*error-output*    (or error *error-output*)))
               (command-execute-builtin target arguments)))

           (invoke-with-error (output)
             (cond (merge-error
                    (invoke output output))
                   (error-sink
                    (let ((collected (make-string-output-stream)))
                      (prog1 (invoke output collected)
                        (write-string (pipeline--string-encode
                                       (get-output-stream-string collected))
                                      error-sink)
                        (force-output error-sink))))
                   (t
                    (invoke output nil)))))
    (if last-stage-p
        (values (invoke-with-error *standard-output*) nil)
        (let ((buffer (make-string-output-stream)))
          (let ((status (invoke-with-error buffer)))
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
  "Run RESOLVED stages: redirect stages feed or receive files, error
   redirects apply to every stage, and with CAPTURE the final output
   is collected instead of written to the terminal. An interactive
   pipeline stops as one job under Ctrl-Z and its files stay open for
   the resumed job; a capture continues through stops with a notice
   because the shell itself consumes its output. Returns (values
   status captured-string)."
  (multiple-value-bind (commands input-path output-path append
                        error-path error-append merge-error)
      (pipeline--split-redirects resolved)
    (when (and capture output-path)
      (error 'pipeline-syntax-error
             :message "capture cannot combine with to or append-to"))
    (let ((interactive       (terminal-tty-p))
          (shell-group       (terminal-own-process-group))
          (redirected        (or output-path capture))
          (opened-input      nil)
          (input-handed      nil)
          (sink              nil)
          (sink-semaphore    nil)
          (error-sink        nil)
          (error-semaphore   (ccl:make-semaphore))
          (error-copiers     0)
          (capture-collector nil)
          (capture-semaphore nil)
          (captured          nil)
          (processes         nil)
          (job               nil)
          (stopped           nil)
          (status            0)
          (carried           nil)
          (stage-index       0)
          (foreground-p      nil))
      (unwind-protect
          (progn
            (when input-path
              (setf opened-input
                    (open input-path
                          :direction       :input
                          :sharing         ':lock
                          :external-format *pipe-external-format*))
              (setf carried opened-input))
            (when error-path
              (setf error-sink
                    (open error-path
                          :direction         :output
                          :if-exists         (if error-append
                                                 ':append
                                                 ':supersede)
                          :if-does-not-exist :create
                          :sharing           ':lock
                          :external-format   *pipe-external-format*)))
            (loop for (kind target arguments) in commands
                  for remaining on commands
                  for last-stage-p = (and (null (rest remaining))
                                          (not redirected))
                  do (ecase kind
                       (:external
                        (when (and opened-input (eq carried opened-input))
                          (setf input-handed t))
                        (multiple-value-bind (process output)
                            (pipeline--spawn-external
                             target arguments carried last-stage-p
                             (cond (merge-error ':output)
                                   (error-sink  ':stream)
                                   (t           nil)))
                          (when (and interactive (zerop stage-index))
                            (terminal-foreground (external-process-id process))
                            (process-group-continue
                             (external-process-id process))
                            (setf foreground-p t))
                          (when error-sink
                            (pipeline--copy-signaled
                             (ccl:external-process-error-stream process)
                             error-sink error-semaphore)
                            (incf error-copiers))
                          (push process processes)
                          (setf carried output)))
                       (:builtin
                        (multiple-value-bind (builtin-status output)
                            (pipeline--run-builtin target arguments carried
                                                   last-stage-p
                                                   error-sink merge-error)
                          (setf status builtin-status)
                          (setf carried output))))
                     (incf stage-index))
            (when redirected
              (cond (capture
                     (setf capture-collector (make-string-output-stream))
                     (setf capture-semaphore (ccl:make-semaphore))
                     (if carried
                         (pipeline--copy-signaled carried capture-collector
                                                  capture-semaphore)
                         (ccl:signal-semaphore capture-semaphore)))
                    (t
                     (setf sink
                           (open output-path
                                 :direction         :output
                                 :if-exists         (if append
                                                        ':append
                                                        ':supersede)
                                 :if-does-not-exist :create
                                 :sharing           ':lock
                                 :external-format   *pipe-external-format*))
                     (when carried
                       (setf sink-semaphore (ccl:make-semaphore))
                       (pipeline--copy-signaled carried sink
                                                sink-semaphore)))))
            (when processes
              (setf job (job-make :processes (nreverse processes)
                                  :command   (format nil "~{~a~^ | ~}"
                                                     (mapcar #'fourth
                                                             commands))))
              (let ((builtin-status status))
                (multiple-value-setq (status stopped)
                  (job-wait-attended job :on-stop (if capture
                                                      ':continue
                                                      ':suspend)))
                (unless (or stopped
                            (eq (first (first (last commands))) ':external))
                  (setf status builtin-status))))
            (unless stopped
              (when capture-semaphore
                (ccl:wait-on-semaphore capture-semaphore)
                (setf captured
                      (get-output-stream-string capture-collector)))
              (when sink-semaphore
                (ccl:wait-on-semaphore sink-semaphore))
              (dotimes (copier error-copiers)
                (declare (ignorable copier))
                (ccl:wait-on-semaphore error-semaphore))))
        (when foreground-p
          (terminal-foreground shell-group))
        (cond (stopped
               ;; The stopped job keeps using the pipeline plumbing;
               ;; closing it here would truncate the resumed job, so
               ;; ownership moves to the job's completion cleanups.
               (when (and opened-input (not input-handed))
                 (ignore-errors (close opened-input)))
               (setf (job-cleanups job)
                     (list (lambda ()
                             (when sink-semaphore
                               (ccl:wait-on-semaphore sink-semaphore))
                             (when sink
                               (ignore-errors (close sink)))
                             (dotimes (copier error-copiers)
                               (declare (ignorable copier))
                               (ccl:wait-on-semaphore error-semaphore))
                             (when error-sink
                               (ignore-errors (close error-sink))))))
               (setf status (job--suspend job)))
              (t
               (when sink
                 (ignore-errors (close sink)))
               (when error-sink
                 (ignore-errors (close error-sink)))
               (when opened-input
                 (ignore-errors (close opened-input))))))
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
               (let ((*job-command-label*
                       (format nil "~{~a~^ ~}" (cons name arguments))))
                 (setf status
                       (ecase kind
                         (:builtin  (command-execute-builtin target
                                                             arguments))
                         (:external (command-execute-external target
                                                              arguments))
                         (:unknown  (error 'command-not-found-error
                                           :name name)))))
               (setf *last-status* status)
               (case mode
                 (:while-successful
                  (unless (zerop status)
                    (return)))
                 (:until-successful
                  (when (zerop status)
                    (return))))))
    status))
