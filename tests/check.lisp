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

(let ((complete
        '("(list #| ignored ) #| nested ( |# |# 1)"
          "(list |symbol with ) ; and hash text| 1)"
          "(list |escaped \\| and )| 1)"
          "(list #\\) 1)"
          "(list foo\\) 1)"
          "echo (list #| ignored ) |# #\\) |symbol )| foo\\))"))
      (open
        '("(list #| unfinished"
          "(list |unfinished"
          "echo (list #| unfinished"
          "echo (list |unfinished")))
  (dolist (line complete)
    (check-equal (format nil "complete reader syntax in ~s" line)
                 nil
                 (cclsh::input-line-open-p line))
    (check-equal (format nil "highlight preserves reader syntax in ~s" line)
                 line
                 (cclsh::ansi-strip (cclsh::highlight-line line))))
  (dolist (line open)
    (check-equal (format nil "unfinished reader syntax in ~s" line)
                 t
                 (not (null (cclsh::input-line-open-p line))))))


;;;; -- Completion safety --

(let ((*package* (find-package '#:cclsh-user)))
  (dolist (name '("odd name" "odd(name)" "odd&name" "odd\"name"
                  "odd'name" "odd\\name" "odd$name" "odd*name"
                  "odd?name" "~odd"))
    (let ((escaped (cclsh::completion--escape name)))
      (check-equal (format nil "completed command round-trip for ~s" name)
                   (list name)
                   (cclsh::command-line-words escaped))))
  (let* ((name    (format nil "bell~c esc~c newline~% c1~c"
                          (code-char 7) (code-char 27) (code-char 133)))
         (escaped (cclsh::completion--escape name))
         (display (cclsh::completion--display name)))
    (check-equal "control-bearing completion round-trip"
                 (list name)
                 (cclsh::command-line-words escaped))
    (check-equal "accepted completion has no terminal controls"
                 nil
                 (find-if #'cclsh::completion--terminal-control-p escaped))
    (check-equal "completion display has no terminal controls"
                 nil
                 (find-if #'cclsh::completion--terminal-control-p display)))
  (check-equal "common prefix never leaves a dangling escape"
               "foo"
               (cclsh::completion--common-prefix '("foo\\(" "foo\\)")))
  (check-equal "common prefix never inserts a partial control expression"
               ""
               (cclsh::completion--common-prefix
                (list (cclsh::completion--escape (format nil "a~%x"))
                      (cclsh::completion--escape
                       (format nil "a~cy" (code-char 27)))))))


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


;;;; -- UTF-8 environment --

(let* ((name  "CCLSH_CHECK_ŽLUŤ")
       (value "Příliš žluťoučký kůň 🐈")
       (old   (cclsh:getenv name)))
  (unwind-protect
      (progn
        (cclsh:setenv name value)
        (check-equal "UTF-8 getenv round-trip"
                     value
                     (cclsh:getenv name))
        (check-equal "UTF-8 environ snapshot"
                     t
                     (not (null
                           (find (format nil "~a=~a" name value)
                                 (cclsh:environment-variables)
                                 :test #'string=))))
        (cclsh::unsetenv name)
        (check-equal "UTF-8 unsetenv"
                     nil
                     (cclsh:getenv name)))
    (if old
        (cclsh:setenv name old)
        (cclsh::unsetenv name))))


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

(let ((rendered
        (with-output-to-string (*standard-output*)
          (cclsh::editor--render 7 "pri" 3 80 0
                                 :suggestion "ntf example"))))
  (check-equal "redraw hides the cursor before repositioning"
               t
               (cclsh::string-prefix-p (cclsh::ansi-cursor-hide) rendered))
  (check-equal "redraw never erases or repaints the static prompt"
               nil
               (or (find #\return rendered)
                   (search "root" rendered)))
  (check-equal "redraw restores the cursor after positioning"
               t
               (let ((show (cclsh::ansi-cursor-show)))
                 (and (>= (length rendered) (length show))
                      (string= show rendered
                               :start2 (- (length rendered)
                                          (length show)))))))


;;;; -- Lisp-dispatched shell status --

(let ((cclsh:*last-status* 23)
      (*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (check-equal "ordinary successful Lisp returns zero"
               0
               (cclsh::dispatch-line "(+ 20 22)"))
  (check-equal "ordinary successful Lisp records zero"
               0
               cclsh:*last-status*))

(let ((cclsh:*last-status* 23)
      (*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (check-equal "same shell status survives Lisp dispatch"
               23
               (cclsh::dispatch-line
                "(cclsh::command-status-record 23)"))
  (check-equal "same shell status remains recorded"
               23
               cclsh:*last-status*))

(let ((cclsh:*last-status* 0)
      (*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (check-equal "shell status survives later Lisp forms"
               17
               (cclsh::dispatch-line
                "(cclsh::command-status-record 17) (+ 1 1)"))
  (check-equal "later Lisp forms retain recorded shell status"
               17
               cclsh:*last-status*))

(let ((cclsh:*last-status* 0)
      (*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (check-equal "Lisp error overrides an earlier shell status"
               1
               (cclsh::dispatch-line
                "(progn (cclsh::command-status-record 17) (error \"no\"))"))
  (check-equal "Lisp error records failure after shell status"
               1
               cclsh:*last-status*))


;;;; -- Job aggregation --

(check-equal "stopped external group wins over a live Lisp task"
             ':stopped
             (cclsh::job--aggregate-state '(:done :stopped) nil))
(check-equal "finished externals wait for a live Lisp task"
             ':running
             (cclsh::job--aggregate-state '(:done :done) nil))
(check-equal "job finishes after external and Lisp tasks"
             ':done
             (cclsh::job--aggregate-state '(:done :done) t))

(let ((done nil)
      (job  (cclsh::job-make)))
  (cclsh::job-add-auxiliary job (lambda () done))
  (check-equal "live auxiliary keeps an empty job running"
               ':running
               (cclsh::job-refresh job))
  (setf done t)
  (check-equal "completed auxiliary lets an empty job finish"
               ':done
               (cclsh::job-refresh job)))

(let ((job
        (cclsh::job-make
         :result-provider (lambda () (values ':signaled 2)))))
  (setf (cclsh::job-status job) ':done)
  (check-equal "logical final stage supplies resumed job status"
               130
               (cclsh::job-exit-status job))
  (check-equal "logical final stage supplies job display status"
               "Interrupt"
               (cclsh::job--status-text job)))

(let* ((callers      20)
       (job          (cclsh::job-make :command "concurrent startup"))
       (event        (cclsh::job-event job))
       (process      nil)
       (start-gate   (ccl:make-semaphore))
       (all-returned (ccl:make-semaphore))
       (result-lock  (ccl:make-lock "monitor startup results"))
       (returned     0)
       (failures     nil)
       (threads      nil))
  (unwind-protect
      (progn
        ;; Keep the child alive so child events cannot accidentally wake
        ;; startup callers stranded on the job's transition semaphore.
        (setf process
              (cclsh::shell-process-spawn
               "/usr/bin/sleep" (list "30")
               :process-group 0 :event event))
        (setf (cclsh::job-processes job) (list process)
              (cclsh::job-process-group job)
              (cclsh::shell-process-pid process))
        (loop repeat callers
              do (push
                  (ccl:process-run-function
                   "concurrent monitor starter"
                   (lambda ()
                     (ccl:wait-on-semaphore start-gate)
                     (let ((failure nil))
                       (handler-case
                           (cclsh::job-start-monitors job)
                         (error (condition)
                           (setf failure condition)))
                       (ccl:with-lock-grabbed (result-lock)
                         (when failure
                           (push (princ-to-string failure) failures))
                         (incf returned)
                         (when (= returned callers)
                           (ccl:signal-semaphore all-returned))))))
                  threads))
        (loop repeat callers
              do (ccl:signal-semaphore start-gate))
        (ccl:timed-wait-on-semaphore all-returned 3)
        (check-equal "concurrent monitor startup returns every caller"
                     callers
                     (ccl:with-lock-grabbed (result-lock) returned))
        (check-equal "concurrent monitor startup has no failures"
                     nil
                     (ccl:with-lock-grabbed (result-lock)
                       (copy-list failures))))
    (dolist (thread threads)
      (when (ccl::process-active-p thread)
        (ignore-errors (ccl:process-kill thread)))
      (ignore-errors (ccl:join-process thread)))
    (when process
      (ignore-errors (cclsh::job--kill-reap job)))))

(let* ((started (ccl:make-semaphore))
       (blocker (ccl:make-semaphore))
       (job     (cclsh::job-make :command "anchor signal"))
       (group   (cclsh::make-pipeline-task-group :job job))
       (anchor  (cclsh::process--make 1))
       (task
         (cclsh::make-pipeline-task
          :name "anchor-controlled task"
          :group group
          :function (lambda ()
                      (ccl:signal-semaphore started)
                      (ccl:wait-on-semaphore blocker)
                      (values 0 nil))))
       (thread nil))
  (setf (cclsh::pipeline-task-group-tasks group) (list task)
        (cclsh::pipeline-task-group-anchor-process group) anchor)
  (unwind-protect
      (progn
        (setf thread
              (ccl:process-run-function
               "anchor-controlled task"
               #'cclsh::pipeline--run-task task)
              (cclsh::pipeline-task-thread task) thread)
        (check-equal "anchor-controlled task starts"
                     t
                     (not (null
                           (ccl:timed-wait-on-semaphore started 2))))
        (ccl:with-lock-grabbed ((cclsh::shell-process-lock anchor))
          (setf (cclsh::shell-process-state anchor) ':signaled
                (cclsh::shell-process-code anchor)
                cclsh::+process-sigpipe+))
        (cclsh::pipeline--tasks-lifecycle-done-p group)
        (check-equal "anchor SIGPIPE does not cancel Lisp tasks"
                     nil
                     (cclsh::pipeline-task-group-aborted group))
        (ccl:with-lock-grabbed ((cclsh::shell-process-lock anchor))
          (setf (cclsh::shell-process-code anchor) 15))
        (cclsh::pipeline--tasks-lifecycle-done-p group)
        (ccl:join-process thread)
        (setf thread nil)
        (check-equal "fatal anchor signal completes Lisp task"
                     t
                     (cclsh::pipeline-task-done task))
        (check-equal "fatal anchor signal supplies Lisp task state"
                     ':signaled
                     (cclsh::pipeline-task-state task))
        (check-equal "fatal anchor signal supplies Lisp task code"
                     15
                     (cclsh::pipeline-task-code task)))
    (when (and thread (ccl::process-active-p thread))
      (ignore-errors
        (cclsh::pipeline--abort-tasks group :signal 9))
      (ignore-errors (ccl:join-process thread)))))


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
