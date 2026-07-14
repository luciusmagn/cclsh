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

(defun check-write-utf8-file (path contents)
  "Write CONTENTS to PATH as UTF-8 and return PATH."
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction ':output
                          :if-exists ':supersede
                          :if-does-not-exist ':create
                          :external-format ':utf-8)
    (write-string contents stream))
  path)

(defun check-set-locale (locale)
  "Set Linux LC_ALL to LOCALE, or query it when LOCALE is NIL."
  (let ((pointer
          (if locale
              (ccl::with-utf-8-cstr (encoded locale)
                (ccl:external-call "setlocale"
                                   :int 6 :address encoded :address))
              (ccl:external-call "setlocale"
                                 :int 6 :address (ccl:%null-ptr) :address))))
    (unless (ccl:%null-ptr-p pointer)
      (ccl::%get-utf-8-cstring pointer))))

(defun check-path-mode (path)
  "Return PATH permissions as three or four octal digits."
  (string-trim '(#\Space #\Tab #\Newline #\Return)
               (uiop:run-program (list "stat" "-c" "%a" path)
                                 :output ':string)))

(defvar *check-stage-arguments* nil
  "Last argument vector received by CHECK-STAGE-ARGUMENTS.")

(cclsh:defcommand check-stage-arguments (&rest arguments)
  "Record ARGUMENTS for stage-expansion regression checks."
  (setf *check-stage-arguments* (copy-list arguments))
  23)

(cclsh:defcommand check-stage-emit (&rest arguments)
  "Print ARGUMENTS for pipeline stage-expansion regression checks."
  (format t "~s~%" arguments)
  41)


;;;; -- Clinedi boundary --

(check-equal "Clinedi supplies ANSI presentation"
             (find-symbol "ANSI-STRIP" '#:clinedi)
             'cclsh::ansi-strip)
(check-equal "Clinedi supplies Unicode cell geometry"
             2
             (clinedi:text-cell-width "猫"))


;;;; -- Prompt customization --

(let* ((*package* (find-package '#:cclsh-user))
       (prefix   (format nil "~a@~a (CCLSH-USER) "
                         (cclsh::prompt--username)
                         (cclsh::prompt--hostname)))
       (prompt   (cclsh::ansi-strip (cclsh:prompt-default :status 0))))
  (check-equal "default prompt begins with identity and package"
               0
               (search prefix prompt)))

(let ((old-package-environment (cclsh:getenv "CCLSH_PACKAGE"))
      (received                nil)
      (custom-prompt           (format nil "custom~%> ")))
  (unwind-protect
      (let* ((*package* (find-package '#:cclsh-user))
             (expected-job-count (cclsh::jobs-count))
             (cclsh:*last-status* 73)
             (cclsh:*prompt-function*
               (lambda (&key status duration-milliseconds columns job-count
                        &allow-other-keys)
                 (setf received
                       (list status duration-milliseconds columns job-count
                             cclsh:*last-status*
                             (cclsh:getenv "CCLSH_PACKAGE")))
                 (setf cclsh:*last-status* 99)
                 custom-prompt)))
        (check-equal "custom prompt result is used verbatim"
                     custom-prompt
                     (cclsh::prompt-render 7 125 91))
        (check-equal "custom prompt receives shell snapshots"
                     (list 7 125 91 expected-job-count 7 "CCLSH-USER")
                     received)
        (check-equal "custom prompt preserves the last command status"
                     73
                     cclsh:*last-status*))
    (if old-package-environment
        (cclsh:setenv "CCLSH_PACKAGE" old-package-environment)
        (cclsh::unsetenv "CCLSH_PACKAGE"))))

(let* ((identity (format nil "~a@~a"
                         (cclsh::prompt--username)
                         (cclsh::prompt--hostname)))
       (cases
         (list (list "NIL custom prompt selects the default"
                     (lambda (&key &allow-other-keys)
                       nil)
                     nil)
               (list "failing custom prompt selects the default"
                     (lambda (&key &allow-other-keys)
                       (error "injected prompt failure"))
                     "renderer failed")
               (list "non-string custom prompt selects the default"
                     (lambda (&key &allow-other-keys)
                       42)
                     "instead of a string or NIL"))))
  (dolist (case cases)
    (destructuring-bind (name renderer expected-error) case
      (let ((errors (make-string-output-stream))
            (cclsh:*prompt-function* renderer))
        (let ((*error-output* errors))
          (check-equal name
                       0
                       (search identity
                               (cclsh::ansi-strip
                                (cclsh::prompt-render 0 0 80)))))
          (let ((reported (get-output-stream-string errors)))
            (check-equal (format nil "~a error report" name)
                         (not (null expected-error))
                         (and expected-error
                              (not (null (search expected-error reported))))))))))

;;;; -- Command line arguments --

(dolist (case
          '(("plain command mode" ("-c" "echo plain")
             (:command "echo plain" nil nil))
            ("login command group" ("-lc" "echo login")
             (:command "echo login" t nil))
            ("reverse login command group" ("-cl" "echo reverse")
             (:command "echo reverse" t nil))
            ("interactive login command group" ("-ilc" "echo both")
             (:command "echo both" t nil))
            ("interactive command group" ("-ic" "echo configured")
             (:command "echo configured" t nil))
            ("separate login and command flags" ("-l" "-c" "echo split")
             (:command "echo split" t nil))
            ("unknown letters mixed with command flags"
             ("-xlc" "echo tolerant")
             (:command "echo tolerant" t nil))
            ("unknown letters mixed with plain command mode"
             ("-xc" "echo safe")
             (:command "echo safe" nil nil))
            ("command operand beginning with dash" ("-c" "-l")
             (:command "-l" nil nil))
            ("missing command" ("-c")
             (:missing-command nil nil nil))
            ("configured missing command" ("-l" "-c")
             (:missing-command nil t nil))
            ("unknown short flags" ("-xyz")
             (:main nil nil nil))
            ("configuration flags without command" ("-lix")
             (:main nil t nil))
            ("long option is not a short group" ("--lc" "script.cclsh")
             (:script "script.cclsh" nil nil))
            ("uppercase C remains unknown" ("-C" "script.cclsh")
             (:script "script.cclsh" nil nil))
            ("script stops option parsing"
             ("script.cclsh" "-lc" "echo not-executed")
             (:script "script.cclsh" nil
              ("-lc" "echo not-executed")))
            ("double dash permits a dash-prefixed script"
             ("--" "-script.cclsh" "--help")
             (:script "-script.cclsh" nil ("--help")))
            ("double dash without a script starts normally" ("--")
             (:main nil nil nil))))
  (destructuring-bind (name arguments expected) case
    (check-equal name expected
                 (multiple-value-list
                  (cclsh::shell--argument-plan arguments)))))

(check-equal "absolute invocation path preserves an installed symlink"
             "/usr/local/bin/cclsh"
             (cclsh::shell--invocation-path
              :arguments '("/usr/local/bin/cclsh" "-c" "exit 0")
              :executable-path "/usr/local/bin/cclsh"))

(check-equal "relative invocation path defers to the executable fallback"
             nil
             (cclsh::shell--invocation-path
              :arguments '("cclsh" "-c" "exit 0")
              :executable-path "/usr/local/bin/cclsh"))

(check-equal "login argv uses the account's stable shell path"
             "/usr/local/bin/cclsh"
             (cclsh::shell--invocation-path
              :arguments '("-cclsh")
              :environment-shell "/usr/local/bin/cclsh"
              :executable-path "/usr/local/bin/cclsh"))

(check-equal "login argv rejects a stale inherited shell"
             nil
             (cclsh::shell--invocation-path
              :arguments '("-cclsh")
              :environment-shell "/bin/sh"
              :executable-path "/usr/local/bin/cclsh"))


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
                       (format nil "a~cy" (code-char 27))))))
  (check-equal "unique non-directory completion adds a space"
               "printf "
               (cclsh::line-editor--accept-completion "printf"))
  (check-equal "unique directory completion remains open"
               "source/"
               (cclsh::line-editor--accept-completion "source/")))


;;;; -- Lisp glob arguments --

(let* ((root
         (format nil "/tmp/cclsh-check-glob-~d/"
                 (ccl:external-call "getpid" :int)))
       (single-path      (concatenate 'string root "only.single"))
       (alpha-path       (concatenate 'string root "alpha.multi"))
       (space-path       (concatenate 'string root "beta space.multi"))
       (single-pattern   (concatenate 'string root "*.single"))
       (multiple-pattern (concatenate 'string root "*.multi"))
       (missing-pattern  (concatenate 'string root "*.absent"))
       (old-home         (cclsh:getenv "HOME"))
       (variable         "CCLSH_CHECK_GLOB_ROOT")
       (old-variable     (cclsh:getenv variable)))
  (flet ((restore-environment (name value)
           (if value
               (cclsh:setenv name value)
               (cclsh::unsetenv name))))
    (unwind-protect
        (progn
          (check-write-utf8-file single-path "single")
          (check-write-utf8-file alpha-path "alpha")
          (check-write-utf8-file space-path "space")

          (multiple-value-bind (symbol status)
              (find-symbol "GLOB" '#:cclsh)
            (check-equal "glob is exported" ':external status)
            (check-equal "glob is callable"
                         t
                         (and symbol (fboundp symbol) t))
            (check-equal "glob is visible in cclsh-user"
                         symbol
                         (find-symbol "GLOB" '#:cclsh-user)))

          (check-equal "one-match glob returns a proper list"
                       (list single-path)
                       (cclsh:glob single-pattern))
          (check-equal "multiple glob matches are sorted"
                       (list alpha-path space-path)
                       (cclsh:glob multiple-pattern))
          (check-equal "multiple glob patterns retain their positions"
                       (list single-path alpha-path space-path)
                       (cclsh:glob single-pattern multiple-pattern))
          (check-equal "unmatched glob remains literal"
                       (list missing-pattern)
                       (cclsh:glob missing-pattern))
          (check-equal "glob without patterns returns NIL"
                       nil
                       (cclsh:glob))
          (check-equal "glob rejects a non-string pattern"
                       t
                       (handler-case
                           (progn (cclsh:glob 42) nil)
                         (type-error () t)))

          (cclsh:setenv "HOME" (string-right-trim "/" root))
          (check-equal "glob expands a leading tilde"
                       (list single-path)
                       (cclsh:glob "~/*.single"))
          (cclsh:setenv variable (string-right-trim "/" root))
          (check-equal "glob expands an environment variable"
                       (list single-path)
                       (cclsh:glob
                        (format nil "$~a/*.single" variable)))

          (check-equal "cmd glob returns the command status"
                       23
                       (cclsh:cmd check-stage-arguments
                                  "before"
                                  (cclsh:glob multiple-pattern)
                                  "after"))
          (check-equal "cmd glob splices matches in place"
                       (list "before" alpha-path space-path "after")
                       *check-stage-arguments*)
          (check-equal "cmd glob records the command status"
                       23
                       cclsh:*last-status*)

          (let ((matches (cclsh:glob multiple-pattern)))
            (cclsh:cmd check-stage-arguments matches)
            (check-equal "stored glob list remains spliceable"
                         (list alpha-path space-path)
                         *check-stage-arguments*))
          (cclsh:cmd check-stage-arguments '("left" 2))
          (check-equal "proper list stage argument splices one level"
                       (list "left" "2")
                       *check-stage-arguments*)
          (cclsh:cmd check-stage-arguments '((left right) "tail"))
          (check-equal "nested stage list does not flatten recursively"
                       (list "(LEFT RIGHT)" "tail")
                       *check-stage-arguments*)
          (cclsh:cmd check-stage-arguments nil "after")
          (check-equal "NIL stage argument contributes no words"
                       (list "after")
                       *check-stage-arguments*)
          (cclsh:cmd check-stage-arguments "*.multi")
          (check-equal "ordinary string stage argument remains literal"
                       (list "*.multi")
                       *check-stage-arguments*)

          (multiple-value-bind (text status)
              (cclsh:capture
                (check-stage-emit "before"
                                  (cclsh:glob multiple-pattern)
                                  "after"))
            (check-equal "pipeline glob splices matches"
                         (format nil "~s"
                                 (list "before" alpha-path
                                       space-path "after"))
                         text)
            (check-equal "pipeline glob keeps stage status" 41 status)))

          (check-equal "single glob may supply a redirect path"
                       41
                       (cclsh:pipe
                         (check-stage-emit "redirected")
                         (to (cclsh:glob single-pattern))))
          (check-equal "single glob redirect receives stage output"
                       (format nil "~s~%" (list "redirected"))
                       (uiop:read-file-string single-path))

          (setf *check-stage-arguments* '(:not-started))
          (check-equal "multi-match redirect fails arity validation"
                       t
                       (handler-case
                           (progn
                             (cclsh:pipe
                               (check-stage-arguments "started")
                               (to (cclsh:glob multiple-pattern)))
                             nil)
                         (cclsh::pipeline-syntax-error () t)))
          (check-equal "invalid glob redirect starts no command"
                       '(:not-started)
                       *check-stage-arguments*)
      (restore-environment "HOME" old-home)
      (restore-environment variable old-variable)
      (ignore-errors
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore)))))


;;;; -- Implicit directory commands --

(let* ((root
         (format nil "/tmp/cclsh-check-implicit-cd-~d/"
                 (ccl:external-call "getpid" :int)))
       (child         (concatenate 'string root "implicit-child/"))
       (spaced        (concatenate 'string root "implicit-space dir/"))
       (old-directory (ccl:current-directory))
       (old-defaults  *default-pathname-defaults*)
       (old-pwd       (cclsh:getenv "PWD"))
       (old-oldpwd    (cclsh:getenv "OLDPWD"))
       (old-home      (cclsh:getenv "HOME"))
       (variable      "CCLSH_CHECK_IMPLICIT_DIR")
       (old-variable  (cclsh:getenv variable)))
  (flet ((current-directory-name ()
           (cclsh::directory-namestring-clean (ccl:current-directory)))
         (restore-environment (name value)
           (if value
               (cclsh:setenv name value)
               (cclsh::unsetenv name))))
    (unwind-protect
        (let ((*package* (find-package '#:cclsh-user))
              (*standard-output* (make-broadcast-stream))
              (*error-output* (make-broadcast-stream)))
          (ensure-directories-exist
           (concatenate 'string child ".keep"))
          (ensure-directories-exist
           (concatenate 'string spaced ".keep"))
          (ensure-directories-exist
           (concatenate 'string root "rehash/.keep"))
          (ensure-directories-exist
           (concatenate 'string root "true/.keep"))
          (check-write-utf8-file
           (concatenate 'string root "implicit-plain") "plain")
          (cclsh:cd root)

          (cclsh:cd child)
          (check-equal ".. changes to the parent directory"
                       0
                       (cclsh::dispatch-line ".."))
          (check-equal ".. changes the process directory"
                       (string-right-trim "/" root)
                       (current-directory-name))
          (check-equal "implicit cd updates PWD"
                       (string-right-trim "/" root)
                       (cclsh:getenv "PWD"))
          (check-equal "implicit cd updates OLDPWD"
                       (string-right-trim "/" child)
                       (cclsh:getenv "OLDPWD"))

          (check-equal "./ directory changes implicitly"
                       0
                       (cclsh::dispatch-line "./implicit-child"))
          (check-equal "./ directory becomes current"
                       (string-right-trim "/" child)
                       (current-directory-name))
          (cclsh:cd root)
          (check-equal "trailing slash directory changes implicitly"
                       0
                       (cclsh::dispatch-line "implicit-child/"))
          (check-equal "trailing slash directory becomes current"
                       (string-right-trim "/" child)
                       (current-directory-name))
          (cclsh:cd root)
          (check-equal "absolute directory changes implicitly"
                       0
                       (cclsh::dispatch-line
                        (string-right-trim "/" child)))
          (check-equal "absolute directory becomes current"
                       (string-right-trim "/" child)
                       (current-directory-name))
          (cclsh:cd root)
          (cclsh:setenv "HOME" (string-right-trim "/" root))
          (check-equal "tilde directory changes implicitly"
                       0
                       (cclsh::dispatch-line "~/implicit-child"))
          (check-equal "tilde directory becomes current"
                       (string-right-trim "/" child)
                       (current-directory-name))
          (cclsh:cd root)
          (cclsh:setenv variable (string-right-trim "/" child))
          (check-equal "variable directory changes implicitly"
                       0
                       (cclsh::dispatch-line
                        "$CCLSH_CHECK_IMPLICIT_DIR"))
          (check-equal "variable directory becomes current"
                       (string-right-trim "/" child)
                       (current-directory-name))
          (cclsh:cd root)
          (check-equal "singleton directory glob changes implicitly"
                       0
                       (cclsh::dispatch-line "./implicit-chil?"))
          (check-equal "singleton glob directory becomes current"
                       (string-right-trim "/" child)
                       (current-directory-name))
          (cclsh:cd root)
          (check-equal "quoted directory changes implicitly"
                       0
                       (cclsh::dispatch-line "'implicit-space dir/'"))
          (check-equal "quoted directory becomes current"
                       (string-right-trim "/" spaced)
                       (current-directory-name))

          (cclsh:cd root)
          (check-equal "bare directory remains a command lookup"
                       127
                       (cclsh::dispatch-line "implicit-child"))
          (check-equal "bare directory does not change directory"
                       (string-right-trim "/" root)
                       (current-directory-name))
          (check-equal "implicit directory rejects arguments"
                       127
                       (cclsh::dispatch-line "implicit-child/ extra"))
          (check-equal "directory arguments do not change directory"
                       (string-right-trim "/" root)
                       (current-directory-name))
          (check-equal "implicit directory rejects backgrounding"
                       1
                       (cclsh::dispatch-line "implicit-child/ &"))
          (check-equal "background rejection does not change directory"
                       (string-right-trim "/" root)
                       (current-directory-name))
          (check-equal "builtin wins over a same-named directory"
                       0
                       (cclsh::dispatch-line "rehash"))
          (check-equal "builtin precedence keeps the directory"
                       (string-right-trim "/" root)
                       (current-directory-name))
          (check-equal "external wins over a same-named directory"
                       0
                       (cclsh::dispatch-line "true"))
          (check-equal "external precedence keeps the directory"
                       (string-right-trim "/" root)
                       (current-directory-name))

          (check-equal "implicit directory highlights like cd"
                       ':cyan
                       (cclsh::highlight-command-name
                        "implicit-child/" t))
          (check-equal "bare directory remains unknown highlighting"
                       ':red
                       (cclsh::highlight-command-name
                        "implicit-child" t))
          (check-equal "directory with arguments remains unknown"
                       ':red
                       (cclsh::highlight-command-name
                        "implicit-child/" nil))
          (check-equal "multi-match directory glob remains unknown"
                       ':red
                       (cclsh::highlight-command-name "./implicit-*" t))
          (multiple-value-bind (start candidates displays)
              (cclsh::complete-line "implicit-" 9)
            (declare (ignore displays))
            (check-equal "command-position directory completion start"
                         0 start)
            (check-equal "command-position completion includes directory"
                         t
                         (not (null
                               (member "implicit-child/" candidates
                                       :test #'string=))))
            (check-equal "command-position completion excludes plain file"
                         nil
                         (member "implicit-plain" candidates
                                 :test #'string=))))
      (ignore-errors
        (setf (ccl:current-directory) old-directory)
        (setf *default-pathname-defaults* old-defaults))
      (restore-environment "PWD" old-pwd)
      (restore-environment "OLDPWD" old-oldpwd)
      (restore-environment "HOME" old-home)
      (restore-environment variable old-variable)
      (ignore-errors
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore)))))


;;;; -- Directory-change hooks --

(let* ((root
         (format nil "/tmp/cclsh-check-directory-hooks-~d/"
                 (ccl:external-call "getpid" :int)))
       (child         (concatenate 'string root "child/"))
       (nested        (concatenate 'string root "nested/"))
       (missing       (concatenate 'string root "missing/"))
       (old-directory (ccl:current-directory))
       (old-defaults  *default-pathname-defaults*)
       (old-pwd       (cclsh:getenv "PWD"))
       (old-oldpwd    (cclsh:getenv "OLDPWD"))
       (events        nil))
  (flet ((restore-environment (name value)
           (if value
               (cclsh:setenv name value)
               (cclsh::unsetenv name))))
    (unwind-protect
        (let ((cclsh:*directory-change-hooks* nil)
              (*error-output* (make-broadcast-stream)))
          (ensure-directories-exist
           (concatenate 'string child ".keep"))
          (ensure-directories-exist
           (concatenate 'string nested ".keep"))
          (cclsh:cd root)
          (labels ((record-state (old new)
                     (push (list ':first old new
                                 (cclsh:getenv "OLDPWD")
                                 (cclsh:getenv "PWD")
                                 (cclsh::directory-namestring-clean
                                  *default-pathname-defaults*))
                           events))

                   (fail-after-recording (old new)
                     (declare (ignore old new))
                     (push (list ':failing) events)
                     (error "hook failure"))

                   (record-later (old new)
                     (declare (ignore old new))
                     (push (list ':later) events)))
            (cclsh:directory-change-hook-add #'record-state)
            (cclsh:directory-change-hook-add #'fail-after-recording)
            (cclsh:directory-change-hook-add #'record-later)
            (cclsh:directory-change-hook-add #'record-state)
            (check-equal "directory hook registration is idempotent"
                         3
                         (length cclsh:*directory-change-hooks*))
            (check-equal "successful cd survives a failing hook"
                         0
                         (cclsh:cd child))
            (let ((ordered (nreverse events)))
              (check-equal "directory hooks retain registration order"
                           '(:first :failing :later)
                           (mapcar #'first ordered))
              (check-equal "directory hook sees committed state"
                           (list ':first
                                 (string-right-trim "/" root)
                                 (string-right-trim "/" child)
                                 (string-right-trim "/" root)
                                 (string-right-trim "/" child)
                                 (string-right-trim "/" child))
                           (first ordered)))
            (setf events nil)
            (check-equal "same-directory cd succeeds"
                         0
                         (cclsh:cd child))
            (check-equal "same-directory cd does not run hooks"
                         nil events)
            (check-equal "failed cd reports failure"
                         1
                         (cclsh:cd missing))
            (check-equal "failed cd does not run hooks"
                         nil events)
            (cclsh:directory-change-hook-remove #'record-later)
            (check-equal "directory hook removal takes effect"
                         2
                         (length cclsh:*directory-change-hooks*)))
          (let ((nested-status nil)
                (later-pwd nil)
                (cclsh:*directory-change-hooks* nil))
            (cclsh:cd root)
            (labels ((change-again (old new)
                       (declare (ignore old new))
                       (setf nested-status (cclsh:cd nested)))

                     (record-final-pwd (old new)
                       (declare (ignore old new))
                       (setf later-pwd (cclsh:getenv "PWD"))))
              (cclsh:directory-change-hook-add #'change-again)
              (cclsh:directory-change-hook-add #'record-final-pwd)
              (check-equal "outer cd survives a reentrant hook"
                           0
                           (cclsh:cd child))
              (check-equal "directory hooks reject a reentrant cd"
                           1 nested-status)
              (check-equal "reentrant cd leaves committed state coherent"
                           (string-right-trim "/" child)
                           later-pwd))))
      (ignore-errors
        (setf (ccl:current-directory) old-directory))
      (setf *default-pathname-defaults* old-defaults)
      (restore-environment "PWD" old-pwd)
      (restore-environment "OLDPWD" old-oldpwd)
      (ignore-errors
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist ':ignore)))))


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
  (check-equal "printed history string round-trips"
               multiline
               (read-from-string
                (with-output-to-string (stream)
                  (prin1 multiline stream)))))

(let* ((root (format nil "/tmp/cclsh-check-žluť-你好-~d/"
                     (ccl:external-call "getpid" :int)))
       (old-xdg (cclsh:getenv "XDG_CONFIG_HOME"))
       (text "Příliš žluťoučký kůň 🐈")
       (startup-symbol
         (intern "*CHECK-UTF8-STARTUP*" (find-package '#:cclsh-user))))
  (unwind-protect
      (let ((cclsh::*history*
              (make-array 0 :adjustable t :fill-pointer t))
            (ccl:*default-file-character-encoding* ':iso-8859-1))
        (cclsh:setenv "XDG_CONFIG_HOME" root)
        (let ((old-umask
                (ccl:external-call "umask"
                                   :unsigned-int 0
                                   :unsigned-int)))
          (unwind-protect
              (cclsh::history-append text)
            (ccl:external-call "umask"
                               :unsigned-int old-umask
                               :unsigned-int)))
        (check-equal "history configuration is private"
                     "700"
                     (check-path-mode (cclsh::config-directory)))
        (check-equal "history file is private"
                     "600"
                     (check-path-mode (cclsh::history-file)))
        (setf (fill-pointer cclsh::*history*) 0)
        (cclsh::history-load)
        (check-equal "UTF-8 history ignores the ambient file encoding"
                     text
                     (and (= (fill-pointer cclsh::*history*) 1)
                          (aref cclsh::*history* 0)))
        (makunbound startup-symbol)
        (check-write-utf8-file
         (cclsh::startup-file)
         (format nil
                 "(setf (symbol-value (find-symbol ~s ~s)) ~s)~%"
                 (symbol-name startup-symbol) "CCLSH-USER" text))
        (cclsh::startup-load)
        (check-equal "UTF-8 startup ignores the ambient file encoding"
                     text
                     (and (boundp startup-symbol)
                          (symbol-value startup-symbol))))
    (when (boundp startup-symbol)
      (makunbound startup-symbol))
    (if old-xdg
        (cclsh:setenv "XDG_CONFIG_HOME" old-xdg)
        (cclsh::unsetenv "XDG_CONFIG_HOME"))
    (ignore-errors
      (uiop:delete-directory-tree root
                                  :validate t
                                  :if-does-not-exist ':ignore))))


;;;; -- Current package environment --

(let* ((variable     "CCLSH_PACKAGE")
       (package-name  (format nil "CCLSH-CHECK-猫-PACKAGE-~d"
                              (ccl:external-call "getpid" :int)))
       (package       (make-package package-name :use nil))
       (old-variable  (cclsh:getenv variable)))
  (unwind-protect
      (let ((*package* package))
        (cclsh:setenv variable "STALE")
        (check-equal "package sync returns the canonical name"
                     package-name
                     (cclsh::environment-package-sync))
        (check-equal "package sync updates CCLSH_PACKAGE"
                     package-name
                     (cclsh:getenv variable))

        (cclsh:setenv variable "STALE")
        (check-equal "environment snapshot refreshes CCLSH_PACKAGE"
                     t
                     (not
                      (null
                       (find (format nil "~a=~a" variable package-name)
                             (cclsh:environment-variables)
                             :test #'string=))))

        (cclsh:setenv variable "STALE")
        (multiple-value-bind (output status)
            (cclsh::pipeline-capture
             (list (list "/usr/bin/printenv" variable)))
          (check-equal "external child receives current Lisp package"
                       0 status)
          (check-equal "external child package value is exact"
                       package-name output)))
    (delete-package package)
    (if old-variable
        (cclsh:setenv variable old-variable)
        (cclsh::unsetenv variable))))


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

(let* ((old-locale (check-set-locale nil))
       (czech-locale (or (check-set-locale "cs_CZ.UTF-8")
                         (check-set-locale "cs_CZ.utf8")))
       (expected "Adresář nebo soubor neexistuje"))
  (unwind-protect
      (when czech-locale
        (check-equal "UTF-8 libc error text"
                     expected
                     (cclsh::process--error-string 2))
        (check-equal "UTF-8 environment error report"
                     t
                     (not (null
                           (search expected
                                   (princ-to-string
                                    (make-condition
                                     'cclsh::environment-error
                                     :operation "set"
                                     :name "BROKEN"
                                     :code 2))))))
        (check-equal "UTF-8 terminal error report"
                     t
                     (not (null
                           (search expected
                                   (princ-to-string
                                    (make-condition
                                     'cclsh::terminal-control-error
                                     :operation "foreground"
                                     :process-group 1
                                     :code 2)))))))
    (when old-locale
      (check-set-locale old-locale))))


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
