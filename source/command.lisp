;;;; -- Commands --
;;;
;;; The COMMAND class, builtin definition, PATH lookup and external
;;; program execution.

(in-package #:cclsh)

(defvar *last-status* 0
  "Exit status of the last executed command.")

(defvar *path-cache* (make-hash-table :test #'equal)
  "Cache of PATH lookups: command name to namestring or NIL.")

(defvar *path-cache-source* nil
  "The PATH value *PATH-CACHE* was built against.")

(defvar *path-command-names* nil
  "Cached sorted list of executable names found in PATH, for
   completion.")

(defvar *path-command-names-source* nil
  "The PATH value *PATH-COMMAND-NAMES* was built against.")


;;; Conditions

(define-condition shell-error (error)
  ()
  (:documentation "Base condition for cclsh failures."))

(define-condition command-not-found-error (shell-error)
  ((name :initarg :name :reader command-not-found-name))
  (:documentation "Signaled when a command word resolves to nothing.")
  (:report (lambda (condition stream)
             (format stream "command not found: ~a"
                     (command-not-found-name condition)))))


;;; The COMMAND class

(defclass command ()
  ((name          :initarg :name          :reader command-name)
   (function      :initarg :function      :reader command-function)
   (documentation :initarg :documentation :reader command-documentation :initform nil))
  (:documentation "A shell command implemented in Lisp."))

(defmacro defcommand (name (&rest lambda-list) &body body)
  "Define NAME as a shell command. Binds NAME to a COMMAND instance and
   defines a function of the same name, so the command is callable from
   both command lines and Lisp forms."
  (multiple-value-bind (documentation forms)
      (if (and (stringp (first body)) (rest body))
          (values (first body) (rest body))
          (values nil body))
    `(progn
       (defun ,name ,lambda-list
         ,@(when documentation (list documentation))
         ,@forms)
       (defparameter ,name
         (make-instance 'command
                        :name          ',name
                        :documentation ,documentation
                        :function      (function ,name)))
       ',name)))


;;; Path utilities

(defun pathname-directory-form-p (pathname)
  "True when PATHNAME is in directory form: no name and no type.
   Dotfiles like .hidden parse with a NIL name but a non-NIL type, so
   checking the name alone misclassifies them."
  (and (null (pathname-name pathname))
       (null (pathname-type pathname))))


;;; PATH lookup

(defun path-directories ()
  "Return the directories listed in the PATH environment variable."
  (let ((path (or (getenv "PATH") "")))
    (loop with start = 0
          for split = (position #\: path :start start)
          for piece = (subseq path start split)
          when (plusp (length piece))
            collect piece
          while split
          do (setf start (1+ split)))))

(defun path--search-uncached (name)
  "Scan the PATH directories for an executable file called NAME."
  (loop for directory in (path-directories)
        for candidate = (concatenate 'string directory "/" name)
        for found = (ignore-errors (probe-file candidate))
        when (and found (not (pathname-directory-form-p found)))
          return (namestring found)))

(defun path-search (name)
  "Find the executable NAME in PATH. Returns a namestring or NIL.
   Results are cached until PATH changes or REHASH clears the cache."
  (let ((path (or (getenv "PATH") "")))
    (unless (equal path *path-cache-source*)
      (clrhash *path-cache*)
      (setf *path-cache-source* path)))
  (multiple-value-bind (cached present)
      (gethash name *path-cache*)
    (if present
        cached
        (setf (gethash name *path-cache*) (path--search-uncached name)))))


;;; Resolution

(defun path-command-names-note (name)
  "Record NAME in the completion name cache after a fresh PATH hit."
  (when (and *path-command-names*
             (not (member name *path-command-names* :test #'string=)))
    (setf *path-command-names*
          (merge 'list (list name) *path-command-names* #'string<)))
  (values))

(defun command-resolve (word)
  "Resolve WORD to a runnable command.
   Returns (values :builtin command), (values :external path) or
   (values :unknown nil). Words containing a slash bypass builtins and
   PATH and are treated as direct file paths."
  (cond ((find #\/ word)
         (let ((found (ignore-errors (probe-file word))))
           (if (and found (not (pathname-directory-form-p found)))
               (values ':external (namestring found))
               (values ':unknown nil))))
        (t
         (let ((symbol (find-symbol (string-upcase word) *package*)))
           (if (and symbol
                    (boundp symbol)
                    (typep (symbol-value symbol) 'command))
               (values ':builtin (symbol-value symbol))
               (let ((path (path-search word)))
                 (if path
                     (values ':external path)
                     (values ':unknown nil))))))))

(defun word-evaluates-alone-p (word)
  "True when WORD typed alone would evaluate as Lisp rather than fail
   as an unknown command: it is a keyword or number literal, or names
   a bound variable. Uses FIND-SYMBOL so probing never interns, and
   its second value so the symbol NIL still counts as found."
  (cond ((and (> (length word) 1)
              (char= (char word 0) #\:))
         t)
        ((multiple-value-bind (symbol found)
             (find-symbol (string-upcase word) *package*)
           (and found (boundp symbol)))
         t)
        ((lisp-number-text-p word)
         (multiple-value-bind (object position)
             (ignore-errors
               (let ((*read-eval* nil))
                 (read-from-string word)))
           (and position
                (numberp object)
                (= position (length word))
                t)))
        (t
         nil)))

(defun command-resolve-fresh (word)
  "Resolve WORD like COMMAND-RESOLVE, but retry a PATH miss with a
   fresh filesystem scan. Highlighting caches a miss for every prefix
   typed, so a program installed after the cache was built would stay
   invisible until REHASH; execution paths use this instead and pay
   the rescan only when the cache says no. A fresh hit backfills the
   lookup and completion caches."
  (multiple-value-bind (kind target)
      (command-resolve word)
    (if (and (eq kind ':unknown)
             (not (find #\/ word)))
        (let ((found (path--search-uncached word)))
          (if found
              (progn
                (setf (gethash word *path-cache*) found)
                (path-command-names-note word)
                (values ':external found))
              (values ':unknown nil)))
        (values kind target))))


;;; Execution

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

(defun command-execute-builtin (command arguments)
  "Apply the builtin COMMAND to ARGUMENTS. Returns an exit status: the
   command's return value when it is an integer, zero otherwise."
  (let ((result (apply (command-function command) arguments)))
    (if (integerp result) result 0)))

(defun command-designator-name (designator)
  "Normalize a command DESIGNATOR (symbol or string) to a name string."
  (etypecase designator
    (symbol (string-downcase (symbol-name designator)))
    (string designator)))

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
