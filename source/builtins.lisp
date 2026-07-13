;;;; -- Builtin commands --
;;;
;;; Commands implemented in Lisp. Each DEFCOMMAND is callable from the
;;; command line by name and from Lisp forms as an ordinary function.

(in-package #:cclsh)

(defcommand exit (&optional status)
  "Exit the shell. STATUS defaults to the last command's status. With
   stopped jobs the first exit only warns; exit again to leave anyway."
  (if (jobs-exit-blocked-p)
      1
      (let ((code (cond ((null status)
                         *last-status*)
                        ((integerp status)
                         status)
                        (t
                         (or (parse-integer (princ-to-string status)
                                            :junk-allowed t)
                             0)))))
        (terminal-restore)
        (shell-quit code))))

(defcommand rehash ()
  "Forget cached PATH lookups and completion candidates."
  (clrhash *path-cache*)
  (setf *path-cache-source* nil)
  (setf *path-command-names* nil)
  (setf *path-command-names-source* nil)
  0)

(defcommand commands ()
  "List the shell commands defined in Lisp."
  (let ((found nil))
    (do-symbols (symbol *package*)
      (when (and (boundp symbol)
                 (typep (symbol-value symbol) 'command))
        (pushnew symbol found)))
    (let* ((sorted (sort found #'string< :key #'symbol-name))
           (width  (loop for symbol in sorted
                         maximize (length (symbol-name symbol)))))
      (dolist (symbol sorted)
        (let* ((instance      (symbol-value symbol))
               (documentation (command-documentation instance))
               (name          (format nil "~va" width
                                      (string-downcase (symbol-name symbol)))))
          (format t "~a  ~a~%"
                  (terminal-colorize name ':cyan)
                  (if documentation
                      (subseq documentation 0
                              (position #\newline documentation))
                      ""))))))
  0)


;;; Environment

(defcommand export (&rest assignments)
  "Set environment variables: export NAME=value... A bare NAME prints
   its current value; no arguments print the whole environment."
  (if (null assignments)
      (dolist (entry (environment-variables))
        (format t "~a~%" entry))
      (dolist (assignment assignments)
        (let ((split (position #\= assignment)))
          (if split
              (setenv (subseq assignment 0 split)
                      (subseq assignment (1+ split)))
              (let ((value (getenv assignment)))
                (if value
                    (format t "~a=~a~%" assignment value)
                    (format *error-output* "export: ~a is unset~%"
                            assignment)))))))
  0)

(defcommand unset (&rest names)
  "Remove environment variables. Names are symbols or strings."
  (dolist (name names)
    (unsetenv name))
  0)


;;; Quicklisp

(defun quicklisp-setup ()
  "Make Quicklisp available in the running shell. Loads an existing
   ~/quicklisp/setup.lisp, or downloads and installs Quicklisp with
   curl on first use. Returns true when Quicklisp is available."
  (let ((setup (merge-pathnames "quicklisp/setup.lisp"
                                (user-homedir-pathname))))
    (cond ((find-package '#:quicklisp)
           t)
          ((probe-file setup)
           (load setup :verbose nil :external-format ':utf-8)
           t)
          (t
           (let ((bootstrap (concatenate 'string (config-directory)
                                         "quicklisp-bootstrap.lisp")))
             (ensure-directories-exist (config-directory))
             (when (zerop (run "curl" "-fsSL" "-o" bootstrap
                               "https://beta.quicklisp.org/quicklisp.lisp"))
               (load bootstrap :verbose nil :external-format ':utf-8)
               (funcall (find-symbol "INSTALL" '#:quicklisp-quickstart))
               t))))))
