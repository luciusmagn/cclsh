;;;; -- Builtin commands --
;;;
;;; Commands implemented in Lisp. Each DEFCOMMAND is callable from the
;;; command line by name and from Lisp forms as an ordinary function.

(in-package #:cclsh)

(defun directory-namestring-clean (pathname)
  "Namestring of PATHNAME without a trailing slash, except for /."
  (let ((name (namestring pathname)))
    (if (and (> (length name) 1)
             (char= (char name (1- (length name))) #\/))
        (subseq name 0 (1- (length name)))
        name)))

(defcommand cd (&optional target)
  "Change the working directory. Without TARGET goes home, with - goes
   back to the previous directory. Keeps PWD and OLDPWD updated."
  (let* ((old         (directory-namestring-clean (current-directory)))
         (back        (equal target "-"))
         (destination (cond ((or (null target) (equal target ""))
                             (home-directory))
                            (back
                             (getenv "OLDPWD"))
                            (t
                             (tilde-expand target)))))
    (when (null destination)
      (format *error-output* "~a~%" (ansi-colorize "cd: OLDPWD not set" ':red))
      (return-from cd 1))
    (handler-case
        (setf (current-directory) destination)
      (error ()
        (format *error-output* "~a~%"
                (ansi-colorize (format nil "cd: cannot change to ~a" destination)
                               ':red))
        (return-from cd 1)))
    (let ((new (directory-namestring-clean (current-directory))))
      (setf *default-pathname-defaults* (current-directory))
      (setenv "OLDPWD" old)
      (setenv "PWD" new)
      (when back
        (format t "~a~%" new)))
    0))

(defcommand exit (&optional status)
  "Exit the shell. STATUS defaults to the last command's status."
  (let ((code (cond ((null status)
                     *last-status*)
                    ((integerp status)
                     status)
                    (t
                     (or (parse-integer (princ-to-string status)
                                        :junk-allowed t)
                         0)))))
    (terminal-restore)
    (quit code)))

(defcommand rehash ()
  "Forget cached PATH lookups."
  (clrhash *path-cache*)
  (setf *path-cache-source* nil)
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
                  (ansi-colorize name ':cyan)
                  (if documentation
                      (subseq documentation 0
                              (position #\newline documentation))
                      ""))))))
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
           (load setup :verbose nil)
           t)
          (t
           (let ((bootstrap (concatenate 'string (config-directory)
                                         "quicklisp-bootstrap.lisp")))
             (ensure-directories-exist (config-directory))
             (when (zerop (run "curl" "-fsSL" "-o" bootstrap
                               "https://beta.quicklisp.org/quicklisp.lisp"))
               (load bootstrap :verbose nil)
               (funcall (find-symbol "INSTALL" '#:quicklisp-quickstart))
               t))))))
