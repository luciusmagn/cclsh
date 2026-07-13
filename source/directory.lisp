;;;; -- Directory navigation --
;;;
;;; Working-directory changes, post-change hooks and optional zoxide
;;; integration. Hooks observe committed PWD and OLDPWD state, so every
;;; successful explicit, implicit or Lisp-driven cd follows one path.

(in-package #:cclsh)

(defvar *directory-change-hooks* nil
  "Function designators called with the old and new directory names after a
successful directory change. Register hooks with DIRECTORY-CHANGE-HOOK-ADD so
duplicate registrations are avoided.")

(defvar *directory-change-hooks-running-p* nil
  "True while directory-change hooks are running, preventing recursive hooks.")

(defvar *zoxide-program* nil
  "Exact zoxide executable selected by ZOXIDE-SETUP, or NIL before setup.")

(defvar *zoxide-z-command* nil
  "Command instance installed for Z, or NIL when zoxide is disabled.")

(defvar *zoxide-zi-command* nil
  "Command instance installed for ZI, or NIL when zoxide is disabled.")


;;; Directory changes

(defun directory-namestring-clean (pathname)
  "Namestring of PATHNAME without a trailing slash, except for /."
  (let ((name (namestring pathname)))
    (if (and (> (length name) 1)
             (char= (char name (1- (length name))) #\/))
        (subseq name 0 (1- (length name)))
        name)))

(defun directory-change-hook-add (function)
  "Register FUNCTION to receive old and new directory names after cd.
FUNCTION may be a function object or a symbol naming a function. Prefer a
symbol for a named hook so redefinition and removal retain stable identity.
Returns FUNCTION."
  (check-type function (or symbol function))
  (pushnew function *directory-change-hooks* :test #'eq)
  function)

(defun directory-change-hook-remove (function)
  "Remove FUNCTION from the directory-change hooks and return it."
  (setf *directory-change-hooks*
        (delete function *directory-change-hooks* :test #'eq))
  function)

(defun directory-change-hooks-run (old-directory new-directory)
  "Run a snapshot of the directory hooks after a real directory change.
Hook failures are reported without rolling back cd or skipping later hooks."
  (when (and (not *directory-change-hooks-running-p*)
             (not (string= old-directory new-directory)))
    (let ((*directory-change-hooks-running-p* t))
      (dolist (hook (reverse (copy-list *directory-change-hooks*)))
        (handler-case
            (funcall hook old-directory new-directory)
          (serious-condition (condition)
            (format *error-output* "~a~%"
                    (terminal-colorize
                     (format nil "directory change hook ~s failed: ~a"
                             hook condition)
                     ':red))
            (force-output *error-output*))))))
  (values))

(defcommand cd (&optional target)
  "Change the working directory. Without TARGET goes home, with - goes
   back to the previous directory. Keeps PWD and OLDPWD updated and runs
   registered directory-change hooks after a successful change."
  (when *directory-change-hooks-running-p*
    (format *error-output* "~a~%"
            (terminal-colorize
             "cd: cannot change directory from a directory-change hook"
             ':red))
    (force-output *error-output*)
    (return-from cd 1))
  (let* ((old         (directory-namestring-clean (current-directory)))
         (back        (equal target "-"))
         (destination (cond ((or (null target) (equal target ""))
                             (home-directory))
                            (back
                             (getenv "OLDPWD"))
                            (t
                             (tilde-expand target)))))
    (when (null destination)
      (format *error-output* "~a~%"
              (terminal-colorize "cd: OLDPWD not set" ':red))
      (return-from cd 1))
    (handler-case
        (setf (current-directory) destination)
      (error ()
        (format *error-output* "~a~%"
                (terminal-colorize
                 (format nil "cd: cannot change to ~a" destination)
                 ':red))
        (return-from cd 1)))
    (let ((new (directory-namestring-clean (current-directory))))
      (setf *default-pathname-defaults* (current-directory))
      (setenv "OLDPWD" old)
      (setenv "PWD" new)
      (directory-change-hooks-run old new)
      (when back
        (format t "~a~%" new)))
    0))


;;; Zoxide

(defun zoxide--resolve-program ()
  "Return a fresh lookup of the external zoxide executable in PATH."
  (path--search-uncached "zoxide"))

(defun zoxide--process-status (process)
  "Return conventional integer status for a completed CCL PROCESS."
  (multiple-value-bind (state code)
      (external-process-status process)
    (case state
      (:exited
       (or code 1))
      (:signaled
       (+ 128 (or code 0)))
      (t
       125))))

(defun zoxide--add-directory (directory &optional (program *zoxide-program*))
  "Tell PROGRAM, defaulting to the configured zoxide, to record DIRECTORY."
  (if (null program)
      127
      (handler-case
          (zoxide--process-status
           (run-program program (list "add" "--" directory)
                        :input           nil
                        :output          nil
                        :error           *error-output*
                        :wait            t
                        :external-format ':utf-8))
        (serious-condition ()
          126))))

(defun zoxide--report-add-failure (directory status)
  "Report that zoxide could not record DIRECTORY with integer STATUS."
  (format *error-output* "~a~%"
          (terminal-colorize
           (format nil "zoxide: could not record ~a, status ~d"
                   directory status)
           ':red))
  (force-output *error-output*)
  (values))

(defun zoxide--directory-change (old-directory new-directory)
  "Directory-change hook that records NEW-DIRECTORY with zoxide."
  (declare (ignore old-directory))
  (let ((status (zoxide--add-directory new-directory)))
    (unless (zerop status)
      (zoxide--report-add-failure new-directory status)))
  (values))

(defun zoxide--install-command (symbol)
  "Install SYMBOL's function as a command with its function documentation."
  (let ((command
          (make-instance 'command
                         :name          symbol
                         :function      (symbol-function symbol)
                         :documentation (documentation symbol 'function))))
    (setf (symbol-value symbol) command)
    command))

(defun zoxide--remove-command (symbol command)
  "Unbind SYMBOL only when its value is the zoxide COMMAND instance."
  (when (and command
             (boundp symbol)
             (eq (symbol-value symbol) command))
    (makunbound symbol))
  (values))

(defun zoxide--disable ()
  "Remove zoxide's hook and command bindings without disturbing replacements."
  (directory-change-hook-remove 'zoxide--directory-change)
  (zoxide--remove-command 'z *zoxide-z-command*)
  (zoxide--remove-command 'zi *zoxide-zi-command*)
  (setf *zoxide-program*    nil
        *zoxide-z-command*  nil
        *zoxide-zi-command* nil)
  (values))

(defun zoxide--configured-p (program)
  "True when PROGRAM and all zoxide-owned shell state are installed."
  (and (equal program *zoxide-program*)
       (member 'zoxide--directory-change
               *directory-change-hooks*
               :test #'eq)
       *zoxide-z-command*
       (boundp 'z)
       (eq (symbol-value 'z) *zoxide-z-command*)
       *zoxide-zi-command*
       (boundp 'zi)
       (eq (symbol-value 'zi) *zoxide-zi-command*)
       t))

(defun zoxide--activate (program)
  "Install a successfully tested PROGRAM, directory hook, Z and ZI."
  (setf *zoxide-program*    program
        *zoxide-z-command*  (zoxide--install-command 'z)
        *zoxide-zi-command* (zoxide--install-command 'zi))
  (directory-change-hook-add 'zoxide--directory-change)
  (values))

(defun zoxide--query (arguments &key interactive)
  "Query zoxide with ARGUMENTS and change to its selected directory."
  (let* ((program
           (or *zoxide-program*
               (error 'command-not-found-error :name "zoxide")))
         (current
           (directory-namestring-clean (current-directory)))
         (options
           (if interactive
               (list "query" "--interactive" "--")
               (list "query" "--exclude" current "--"))))
    (multiple-value-bind (directory status)
        (pipeline-capture
         (list (cons program
                     (append options
                             (mapcar #'princ-to-string arguments)))))
      (cond ((not (zerop status))
             status)
            ((zerop (length directory))
             (format *error-output* "~a~%"
                     (terminal-colorize
                      "zoxide: query returned no directory" ':red))
             (force-output *error-output*)
             1)
            (t
             (cd directory))))))

(defun z (&rest arguments)
  "Change directory using a path or zoxide keywords. With no arguments goes
home; - goes to OLDPWD; an existing directory is used directly."
  (let ((words (mapcar #'princ-to-string arguments)))
    (cond ((null words)
           (cd))
          ((and (null (rest words))
                (string= (first words) "-"))
           (cd "-"))
          ((and (null (rest words))
                (directory-exists-p (tilde-expand (first words))))
           (cd (first words)))
          ((and (= (length words) 2)
                (string= (first words) "--"))
           (cd (second words)))
          (t
           (zoxide--query words)))))

(defun zi (&rest arguments)
  "Interactively select a zoxide directory, optionally seeded by ARGUMENTS."
  (zoxide--query arguments :interactive t))

(defcommand zoxide-setup ()
  "Enable zoxide directory tracking and install the z and zi commands.
   Safe to call repeatedly, normally once from startup.lisp."
  (let ((program (zoxide--resolve-program)))
    (cond ((null program)
           (zoxide--disable)
           (format *error-output* "~a~%"
                   (terminal-colorize "zoxide: executable not found" ':red))
           (force-output *error-output*)
           127)
          ((zoxide--configured-p program)
           0)
          (t
           (let* ((directory
                    (directory-namestring-clean (current-directory)))
                  (status
                    (zoxide--add-directory directory program)))
             (cond ((zerop status)
                    (zoxide--disable)
                    (zoxide--activate program)
                    0)
                   (t
                    (zoxide--report-add-failure directory status)
                    (unless (and *zoxide-program*
                                 (ignore-errors
                                   (probe-file *zoxide-program*)))
                      (zoxide--disable))
                    status)))))))
