;;;; Example cclsh startup file
;;;
;;; Copy this to ~/.config/cclsh/startup.lisp and customize it. cclsh does
;;; not source /etc/profile or another shell's startup files, so establish
;;; the environment needed by login sessions here. This example contains no
;;; host-specific paths, private services or credentials.

(in-package #:cclsh-user)

(defun startup--set-default (name value)
  "Set environment variable NAME to VALUE when it is empty or absent."
  (let ((current (getenv name)))
    (unless (and current (plusp (length current)))
      (setenv name value))))

(defun startup--prepend-path (&rest directories)
  "Prepend DIRECTORIES to PATH while retaining the first occurrence."
  (let* ((current
           (uiop:split-string (or (getenv "PATH") "")
                              :separator '(#\:)))
         (paths
           (remove-duplicates
            (remove "" (append directories current) :test #'string=)
            :test     #'string=
            :from-end t)))
    (setenv "PATH" (format nil "~{~a~^:~}" paths))))

(let ((home
        (string-right-trim
         "/" (or (getenv "HOME")
                  (namestring (user-homedir-pathname))))))
  (startup--set-default "XDG_CONFIG_HOME"
                        (format nil "~a/.config" home))
  (startup--set-default "XDG_DATA_HOME"
                        (format nil "~a/.local/share" home))
  (startup--set-default "XDG_STATE_HOME"
                        (format nil "~a/.local/state" home))
  (startup--set-default "XDG_CACHE_HOME"
                        (format nil "~a/.cache" home))
  (startup--prepend-path
   (format nil "~a/.local/bin" home)
   (format nil "~a/.cargo/bin" home)
   (format nil "~a/.bun/bin" home)
   "/usr/local/bin"
   "/usr/bin"
   "/bin"))

(startup--set-default "EDITOR" "vi")
(startup--set-default "VISUAL" (getenv "EDITOR"))

(defcommand la ()
  "List all files in long form."
  (run "ls" "-la"))

(defcommand gd ()
  "Show the current Git diff."
  (run "git" "diff"))

;; Zoxide is optional. After installing it, uncomment this to record every
;; successful cd and install the z and zi commands.
;; (zoxide-setup)
