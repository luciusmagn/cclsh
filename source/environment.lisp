;;;; -- Environment variables --
;;;
;;; Lispy access to the process environment. Names are designators:
;;; (getenv 'editor), (getenv :editor) and (getenv "EDITOR") all name
;;; EDITOR since the reader upcases symbols, and ENV is a setf-able
;;; accessor. Lowercase variable names like http_proxy need strings.

(in-package #:cclsh)

(defun environment-name (designator)
  "Normalize an environment variable DESIGNATOR to a name string.
   Symbols and keywords use their symbol name."
  (etypecase designator
    (symbol (symbol-name designator))
    (string designator)))

(defun getenv (name)
  "Value of the environment variable NAME (symbol or string), or NIL."
  (ccl:getenv (environment-name name)))

(defun setenv (name value)
  "Set the environment variable NAME (symbol or string) to VALUE.
   Non-string values are stringified with PRINC-TO-STRING. Returns
   VALUE."
  (ccl:setenv (environment-name name)
              (if (stringp value)
                  value
                  (princ-to-string value)))
  value)

(defun env (name)
  "Setf-able accessor for the environment variable NAME."
  (getenv name))

(defun (setf env) (value name)
  "Set the environment variable NAME to VALUE."
  (setenv name value))

(defun environment-variables ()
  "Return the current environment as a sorted list of NAME=value
   strings, read live from libc's environ."
  (let ((environ (ccl:%get-ptr (ccl:foreign-symbol-address "environ")))
        (entries nil))
    (loop for index from 0
          for entry = (ccl:%get-ptr environ (* index 8))
          until (ccl:%null-ptr-p entry)
          do (push (ccl:%get-cstring entry) entries))
    (sort entries #'string<)))
