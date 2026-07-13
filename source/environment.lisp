;;;; -- Environment variables --
;;;
;;; Lispy access to the process environment. Names are designators:
;;; (getenv 'editor), (getenv :editor) and (getenv "EDITOR") all name
;;; EDITOR since the reader upcases symbols, and ENV is a setf-able
;;; accessor. Lowercase variable names like http_proxy need strings.

(in-package #:cclsh)

(define-condition environment-error (error)
  ((operation
    :initarg :operation
    :reader environment-error-operation)
   (name
    :initarg :name
    :reader environment-error-name)
   (code
    :initarg :code
    :reader environment-error-code))
  (:documentation "Signaled when libc rejects an environment operation.")
  (:report
   (lambda (condition stream)
     (format stream "cannot ~a environment variable ~a~@[: ~a~]"
             (environment-error-operation condition)
             (environment-error-name condition)
             (let ((code (environment-error-code condition)))
               (when code
                 (let ((pointer
                         (external-call "strerror" :int code :address)))
                   (if (ccl:%null-ptr-p pointer)
                       (format nil "system error ~d" code)
                       (ccl::%get-utf-8-cstring pointer)))))))))

(defvar *environment-lock* (ccl:make-lock "cclsh environment")
  "Serializes libc environment reads and mutations.")

(defun environment-name (designator)
  "Normalize an environment variable DESIGNATOR to a name string.
   Symbols and keywords use their symbol name."
  (etypecase designator
    (symbol (symbol-name designator))
    (string designator)))

(defun getenv (name)
  "Value of the environment variable NAME (symbol or string), or NIL."
  (let ((name (environment-name name)))
    (ccl:with-lock-grabbed (*environment-lock*)
      (ccl::with-utf-8-cstr (encoded name)
        (let ((value (external-call "getenv" :address encoded :address)))
          (unless (ccl:%null-ptr-p value)
            (ccl::%get-utf-8-cstring value)))))))

(defun setenv (name value)
  "Set the environment variable NAME (symbol or string) to VALUE.
   Non-string values are stringified with PRINC-TO-STRING. Returns
   VALUE."
  (let ((name (environment-name name))
        (text (if (stringp value)
                  value
                  (princ-to-string value))))
    (ccl:with-lock-grabbed (*environment-lock*)
      (ccl::with-utf-8-cstr (encoded-name name)
        (ccl::with-utf-8-cstr (encoded-text text)
          (unless (zerop (external-call "setenv"
                                        :address encoded-name
                                        :address encoded-text
                                        :int 1
                                        :int))
            (error 'environment-error
                   :operation "set"
                   :name name
                   :code (ccl::get-errno)))))))
  value)

(defun unsetenv (name)
  "Remove the environment variable NAME and return no values."
  (let ((name (environment-name name)))
    (ccl:with-lock-grabbed (*environment-lock*)
      (ccl::with-utf-8-cstr (encoded name)
        (unless (zerop (external-call "unsetenv" :address encoded :int))
          (error 'environment-error
                 :operation "unset"
                 :name name
                 :code (ccl::get-errno))))))
  (values))

(defun env (name)
  "Setf-able accessor for the environment variable NAME."
  (getenv name))

(defun (setf env) (value name)
  "Set the environment variable NAME to VALUE."
  (setenv name value))

(defun environment-variables ()
  "Return the current environment as a sorted list of NAME=value
   strings, read live from libc's environ."
  (ccl:with-lock-grabbed (*environment-lock*)
    (let ((environ
            (ccl:%get-ptr (ccl:foreign-symbol-address "environ")))
          (entries nil))
      (loop for index from 0
            for entry = (ccl:%get-ptr environ (* index 8))
            until (ccl:%null-ptr-p entry)
            do (push (ccl::%get-utf-8-cstring entry) entries))
      (sort entries #'string<))))
