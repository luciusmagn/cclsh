;;;; -- Build script --
;;;
;;; Loads Quicklisp and the cclsh system into a fresh CCL, then saves a
;;; standalone executable. Run through scripts/build.

(require :asdf)

(defun build-quicklisp-setup-pathname ()
  "The Quicklisp setup file required by saved cclsh images."
  (let ((override (ccl:getenv "CCLSH_QUICKLISP_SETUP")))
    (if (and override (plusp (length override)))
        (pathname override)
        (merge-pathnames "quicklisp/setup.lisp"
                         (user-homedir-pathname)))))

(defun build-fail (control &rest arguments)
  "Report a build error clearly and leave without saving an image."
  (format *error-output* "cclsh build: error: ~?~%" control arguments)
  (finish-output *error-output*)
  (ccl:quit 1))

(defun build-load-quicklisp ()
  "Load the required Quicklisp setup and verify its public entry point."
  (let ((setup (build-quicklisp-setup-pathname)))
    (unless (handler-case (probe-file setup)
              (error (condition)
                (build-fail "could not access Quicklisp setup ~a: ~a"
                            setup condition)))
      (build-fail
       "Quicklisp is required, but ~a does not exist.~%~
        Install it there, or set CCLSH_QUICKLISP_SETUP to its setup.lisp."
       setup))
    (format t "Including Quicklisp from ~a~%" setup)
    (finish-output)
    (handler-case
        (load setup :verbose nil)
      (error (condition)
        (build-fail "could not load Quicklisp from ~a: ~a"
                    setup condition)))
    (let* ((package (find-package "QL"))
           (quickload (and package (find-symbol "QUICKLOAD" package))))
      (unless (and quickload (fboundp quickload))
        (build-fail
         "~a loaded without providing the function QL:QUICKLOAD"
         setup)))))

(build-load-quicklisp)

(asdf:load-asd (truename "cclsh.asd"))
(asdf:load-system "cclsh")

(defun build-git-output (arguments)
  "Trimmed output of git ARGUMENTS, or NIL when git fails."
  (handler-case
      (let* ((output  (make-string-output-stream))
             (process (ccl:run-program "git" arguments
                                       :input  nil
                                       :output output
                                       :error  nil
                                       :wait   t)))
        (multiple-value-bind (status code)
            (ccl:external-process-status process)
          (when (and (eq status ':exited) (zerop code))
            (string-trim '(#\newline #\space)
                         (get-output-stream-string output)))))
    (error () nil)))

(defun build-git-commit ()
  "The short commit of the checkout, with a -dirty marker when the
   working tree has uncommitted changes. NIL outside a git checkout."
  (let ((commit (build-git-output '("rev-parse" "--short" "HEAD"))))
    (when (and commit (plusp (length commit)))
      (if (plusp (length (or (build-git-output '("status" "--porcelain")) "")))
          (concatenate 'string commit "-dirty")
          commit))))

(let ((commit (build-git-commit)))
  (setf cclsh:*cclsh-build-commit* commit)
  (format t "Build commit: ~a~%" (or commit "unknown")))

(setf ccl:*terminal-character-encoding-name* ':utf-8)

;; Save under a temporary name; scripts/build renames it into place so
;; an interrupted build can never truncate a binary that might be
;; someone's login shell.
(format t "Saving cclsh executable...~%")
(ccl:save-application "cclsh.new"
                      :toplevel-function #'cclsh:shell-toplevel
                      :prepend-kernel t)
