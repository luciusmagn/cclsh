;;;; -- Build script --
;;;
;;; Loads Quicklisp and the cclsh system into a fresh CCL, then saves a
;;; standalone executable. Run through scripts/build.

;; These defaults must be established before loading ASDF, Quicklisp,
;; or project source. In particular, CCL's ordinary GETENV decodes raw
;; environment bytes as Latin-1 on Linux, so the Quicklisp override is
;; read through libc and decoded explicitly below.
(setf ccl:*default-file-character-encoding* ':utf-8
      ccl:*default-external-format*
      '(:character-encoding :utf-8 :line-termination :unix)
      ccl:*terminal-character-encoding-name* ':utf-8)

(defun build-getenv-utf-8 (name)
  "The UTF-8 value of environment variable NAME, or NIL."
  (ccl::with-utf-8-cstr (encoded-name name)
    (let ((value (ccl:external-call "getenv"
                                    :address encoded-name
                                    :address)))
      (unless (ccl:%null-ptr-p value)
        (ccl::%get-utf-8-cstring value)))))

(require :asdf)

(defun build-quicklisp-setup-pathname ()
  "The Quicklisp setup file required by saved cclsh images."
  (let ((override (build-getenv-utf-8 "CCLSH_QUICKLISP_SETUP")))
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
        (load setup :verbose nil :external-format ':utf-8)
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

(defun build-load-cclsh ()
  "Load cclsh from this checkout, ignoring inherited ASDF registries."
  (let* ((root (truename "./"))
         (asd  (truename "cclsh.asd")))
    (asdf:initialize-source-registry
     `(:source-registry
       (:directory ,root)
       :ignore-inherited-configuration))
    (asdf:load-asd asd)
    (let ((loaded (truename
                   (asdf:system-source-file
                    (asdf:find-system "cclsh")))))
      (unless (equal asd loaded)
        (build-fail "ASDF selected ~a instead of ~a" loaded asd)))
    (asdf:load-system "cclsh")))

(build-load-cclsh)

(defun build-git-output (arguments)
  "Trimmed output of git ARGUMENTS, or NIL when git fails."
  (handler-case
      (let* ((output  (make-string-output-stream))
             (process (ccl:run-program "git" arguments
                                       :input  nil
                                       :output output
                                       :error  nil
                                       :wait   t
                                       :external-format ':utf-8)))
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

;; Keep the heap image separate from the kernel. On current Linux,
;; CCL 1.13's prepended-kernel image can intermittently resume through
;; an invalid rt_sigreturn frame. The ordinary adjacent-image startup
;; path does not have that failure. scripts/build atomically installs
;; the matched kernel and image after both files exist.
(format t "Copying the CCL kernel...~%")
(uiop:copy-file (truename "/proc/self/exe") "cclsh.new")
(format t "Saving cclsh image...~%")
(ccl:save-application "cclsh.image.new"
                      :toplevel-function #'cclsh:shell-toplevel
                      :prepend-kernel nil)
