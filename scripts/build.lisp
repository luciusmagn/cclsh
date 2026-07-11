;;;; -- Build script --
;;;
;;; Loads the cclsh system into a fresh CCL, bakes in Quicklisp when
;;; the user has one, and saves a standalone executable. Run through
;;; scripts/build.

(require :asdf)

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

(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (if (probe-file setup)
      (progn
        (format t "Including Quicklisp from ~a~%" setup)
        (load setup :verbose nil))
      (format t "No ~a found, building without Quicklisp.~%~
                 Run (quicklisp-setup) in the shell to install it later.~%"
              setup)))

(setf ccl:*terminal-character-encoding-name* ':utf-8)

;; Save under a temporary name; scripts/build renames it into place so
;; an interrupted build can never truncate a binary that might be
;; someone's login shell.
(format t "Saving cclsh executable...~%")
(ccl:save-application "cclsh.new"
                      :toplevel-function #'cclsh:shell-toplevel
                      :prepend-kernel t)
