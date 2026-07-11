;;;; -- Build script --
;;;
;;; Loads the cclsh system into a fresh CCL, bakes in Quicklisp when
;;; the user has one, and saves a standalone executable. Run through
;;; scripts/build.

(require :asdf)

(asdf:load-asd (truename "cclsh.asd"))
(asdf:load-system "cclsh")

(let ((setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (if (probe-file setup)
      (progn
        (format t "Including Quicklisp from ~a~%" setup)
        (load setup :verbose nil))
      (format t "No ~a found, building without Quicklisp.~%~
                 Run (quicklisp-setup) in the shell to install it later.~%"
              setup)))

(setf ccl:*terminal-character-encoding-name* ':utf-8)

(format t "Saving cclsh executable...~%")
(ccl:save-application "cclsh"
                      :toplevel-function #'cclsh:shell-toplevel
                      :prepend-kernel t)
