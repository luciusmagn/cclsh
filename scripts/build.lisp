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

(defun build-git-output (directory arguments)
  "Trimmed output of a Git command in DIRECTORY, or NIL on failure."
  (handler-case
      (let* ((output
               (make-string-output-stream))
             (process
               (ccl:run-program
                "git"
                (append (list "-C" (namestring directory)) arguments)
                :input           nil
                :output          output
                :error           nil
                :wait            t
                :external-format ':utf-8)))
        (multiple-value-bind (status code)
            (ccl:external-process-status process)
          (when (and (eq status ':exited) (zerop code))
            (string-trim '(#\newline #\return #\space #\tab)
                         (get-output-stream-string output)))))
    (error () nil)))

(defun build-clinedi-lock-commit ()
  "Return the exact Clinedi commit recorded in dependencies.lock."
  (handler-case
      (with-open-file (stream "dependencies.lock"
                              :direction       ':input
                              :external-format ':utf-8)
        (let* ((line   (read-line stream nil nil))
               (prefix "clinedi=")
               (commit
                 (and line
                      (<= (length prefix) (length line))
                      (string= prefix line :end2 (length prefix))
                      (subseq line (length prefix)))))
          (unless (and commit
                       (= (length commit) 40)
                       (every (lambda (character)
                                (and (digit-char-p character 16)
                                     (not (find character "ABCDEF"))))
                              commit)
                       (null (read-line stream nil nil)))
            (build-fail
             "dependencies.lock must contain exactly one clinedi=<40-character lowercase Git commit> line"))
          commit))
    (error (condition)
      (build-fail "could not read dependencies.lock: ~a" condition))))

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

(defvar *build-clinedi-identity* nil
  "Verified pathname, repository and commit of the loaded Clinedi system.")

(defun build-initialize-source-registry ()
  "Expose only this checkout through ASDF's ordinary source registry.
   Quicklisp's local-project searcher remains available separately."
  (let ((root (truename "./")))
    (asdf:initialize-source-registry
     `(:source-registry
       (:directory ,root)
       :ignore-inherited-configuration)))
  (values))

(defun build-local-clinedi-asd ()
  "Refresh Quicklisp local projects and return its selected clinedi.asd."
  (handler-case
      (progn
        (uiop:symbol-call '#:ql '#:register-local-projects)
        (asdf:clear-system "clinedi")
        (let ((asd
                (uiop:symbol-call '#:ql
                                  '#:local-projects-searcher
                                  "clinedi")))
          (unless asd
            (build-fail
             "Quicklisp did not find clinedi in any QL:*LOCAL-PROJECT-DIRECTORIES*.~%Add the Clinedi checkout there, for example as local-projects/clinedi."))
          (truename asd)))
    (error (condition)
      (build-fail "could not discover Clinedi through Quicklisp: ~a"
                  condition))))

(defun build-clinedi-identity (asd expected-commit)
  "Return the clean, locked Git identity containing Clinedi ASD."
  (let* ((directory
           (uiop:pathname-directory-pathname asd))
         (root-output
           (build-git-output directory '("rev-parse" "--show-toplevel")))
         (commit
           (build-git-output directory
                             '("rev-parse" "--verify" "HEAD^{commit}")))
         (status
           (build-git-output directory
                             '("status" "--porcelain"
                               "--untracked-files=all"))))
    (unless (and root-output (plusp (length root-output)))
      (build-fail "the selected Clinedi system is not in a Git checkout: ~a"
                  asd))
    (let* ((root         (truename (uiop:ensure-directory-pathname
                                    root-output)))
           (expected-asd (truename (merge-pathnames "clinedi.asd" root))))
      (unless (equal asd expected-asd)
        (build-fail "Quicklisp selected ~a instead of repository-root ~a"
                    asd expected-asd))
      (unless (and commit (string= commit expected-commit))
        (build-fail
         "Clinedi checkout ~a is at ~a, but dependencies.lock requires ~a"
         root (or commit "an unreadable commit") expected-commit))
      (unless (and (stringp status) (zerop (length status)))
        (build-fail "Clinedi checkout ~a must be clean; Git reports:~%~a"
                    root (or status "could not read repository status")))
      (list :asd asd :root root :commit commit))))

(defun build-load-clinedi ()
  "Load the exact clean Clinedi revision selected by Quicklisp."
  (let* ((expected-commit (build-clinedi-lock-commit))
         (asd             (build-local-clinedi-asd))
         (identity        (build-clinedi-identity asd expected-commit)))
    (asdf:load-asd asd)
    (let ((selected
            (truename
             (asdf:system-source-file (asdf:find-system "clinedi")))))
      (unless (equal asd selected)
        (build-fail "ASDF selected Clinedi from ~a instead of ~a"
                    selected asd)))
    ;; Do not let an older cached FASL stand in for the locked source tree.
    (asdf:load-system "clinedi" :force t)
    (setf *build-clinedi-identity* identity)
    (format t "Including Clinedi ~a from ~a~%"
            expected-commit asd)
    (finish-output))
  (values))

(defun build-verify-clinedi-identity ()
  "Require the loaded Clinedi checkout to retain its verified identity."
  (let* ((expected-commit (getf *build-clinedi-identity* ':commit))
         (asd             (getf *build-clinedi-identity* ':asd))
         (current         (build-clinedi-identity asd expected-commit))
         (selected
           (truename
            (asdf:system-source-file (asdf:find-system "clinedi")))))
    (unless (equal *build-clinedi-identity* current)
      (build-fail "Clinedi identity changed while cclsh was being built"))
    (unless (equal asd selected)
      (build-fail "ASDF's selected Clinedi changed from ~a to ~a"
                  asd selected)))
  (values))

(build-initialize-source-registry)
(build-load-clinedi)

(defun build-load-cclsh ()
  "Load cclsh from this checkout after its locked dependency."
  (let ((asd (truename "cclsh.asd")))
    (asdf:load-asd asd)
    (let ((loaded (truename
                   (asdf:system-source-file
                    (asdf:find-system "cclsh")))))
      (unless (equal asd loaded)
        (build-fail "ASDF selected ~a instead of ~a" loaded asd)))
    (asdf:load-system "cclsh")))

(build-load-cclsh)
(build-verify-clinedi-identity)

(defun build-git-commit ()
  "The short commit of the checkout, with a -dirty marker when the
   working tree has uncommitted changes. NIL outside a git checkout."
  (let* ((root   (truename "./"))
         (commit (build-git-output root
                                   '("rev-parse" "--short" "HEAD"))))
    (when (and commit (plusp (length commit)))
      (if (plusp (length (or (build-git-output
                              root '("status" "--porcelain")) "")))
          (concatenate 'string commit "-dirty")
          commit))))

(let ((commit          (build-git-commit))
      (clinedi-commit (getf *build-clinedi-identity* ':commit)))
  (setf cclsh:*cclsh-build-commit*          commit
        cclsh:*cclsh-build-clinedi-commit* clinedi-commit)
  (format t "Build commit: ~a~%" (or commit "unknown")))

;; Keep the heap image separate from the kernel. On current Linux,
;; CCL 1.13's prepended-kernel image can intermittently resume through
;; an invalid rt_sigreturn frame. The ordinary adjacent-image startup
;; path does not have that failure. scripts/build atomically installs
;; the matched kernel and image after both files exist.
(build-verify-clinedi-identity)
(format t "Copying the CCL kernel...~%")
(uiop:copy-file (truename "/proc/self/exe") "cclsh.new")
(format t "Saving cclsh image...~%")
(ccl:save-application "cclsh.image.new"
                      :toplevel-function #'cclsh:shell-toplevel
                      :prepend-kernel nil)
