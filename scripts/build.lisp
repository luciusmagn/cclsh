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

(defpackage #:cclsh-build
  (:use #:cl))

(in-package #:cclsh-build)

(defun build-getenv-utf-8 (name)
  "The UTF-8 value of environment variable NAME, or NIL."
  (ccl::with-utf-8-cstr (encoded-name name)
    (let ((value (ccl:external-call "getenv"
                                    :address encoded-name
                                    :address)))
      (unless (ccl:%null-ptr-p value)
        (ccl::%get-utf-8-cstring value)))))

(require :asdf)

(defparameter *build-quicklisp-root-files*
  '("setup.lisp" "asdf.lisp" "client-info.sexp")
  "Root Quicklisp files allowed into the sanitized build template.")

(defparameter *build-quicklisp-client-files*
  '("bundle-template.lisp"
    "bundle.lisp"
    "cdb.lisp"
    "client-info.lisp"
    "client-update.lisp"
    "client.lisp"
    "config.lisp"
    "deflate.lisp"
    "dist-update.lisp"
    "dist.lisp"
    "fetch-gzipped.lisp"
    "http.lisp"
    "impl-util.lisp"
    "impl.lisp"
    "local-projects.lisp"
    "minitar.lisp"
    "misc.lisp"
    "network.lisp"
    "package.lisp"
    "progress.lisp"
    "quicklisp.asd"
    "setup.lisp"
    "utils.lisp"
    "version.txt")
  "Quicklisp client files allowed into the sanitized build template.")

(defparameter *build-quicklisp-dist-files*
  '("distinfo.txt" "enabled.txt" "releases.txt" "systems.txt")
  "Quicklisp distribution files allowed into the sanitized build template.")

(defvar *build-quicklisp-home* nil
  "Temporary sanitized Quicklisp home used by this build.")

(defun build-quicklisp-source-setup-pathname ()
  "The source Quicklisp setup file used to create a sanitized template."
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

(defun build-quicklisp-relative-files ()
  "Return the strict allowlist of Quicklisp template files."
  (append
   *build-quicklisp-root-files*
   (mapcar (lambda (name) (concatenate 'string "quicklisp/" name))
           *build-quicklisp-client-files*)
   (mapcar (lambda (name)
             (concatenate 'string "dists/quicklisp/" name))
           *build-quicklisp-dist-files*)))

(defun build-materialize-quicklisp-template ()
  "Copy sanitized Quicklisp metadata into a private temporary home."
  (let* ((setup  (build-quicklisp-source-setup-pathname))
         (source (uiop:pathname-directory-pathname setup))
         (target
           (merge-pathnames
            (format nil ".cclsh-quicklisp-build.~d/" (ccl::getpid))
            (truename "./"))))
    (unless (probe-file setup)
      (build-fail
       "Quicklisp is required, but ~a does not exist.~%~
        Install it there, or set CCLSH_QUICKLISP_SETUP to its setup.lisp."
       setup))
    (when (probe-file target)
      (uiop:delete-directory-tree target
                                  :validate t
                                  :if-does-not-exist ':ignore))
    (dolist (relative (build-quicklisp-relative-files))
      (let ((source-file (merge-pathnames relative source))
            (target-file (merge-pathnames relative target)))
        (unless (probe-file source-file)
          (build-fail "Quicklisp template input is missing: ~a"
                      source-file))
        (ensure-directories-exist target-file)
        (uiop:copy-file source-file target-file)))
    (ensure-directories-exist (merge-pathnames "local-projects/" target))
    (setf *build-quicklisp-home* target)
    target))

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
        (let ((commits '())
              (prefix  "clinedi="))
          (loop for line = (read-line stream nil nil)
                while line
                when (and (<= (length prefix) (length line))
                          (string= prefix line :end2 (length prefix)))
                  do (push (subseq line (length prefix)) commits))
          (let ((commit (and (null (rest commits)) (first commits))))
            (unless (and commit
                         (= (length commit) 40)
                         (every (lambda (character)
                                  (and (digit-char-p character 16)
                                       (not (find character "ABCDEF"))))
                                commit))
              (build-fail
               "dependencies.lock must contain exactly one clinedi=<40-character lowercase Git commit> line"))
            commit)))
    (error (condition)
      (build-fail "could not read dependencies.lock: ~a" condition))))

(defun build-load-quicklisp ()
  "Load sanitized Quicklisp metadata and verify its public entry point."
  (let* ((home  (build-materialize-quicklisp-template))
         (setup (merge-pathnames "setup.lisp" home)))
    (format t "Including sanitized Quicklisp metadata~%")
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

(defun build-initialize-source-registry (clinedi-asd)
  "Expose only this checkout and CLINEDI-ASD to ASDF."
  (let ((root              (truename "./"))
        (clinedi-directory
          (uiop:pathname-directory-pathname clinedi-asd)))
    (asdf:initialize-source-registry
     `(:source-registry
       (:directory ,root)
       (:directory ,clinedi-directory)
       :ignore-inherited-configuration)))
  (values))

(defun build-clinedi-asd-pathname ()
  "Return the explicit sibling or overridden Clinedi ASD pathname."
  (let* ((override (build-getenv-utf-8 "CCLSH_CLINEDI_SOURCE"))
         (directory
           (if (and override (plusp (length override)))
               (uiop:ensure-directory-pathname (pathname override))
               (merge-pathnames "../clinedi/" (truename "./"))))
         (asd (merge-pathnames "clinedi.asd" directory)))
    (unless (probe-file asd)
      (build-fail
       "Clinedi checkout is missing at ~a.~%Set CCLSH_CLINEDI_SOURCE to its repository directory."
       directory))
    (truename asd)))

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
        (build-fail "selected ~a instead of repository-root ~a"
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
  "Load the exact clean Clinedi revision selected for this build."
  (let* ((expected-commit (build-clinedi-lock-commit))
         (asd             (build-clinedi-asd-pathname))
         (identity        (build-clinedi-identity asd expected-commit)))
    (build-initialize-source-registry asd)
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
    (asdf:load-system "cclsh" :force t)))

(build-load-cclsh)
(build-verify-clinedi-identity)

(defun build-read-file-octets (file)
  "Read FILE into a simple octet vector."
  (with-open-file (stream file
                          :direction ':input
                          :element-type '(unsigned-byte 8))
    (let* ((length   (file-length stream))
           (contents (make-array length
                                 :element-type '(unsigned-byte 8))))
      (unless (= length (read-sequence contents stream))
        (build-fail "could not read complete Quicklisp template file: ~a"
                    file))
      contents)))

(defun build-quicklisp-template-files ()
  "Return a sanitized Quicklisp template for embedding in the image."
  (let ((home (uiop:ensure-directory-pathname ql-setup:*quicklisp-home*)))
    (mapcar
     (lambda (relative)
       (let ((file (merge-pathnames relative home)))
         (unless (probe-file file)
           (build-fail "Quicklisp template input is missing: ~a" file))
         (list relative (build-read-file-octets file))))
     (build-quicklisp-relative-files))))

(defun build-git-commit ()
  "The short commit of the checkout, with a -dirty marker when the
   working tree has uncommitted changes. A packager may provide the
   immutable source identity through CCLSH_BUILD_COMMIT."
  (let ((packaged (build-getenv-utf-8 "CCLSH_BUILD_COMMIT")))
    (if (and packaged (plusp (length packaged)))
        packaged
        (let* ((root   (truename "./"))
               (commit (build-git-output
                        root '("rev-parse" "--short" "HEAD"))))
          (when (and commit (plusp (length commit)))
            (if (plusp (length (or (build-git-output
                                    root '("status" "--porcelain")) "")))
                (concatenate 'string commit "-dirty")
                commit))))))

(let* ((commit          (build-git-commit))
       (clinedi-commit  (getf *build-clinedi-identity* ':commit))
       (quicklisp-template
         (build-getenv-utf-8 "CCLSH_PACKAGED_QUICKLISP_TEMPLATE"))
       (quicklisp-files
         (unless (and quicklisp-template
                      (plusp (length quicklisp-template)))
           (build-quicklisp-template-files))))
  (setf cclsh:*cclsh-build-commit*          commit
        cclsh:*cclsh-build-clinedi-commit* clinedi-commit
        cclsh::*packaged-quicklisp-template*
        (and quicklisp-template
             (plusp (length quicklisp-template))
             quicklisp-template)
        cclsh::*packaged-quicklisp-files*           quicklisp-files
        cclsh::*packaged-quicklisp-home*            nil
        cclsh::*packaged-quicklisp-user-setup-home* nil
        cclsh::*packaged-quicklisp-last-error*      nil)
  (format t "Build commit: ~a~%" (or commit "unknown")))

;; Keep the heap image separate from the kernel so installers can activate
;; a matched, content-addressed pair atomically. scripts/build installs the
;; local pair only after both files exist and the saved image validates.
(build-verify-clinedi-identity)
(dolist (system '("cclsh" "clinedi" "clinedi/tests" "quicklisp"))
  (asdf:clear-system system))
(format t "Copying the CCL kernel...~%")
(uiop:copy-file (truename "/proc/self/exe") "cclsh.new")
(setf *build-clinedi-identity* nil)
(when *build-quicklisp-home*
  (uiop:delete-directory-tree *build-quicklisp-home*
                              :validate t
                              :if-does-not-exist ':ignore)
  (setf *build-quicklisp-home* nil))

(in-package #:cl-user)

(delete-package '#:cclsh-build)
(ccl:gc)
(format t "Saving cclsh image...~%")
(ccl:save-application "cclsh.image.new"
                      :toplevel-function #'cclsh:shell-toplevel
                      :prepend-kernel nil
                      :mode #o600)
