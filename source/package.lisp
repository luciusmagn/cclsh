;;;; -- Package definitions --

(defpackage #:cclsh
  (:use #:cl)
  (:import-from #:ccl
                #:run-program
                #:external-process-status
                #:external-process-input-stream
                #:external-process-output-stream
                #:getenv
                #:setenv
                #:current-directory
                #:quit
                #:process-run-function
                #:make-external-format
                #:*break-hook*)
  (:export #:main
           #:shell-toplevel
           #:command
           #:command-name
           #:command-function
           #:command-documentation
           #:defcommand
           #:run
           #:pipe
           #:seq
           #:all
           #:any
           #:*last-status*
           #:cd
           #:exit
           #:rehash
           #:commands
           #:config-directory
           #:quicklisp-setup
           #:shell-error
           #:command-not-found-error)
  (:documentation "A system shell running inside Clozure CL."))

(defpackage #:cclsh-user
  (:use #:cl #:ccl #:cclsh)
  (:shadowing-import-from #:cclsh
                          #:run
                          #:pipe
                          #:seq
                          #:all
                          #:any
                          #:cd
                          #:exit
                          #:rehash
                          #:commands
                          #:command
                          #:defcommand)
  (:documentation "The package command lines and startup.lisp are evaluated in."))
