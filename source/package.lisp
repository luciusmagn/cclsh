;;;; -- Package definitions --

(defpackage #:cclsh
  (:use #:cl)
  (:import-from #:ccl
                #:run-program
                #:external-process-status
                #:external-process-id
                #:external-process-input-stream
                #:external-process-output-stream
                #:current-directory
                #:quit
                #:process-run-function
                #:make-external-format
                #:external-call
                #:*break-hook*
                #:*command-line-argument-list*)
  (:shadow #:export)
  (:export #:main
           #:shell-toplevel
           #:*cclsh-version*
           #:*cclsh-build-commit*
           #:command
           #:command-name
           #:command-function
           #:command-documentation
           #:defcommand
           #:run
           #:cmd
           #:pipe
           #:seq
           #:all
           #:any
           #:*last-status*
           #:cd
           #:exit
           #:rehash
           #:commands
           #:help
           #:export
           #:unset
           #:getenv
           #:setenv
           #:env
           #:environment-variables
           #:config-directory
           #:quicklisp-setup
           #:shell-error
           #:command-not-found-error)
  (:documentation "A system shell running inside Clozure CL."))

(defpackage #:cclsh-user
  (:use #:cl #:ccl #:cclsh)
  (:shadowing-import-from #:cclsh
                          #:run
                          #:cmd
                          #:pipe
                          #:seq
                          #:all
                          #:any
                          #:cd
                          #:exit
                          #:rehash
                          #:commands
                          #:help
                          #:export
                          #:unset
                          #:getenv
                          #:setenv
                          #:command
                          #:defcommand)
  (:documentation "The package command lines and startup.lisp are evaluated in."))
