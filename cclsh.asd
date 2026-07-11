(defsystem "cclsh"
  :version "1.0.0"
  :author "Lukáš Hozda"
  :license "Private"
  :depends-on ()
  :components ((:module "source"
                :components
                ((:file "package")
                 (:file "environment" :depends-on ("package"))
                 (:file "terminal" :depends-on ("package"))
                 (:file "lexer"    :depends-on ("package"))
                 (:file "command"  :depends-on ("package" "environment"))
                 (:file "expand"   :depends-on ("lexer" "command" "environment"))
                 (:file "highlight" :depends-on ("terminal" "lexer" "command" "expand"))
                 (:file "history"  :depends-on ("lexer" "environment"))
                 (:file "prompt"   :depends-on ("terminal" "command" "expand" "environment"))
                 (:file "pipeline" :depends-on ("command"))
                 (:file "complete" :depends-on ("lexer" "command" "expand" "highlight"))
                 (:file "builtins" :depends-on ("command" "expand" "terminal" "history" "complete"))
                 (:file "line-editor" :depends-on ("terminal" "lexer" "highlight" "history" "prompt" "complete"))
                 (:file "dispatch" :depends-on ("command" "expand" "lexer" "highlight" "complete"))
                 (:file "main"     :depends-on ("dispatch" "line-editor" "prompt" "builtins" "pipeline")))))
  :description "A system shell running inside Clozure CL")
