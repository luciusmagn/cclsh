(defsystem "cclsh"
  :version "0.1.0"
  :author "Lukáš Hozda"
  :license "Private"
  :depends-on ()
  :components ((:module "source"
                :components
                ((:file "package")
                 (:file "terminal" :depends-on ("package"))
                 (:file "lexer"    :depends-on ("package"))
                 (:file "command"  :depends-on ("package"))
                 (:file "expand"   :depends-on ("lexer" "command"))
                 (:file "highlight" :depends-on ("terminal" "lexer" "command" "expand"))
                 (:file "history"  :depends-on ("lexer"))
                 (:file "prompt"   :depends-on ("terminal" "command" "expand"))
                 (:file "pipeline" :depends-on ("command"))
                 (:file "builtins" :depends-on ("command" "expand" "terminal" "history"))
                 (:file "line-editor" :depends-on ("terminal" "lexer" "highlight" "history" "prompt"))
                 (:file "dispatch" :depends-on ("command" "expand" "lexer" "highlight"))
                 (:file "main"     :depends-on ("dispatch" "line-editor" "prompt" "builtins" "pipeline")))))
  :description "A system shell running inside Clozure CL")
