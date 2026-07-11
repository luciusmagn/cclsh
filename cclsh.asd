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
                 (:file "highlight" :depends-on ("terminal" "lexer" "command" "expand")))))
  :description "A system shell running inside Clozure CL")
