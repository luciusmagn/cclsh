(defsystem "cclsh"
  :version "0.1.0"
  :author "Lukáš Hozda"
  :license "Private"
  :depends-on ()
  :components ((:module "source"
                :components
                ((:file "package")
                 (:file "terminal" :depends-on ("package"))
                 (:file "lexer"    :depends-on ("package")))))
  :description "A system shell running inside Clozure CL")
