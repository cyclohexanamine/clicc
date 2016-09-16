(asdf:defsystem :clicc
  :description "CLICC: Common Lisp ICC."
  :version "0.0.1"
  :author ""
  :licence "MIT"
  :depends-on ("quicklisp" "usocket" "bordeaux-threads")
  :components ((:file "package")
               (:file "server" :depends-on ("package"))
               (:file "client" :depends-on ("server"))))