(asdf:defsystem :platform
  :description ""
  :version "0.0.1"
  :author ""
  :licence "MIT"
  :depends-on ("bordeaux-threads" "usocket")
  :serial T
  :components ((:file "package")
               (:module thread
                        :components ((:file "thread-bordeaux")
                                     (:file "threaded-object-macros")
                                     (:file "threaded-object")))
               (:module connection
                        :components ((:file "connection")
                                     (:file "connection-manager")
                                     (:file "tcp-connection")))))