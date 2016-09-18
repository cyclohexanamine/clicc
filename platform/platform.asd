(asdf:defsystem :platform
  :description ""
  :version "0.0.1"
  :author ""
  :licence "MIT"
  :depends-on ("bordeaux-threads")
  :serial T
  :components ((:file "package")
               (:module thread
                        :components ((:file "thread-bordeaux")
                                     (:file "threaded-object")))
               (:module connection
                        :components ((:file "connection")
                                     (:file "tcp-connection")))))