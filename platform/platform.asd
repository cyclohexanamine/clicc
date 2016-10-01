(asdf:defsystem :platform
  :description ""
  :version "0.0.1"
  :author ""
  :licence "MIT"
  :depends-on ("bordeaux-threads" "usocket")
  :serial T
  :components ((:file "package")
               (:file "global-macros")
               (:module thread
                        :components ((:module backend
                                             :components ((:file "thread-bordeaux")))
                                     (:file "threaded-object-macros")
                                     (:file "threaded-object")))
               (:module connection
                        :components ((:file "connection")
                                     (:file "connection-manager")
                                     (:module tcp-usocket
                                              :components ((:file "tcp-connection")
                                                           (:file "tcp-connection-manager")))))
               (:module message
                        :components ((:file "message-manager")))))