(asdf:defsystem :platform
  :description ""
  :version "0.0.1"
  :author ""
  :licence "MIT"
  :depends-on ("bordeaux-threads")
  :serial T
  :components ((:file "package")
               (:file "thread-bordeaux")
               (:file "threaded-object")
               (:file "connection")))