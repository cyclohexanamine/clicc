(defpackage :thread
  (:use :cl)
  (:export :make-thread :current-thread :threadp :thread-name
           :make-lock :acquire-lock :release-lock :with-lock-held
           :threaded-object :defslotinterface :defclass-threaded :with-slot-lock))


(defpackage :connection
  (:use :cl)
  (:export :connection
           :read-status :modify-status
           :read-data :modify-data
           :send-message
           :set-handler))