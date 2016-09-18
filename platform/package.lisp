(defpackage :thread
  (:use :cl)
  (:export :make-thread :current-thread :threadp :thread-name :newthread
           :make-lock :acquire-lock :release-lock :with-lock-held
           :threaded-object :defslotinterface :defslotints :defclass-threaded :defprocessors :with-slot-lock
           :push-queue :pop-queue :read-queue :modify-thread :make-processor :start-processor))


(defpackage :connection
  (:use :cl)
  (:export :connection
           :read-status :modify-status
           :read-data :modify-data
           :send-message
           :read-handler))