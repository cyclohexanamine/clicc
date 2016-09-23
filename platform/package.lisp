(defpackage :mac
  (:use :cl)
  (:export :seqp :defmethod-g))

(defpackage :thread
  (:use :cl)
  (:export :make-thread :current-thread :threadp :thread-name :newthread
           :make-lock :acquire-lock :release-lock :with-lock-held
           :threaded-object :defslotinterface :defslotints :defclass-threaded :defprocessors :with-slot :modify-slot
           :push-queue :pop-queue :read-queue :make-processor :start-processor))


(defpackage :connection
  (:use :cl)
  (:export :connection
           :read-status :modify-status
           :read-data :modify-data
           :send-message
           :read-handler))