(defpackage :mac
  (:use :cl)
  (:export :seqp :defmethod-g :mashup-symbol :mquote :eval-when-compile :zipcar :nullf :defnullf))

(defpackage :thread
  (:use :cl :mac)
  (:export :newthread
           :threaded-object :defslotinterface :defslotints :defclass-threaded :defprocessors :with-slot :with-slot-lock :modify-slot
           :push-queue :pop-queue :peek-queue :read-queue
           :make-processors :start-processors :start-processor :stop-processors :stop-processor))


(defpackage :connection
  (:use :cl :mac)
  (:export :connection :connection-manager
           :read-address :send-message-to :open-connection))