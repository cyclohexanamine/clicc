(defpackage :mac
  (:use :cl)
  (:export :seqp :defmethod-g :mashup-symbol :mquote :eval-when-compile :zipcar :nullf :defnullf))

(defpackage :thread
  (:use :cl :mac)
  (:export :make-thread :current-thread :threadp :thread-name :newthread
           :make-recursive-lock :acquire-recursive-lock :release-recursive-lock :with-recursive-lock-held
           :threaded-object :defslotinterface :defslotints :defclass-threaded :defprocessors :with-slot :with-slot-lock :modify-slot
           :push-queue :pop-queue :read-queue :make-processors :start-processors))


(defpackage :connection
  (:use :cl :mac)
  (:export :connection :connection-manager
           :send-message-to :open-connection))