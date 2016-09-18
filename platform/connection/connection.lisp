;;;; Generic connection object, used by the connection manager.
;;;; This is intended to be subclassed into concrete connections that can be instantiated.

(in-package :connection)


;;; We have fields as follows:
;;;   address - this is specialised to whatever the connection is, but will likely be a (host, port) list
;;;   status - T/NIL, indicating whether the connection is alive
;;;   data - plist for metadata used by higher-ups
;;;   handler - message handler to be called when a message is received; return value is not important; arguments to the handler are
;;;     message - message received
;;;     data - the connection's data

(thread:defclass-threaded connection ()
  ((address
    :initarg :address)
   (status
    :initarg :status
    :initform NIL)
   (data
    :initarg :data
    :initform NIL)
   (handler
    :initarg :data
    :initform (lambda (message data)))))


(defgeneric send-message (conn message &key success-callback failure-callback)
  (:documentation "Send a message along the connection. Optional callbacks can be registered."))

