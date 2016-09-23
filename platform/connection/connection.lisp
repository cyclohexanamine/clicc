;;;; Generic connection object, used by the connection manager.
;;;; This is intended to be subclassed into concrete connections that can be instantiated.

(in-package :connection)


;;; We have fields as follows:
;;;   address - this is specialised to whatever the connection is, but will likely be a (host, port) list
;;;   data - plist for metadata used by higher-ups

(thread:defclass-threaded connection ()
  ((address
    :initarg :address)
   (data
    :initarg :data
    :initform NIL)))


(defgeneric send-message (conn message &key success-callback failure-callback)
  (:documentation "Send a message along the connection. Optional callbacks can be registered."))

(defgeneric read-message (conn))
  
(defgeneric is-alive (conn))
(defgeneric close-connection (conn))

(defgeneric match-connection (conn criteria))
(defmethod match-connection ((conn connection) criteria)
  (let ((data (read-data conn)))
    (every #'identity
      (loop for label in criteria
         for val   in criteria
         collecting (eq (get data label) val)))))