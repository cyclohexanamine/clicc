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

;; Send a message along the connection. Optional callbacks can be registered.
(defgeneric send-message (conn message &key success-callback failure-callback))

;; Read a message from the connection. In some cases this may be NIL, in which case no message was read.
(defgeneric read-message (conn))
  
;; Is the connection still alive in whatever sense?
(defgeneric is-alive (conn))

;; Close the connection in whatever sense.
(defgeneric close-connection (conn))

;; Check whether the connection matches all the criteria given. Criteria are a plist,
;; and each label will have its value checked against the connection's data plist.
(defgeneric match-connection (conn criteria))
(defmethod match-connection ((conn connection) criteria)
  (let ((data (read-data conn)))
    (every #'identity 
      (loop for (label val) on criteria by #'cddr
         collecting (equal (getf data label) val)))))