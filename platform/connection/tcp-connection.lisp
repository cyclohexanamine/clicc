;;;; TCP socket connection using usocket.
;;;; This subclasses connection:connection.

(in-package :connection)


(defclass tcp-connection (connection) ())

(defmethod send-message ((conn connection) message &key success-callback failure-callback)
    ())