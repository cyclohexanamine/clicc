;;;; TCP socket connection using usocket.
;;;; This subclasses connection:connection.

(in-package :connection)
(export 'tcp-connection)

(defclass tcp-connection (connection) ())

(defmethod send-message ((conn connection) message &key success-callback failure-callback)
  (let ((command (list :send message)))
    (if success-callback (nconc (list :success success-callback)))
    (if failure-callback (nconc (list :failure failure-callback)))
    (thread:modify-queue conn 
      (lambda (queue) 
        (append queue (list command))))))
