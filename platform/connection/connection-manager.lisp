(in-package :connection)

(thread:defclass-threaded connection-manager ()
  ((address
    :initarg :address)
   (connections
    :initform NIL)
   (message-handler
    :initform (lambda (message data) (write-line message)))
   (id-counter :initform 0)))
(thread:defslotints connection-manager (connections message-handler id-counter))



(defmethod-g new-id ((manager connection-manager))
  (thread:modify-slot manager id-counter
    (1+ id-counter)))

(defmethod-g add-connection ((manager connection-manager) conn)
  (thread:with-slot conn data
    (setf (get data :id) (new-id manager)))
  (thread:modify-slot manager (conns 'connections)
    (nconc conns (list conn))))

(defmethod-g remove-connection ((manager connection-manager) conn)
  (thread:modify-slot manager (conns 'connections)
    (remove conn conns))
  (close-connection conn))

(defmethod-g process-message ((manager connection-manager) conn)
  (let ((message (read-message conn)))
    (if message
      (thread:push-queue manager (list :recv message (read-data conn))))))

(defmethod-g handle-message ((manager connection-manager) message data)
  (let ((handler (read-message-handler manager)))
    (funcall handler message data)))

(defmethod-g send-message-to ((manager connection-manager) msg criteria)
  (thread:with-slot manager (conns 'connections)
    (loop for conn in conns
      if (match-connection conn criteria)
      do (send-message conn msg))))


