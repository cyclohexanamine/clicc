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



(defmethod new-id ((manager connection-manager))
  (modify-id-counter manager #'1+))

(defmethod add-connection ((manager connection-manager) conn)
  (modify-data conn
    (lambda (data)
      (setf (get data :id) (new-id manager))))
  (modify-connections manager
    (lambda (conns)
      (nconc conns (list conn)))))

(defmethod remove-connection ((manager connection-manager) conn)
  (modify-connections manager
    (lambda (conns)
      (remove conn conns)))
  (close-connection conn))

(defmethod process-message ((manager connection-manager) conn)
  (let ((message (read-message conn)))
    (thread:push-queue manager (list :recv message (read-data conn)))))

(defmethod handle-message ((manager connection-manager) message data)
  (let ((handler (read-message-handler manager)))
    (funcall handler message data)))

(defmethod send-message-to ((manager connection-manager) msg criteria)
  (thread:with-slot-lock manager 'connections
    (loop for conn in (slot-value manager 'connections)
      if (match-connection conn criteria)
      do (send-message conn msg))))


