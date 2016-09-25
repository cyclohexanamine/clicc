;;;; Generic connection manager, using connection object.
;;;; This is intended to be subclassed into concrete connection managers,
;;;; paired with concrete connections, that can be instantiated.

(in-package :connection)

;;; The manager has some local address (usually (host, port)).
;;; It also has a list of connections, and a message handler to call
;;; whenever a message is received from any connection. The ID counter
;;; gives its connections unique IDs.
(thread:defclass-threaded connection-manager ()
  ((address
    :initarg :address)
   (connections
    :initform NIL)
   (message-handler
    :initform (lambda (message data manager) (declare (ignore data manager)) (write-line message))
    :initarg :handler)
   (id-counter :initform 0)))
(thread:defslotints connection-manager (connections message-handler id-counter))


;;; External interface

;; The external method for queueing requests to send messages.
;; Criteria is a plist of criteria to select the connections by; making sure all values
;; match the ones in the connection's data plist.
(defmethod-g send-message-to ((manager connection-manager) msg criteria)
  (thread:push-queue manager (list :send msg criteria)))

;; External method for queueing a request to create a new connection (including opening it).
;; data is the plist to give it on creation (an ID will be added to it).
(defmethod-g open-connection ((manager connection-manager) addr data callback)
  (thread:push-queue manager (list :open addr data callback)))


;;; Internals

;; Return a new ID and increment the ID counter (keeping it unique).
(defmethod-g new-id ((manager connection-manager))
  (thread:modify-slot manager id-counter
    (if (integerp id-counter)
      (1+ id-counter)
      0)))

;; Addd the given connection (without changing its state) to the connection list,
;; giving it an ID.
(defmethod-g add-connection ((manager connection-manager) conn)
  (thread:modify-slot conn data
    (setf (getf data :id) (new-id manager))
    data)
  (thread:modify-slot manager (conns 'connections)
    (nconc conns (list conn))))

;; Close the connection and remove it from the connection list.
(defmethod-g remove-connection ((manager connection-manager) conn)
  (thread:modify-slot manager (conns 'connections)
    (remove conn conns))
  (close-connection conn))

;; Check whether there's a message to get from the connection, and if there is,
;; queue up to call the handler on it. This should probably only be used when the
;; connection has been reported as active, although not necessarily.
(defmethod-g process-message ((manager connection-manager) conn)
  (let ((message (read-message conn)))
    (if message
      (thread:push-queue manager (list :recv message (read-data conn))))))

;; Call the handler on the given message; this isn't in a new thread, so whatever
;; is calling this will likely be a worker thread.
(defmethod-g internal-handle-message ((manager connection-manager) message data)
  (let ((handler (read-message-handler manager)))
    (funcall handler message data manager)))

;; Send a message msg along all the connections which match the given criteria.
;; This will also be called from a worker, probably.
(defmethod-g internal-send-message-to ((manager connection-manager) msg criteria)
  (thread:with-slot manager (conns 'connections)
    (loop for conn in conns
      if (match-connection conn criteria)
      if (not (read-listener-p conn))
      do (send-message conn msg))))

;; Create a connection to addr, and add it to the connection list.
(defgeneric internal-open-connection (manager addr data callback))
