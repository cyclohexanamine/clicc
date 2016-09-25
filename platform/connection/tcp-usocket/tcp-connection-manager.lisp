;;; TCP connection manager, for connection:tcp-connection.
;;; Subclasses connection:connection-manager.

(in-package :connection)
(export 'tcp-connection-manager)

;;; We have a listener socket, which listens and accepts new connections.
;;; The 'wait' slot is just a dummy, used for locking on usocket:wait-for-input.

(defclass tcp-connection-manager (connection-manager)
  (listener
   wait))
(thread:defslotints tcp-connection-manager (wait listener))


;;; Internals

(thread:defprocessors (manager tcp-connection-manager)
  ;; Connection listener; start the listener and add it to the main connection list.
  (:listener
  (progn
    (setf (read-listener manager)
      (let ((addr (slot-value manager 'address)))
        (make-tcp-listener (usocket:socket-listen (car addr) (cadr addr)))))
    (add-connection manager (read-listener manager)))

  ;; In the loop, wait for connection activity.
  (let ((conns (wait-for-tcp-connections manager)))
    (loop for conn in conns do
      (cond ((read-listener-p conn) ; This is a listener, so accept a new connection.
              (accept-connection manager conn))
            ((not (is-alive conn)) ; This connection has died.
              (remove-connection manager conn))
            (T ; Otherwise, this is a normal connection, so check for a message.
              (process-message manager conn))))
    ;; Also, prune dead connections in general.
    (thread:with-slot manager connections
      (loop for conn in connections do
        (if (and (not (read-listener-p conn)) (not (is-alive conn)))
            (remove-connection manager conn)))))
  1) ; This should be unique, given that it messes around with the connection list.
  
  ;; Queue processor
  (:queue-processor
  ()
  (let ((msg (thread:pop-queue manager)))
    (if msg
      (case (car msg)
        (:send (internal-send-message-to manager (cadr msg) (caddr msg)))
        (:recv (internal-handle-message manager (cadr msg) (caddr msg)))
        (:open (internal-open-connection manager (cadr msg) (caddr msg))))
      (sleep 0.05)))
  5))


;; Specialising a generic from connection-manager, to open a new connection.
(defmethod internal-open-connection ((manager tcp-connection-manager) addr data)
  (let* ((newsock (usocket:socket-connect (car addr) (cadr addr)))
         (newconn (make-tcp-connection newsock)))
    (thread:modify-slot newconn (cdata 'data)
      (nconc cdata data))
    (add-connection manager newconn)))


;; This wraps usocket:wait-for-input, allowing the same functionality but for connection
;; objects instead of sockets. It's safe to modify the connection list while another thread
;; is waiting on connections, but two shouldn't be at once (hence the 'wait' lock).
(defmethod-g wait-for-tcp-connections ((manager tcp-connection-manager))
  (thread:with-slot-lock manager wait
    (let* ((socks (thread:with-slot manager (conns 'connections)
                    (mapcar #'read-socket conns)))
           (input-socks (usocket:wait-for-input socks :timeout 10 :ready-only T)))
      (thread:with-slot manager (conns 'connections)
        (remove-if-not 
          (lambda (conn) (is-sock-connection-p conn input-socks))
          conns)))))

;; Given a listener 'conn', this will create and add a new connection from whoever
;; has connected to the listener socket. This will block if there are no new connections.
(defmethod-g accept-connection ((manager tcp-connection-manager) conn)
  (let* ((listener (read-socket conn))
         (new-conn (usocket:socket-accept listener)))
    (add-connection manager (make-tcp-connection new-conn))))

(defun is-sock-connection-p (conn socks)
  (member (read-socket conn) socks))
