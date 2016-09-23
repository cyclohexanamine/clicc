;;; TCP connection manager, for connection:tcp-connection.
;;; Subclasses connection:connection-manager.

(in-package :connection)
(export 'tcp-connection-manager)

(defclass tcp-connection-manager (connection-manager)
  (listener
   wait))
(thread:defslotints tcp-connection-manager (wait listener))


;; External interface

(defmethod-g accept-connection ((manager tcp-connection-manager) conn)
  (let* ((listener (read-socket conn))
         (new-conn (usocket:socket-accept listener)))
    (add-connection manager (make-tcp-connection new-conn))))


;; Internals

(thread:defprocessors (manager tcp-connection-manager)
  ;; Connection listener
  ((progn
    (setf (read-listener manager)
      (let ((addr (slot-value manager 'address)))
        (make-tcp-listener (usocket:socket-listen (car addr) (cadr addr)))))
    (add-connection manager (read-listener manager)))

  (let ((conns (wait-for-tcp-connections manager)))
    (loop for conn in conns do
      (cond ((read-listener-p conn)
              (accept-connection manager conn))
            ((not (is-alive conn))
              (remove-connection manager conn))
            (T
              (process-message manager conn)))))
  1)
  
  ;; Queue processor
  (()
  (let ((msg (thread:pop-queue manager)))
    (if msg
      (case (car msg)
        (:send (send-message-to manager (cadr msg) (caddr msg)))
        (:recv (handle-message manager (cadr msg) (caddr msg))))
      (sleep 0.1)))
  5))

(defmethod-g wait-for-tcp-connections ((manager tcp-connection-manager))
  (thread:with-slot manager wait
    (let* ((socks (thread:with-slot manager (conns 'connections)
                    (mapcar #'read-socket conns)))
           (input-socks (usocket:wait-for-input socks :timeout 1000 :ready-only T)))
      (thread:with-slot manager (conns 'connections)
        (remove-if-not 
          (lambda (conn) (is-sock-connection-p conn input-socks))
          conns)))))

(defun is-sock-connection-p (conn socks)
  (member (read-socket conn) socks))
