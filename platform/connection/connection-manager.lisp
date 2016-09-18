(in-package :connection)

(thread:defclass-threaded connection-manager ()
  ((address
    :initform '(#(0 0 0 0) 5678))
   (listener-thread
    :initform NIL)
   (listener
    :initform NIL)
   (connections
    :initform NIL)
   (message-handler
    :initform (lambda (message data) (write-line message)))
   (id-counter :initform 0)))
(thread:defslotints connection-manager (listener connections message-handler id-counter))


(defmethod thread:make-processor ((manager connection-manager))
  (lambda ()
    ; Open the listener socket
    (setf (slot-value manager 'listener)
      (let ((addr (slot-value manager 'address)))
        (usocket:socket-listen (car addr) (cadr addr))))
    (add-connection manager (slot-value manager 'listener))

    ;; Listener loop
    (setf (slot-value manager 'listener-thread)
      (thread:newthread loop
        (loop for sock in (usocket:wait-for-input (read-connections manager) :timeout 1000 :ready-only T) do
          (cond ((eq sock (read-listener manager))
                  (accept-connection manager))
                ((peek-char-eof sock)
                  (remove-connection manager sock))
                (T
                  (process-message manager sock))))))

    ;; Event loop
    (loop for msg = (thread:pop-queue manager)
      do (if msg
            (case (car msg)
              (:send (send-message manager msg)))
            (sleep 0.1)))))

            
(defun accept-connection (manager)
  (let* ((listener (read-listener manager))
         (new-conn (usocket:socket-accept listener))
         (new-id (modify-id-counter manager #'1+)))
    (format T "~a" new-id)
    (add-connection manager new-conn)))

(defun add-connection (manager sock)
  (modify-connections manager
    (lambda (conns)
      (nconc conns (list sock)))))

(defun remove-connection (manager sock)
  (modify-connections manager
    (lambda (conns)
      (remove sock conns)))
  (usocket:socket-close sock))

(defun process-message (manager sock)
  (let ((message (read-line (usocket:socket-stream sock)))
        (handler (read-message-handler manager)))
    (thread:newthread funcall handler message ())))

(defun send-message (manager msg)
  (thread:with-slot-lock manager 'connections
    (loop for sock in (slot-value manager 'connections)
      if (not (eq sock (read-listener manager)))
      do (let ((strm (usocket:socket-stream sock)))
           (write-line msg strm)
           (force-output strm)))))

(defun peek-char-eof (sock)
  (let* ((strm (usocket:socket-stream sock))
         (char (read-char strm NIL :eof)))
    (if (eq char :eof)
      T
      (progn (unread-char char strm) NIL))))

