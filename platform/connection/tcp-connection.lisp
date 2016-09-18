;;;; TCP socket connection using usocket.
;;;; This subclasses connection:connection.

(in-package :connection)
(export 'tcp-connection)

(defclass tcp-connection (connection)
  ((address
    :initarg :address
    :initform '(#(0 0 0 0) 5678))
    (socket
    :initarg :socket
    :initform NIL)
    (listener-p
    :initarg :listener-p
    :initform NIL)))
(thread:defslotints tcp-connection (socket listener-p))


;; External interface

(defmethod send-message ((conn tcp-connection) message &key success-callback failure-callback)
  (modify-socket conn
    (lambda (sock)
      (let ((strm (usocket:socket-stream sock)))
        (write-line message strm)
        (force-output strm)
        sock))))
        
(defmethod read-message ((conn tcp-connection))
  (let (message)
    (modify-socket conn
      (lambda (sock)
        (let ((strm (usocket:socket-stream sock)))
          (setf message (read-line strm))
          sock)))
    message))

(defmethod is-alive ((conn tcp-connection))
  (let ((sock (read-socket conn)))
    (and sock
         (peek-char-eof sock))))

(defmethod close-connection ((conn tcp-connection))
  (modify-socket conn
    (lambda (sock)
      (usocket:socket-close sock)
      sock)))


;; Internals

(defun make-tcp-connection (sock)
  (let ((addr (list (usocket:get-peer-address sock) (usocket:get-peer-port sock))))
    (make-instance 'tcp-connection :address addr :socket sock)))

(defun make-tcp-listener (sock)
  (let ((addr (list (usocket:get-local-address sock) (usocket:get-local-port sock))))
    (make-instance 'tcp-connection :address addr :socket sock :listener-p T)))

(defun peek-char-eof (sock)
  (let* ((strm (usocket:socket-stream sock))
         (char (read-char strm NIL :eof)))
    (if (eq char :eof)
      T
      (progn (unread-char char strm) NIL))))
