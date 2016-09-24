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
   ;; Array of characters to buffer before newline
   (buffer
    :initform (make-array 1 :fill-pointer 0))
   ;; Is this connection a listener?
   (listener-p
    :initarg :listener-p
    :initform NIL)))
(thread:defslotints tcp-connection (socket buffer listener-p))


;;; External interface

(defmethod send-message ((conn tcp-connection) message &key success-callback failure-callback)
  (thread:with-slot conn (sock 'socket)
    (let ((strm (usocket:socket-stream sock)))
      (write-line message strm)
      (force-output strm))))

(defmethod read-message ((conn tcp-connection))
  (thread:with-slot conn (sock 'socket)
    (thread:with-slot conn buffer
      (read-with-buffer (usocket:socket-stream sock) buffer))))

(defmethod is-alive ((conn tcp-connection))
  (let ((sock (read-socket conn)))
    ;; Check that sock isn't NIL, and that the next character isn't EOF.
    (and sock
         (not (peek-char-eof sock)))))

(defmethod close-connection ((conn tcp-connection))
  (thread:with-slot conn (sock 'socket)
    (usocket:socket-close sock)))


;;; Internals

;; This merely wraps an existing socket in a connection object, without making a new underlying connection.
(defun make-tcp-connection (sock)
  (let ((addr (list (usocket:get-peer-address sock) (usocket:get-peer-port sock))))
    (make-instance 'tcp-connection :address addr :socket sock)))

;; Similarly to make-tcp-connection, except the address is local rather than the peer's, and listener-p is true.
(defun make-tcp-listener (sock)
  (let ((addr (list (usocket:get-local-address sock) (usocket:get-local-port sock))))
    (make-instance 'tcp-connection :address addr :socket sock :listener-p T)))

;; Checks whether the socket has been closed. This doesn't work on Windows, so later code,
;; e.g., read-with-buffer, should account for the possibility of reading an EOF.
(defun peek-char-eof (sock)
  (let* ((strm (usocket:socket-stream sock))
         (char (read-char-no-hang strm NIL :eof)))
    (if (eq char :eof)
      T
      ;; Put back what we read, if it wasn't EOF or NIL.
      (if char (progn (unread-char char strm) NIL)))))

;; Read from the socket until we hit a newline. If we don't get one, store what we read in the buffer
;; for next time, and return NIL. If we find EOF, we return NIL.
(defun read-with-buffer (strm buffer)
  (loop for chr = (read-char-no-hang strm NIL NIL)
    while chr
    do (case chr
        (#\Newline (return (prog1
                            (coerce (copy-seq buffer) 'string)
                            (clear-buffer buffer))))
        (NIL (return))
        (otherwise (vector-push-extend chr buffer)))))


(defun clear-buffer (buffer)
  (setf (fill-pointer buffer) 0))
