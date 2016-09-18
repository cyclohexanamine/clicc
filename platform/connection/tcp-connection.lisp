;;;; TCP socket connection using usocket.
;;;; This subclasses connection:connection.

(in-package :connection)
(export 'tcp-connection)

(defclass tcp-connection (connection)
  ((socket
    :initform NIL)))
(thread:defslotints tcp-connection (socket))

(defmethod send-message ((conn tcp-connection) message &key success-callback failure-callback)
  (let ((command (list :send message)))
    (if success-callback (nconc (list :success success-callback)))
    (if failure-callback (nconc (list :failure failure-callback)))
    (thread:push-queue conn command)))

(defmethod thread:make-processor ((conn tcp-connection))
  (lambda ()
    ;; Main event loop
    ;; get-stream will handle reconnects, etc.
    (loop for strm = (get-stream conn)
          if strm
          do ;; Send a message if there's one to send.
             (let ((command (thread:pop-queue conn)))
               (if (eq (car command) :send)
                 (write-line (cdr command) strm)))
             ;; Read a message if there is one.
             (let ((newline (read-line-no-hang strm)))
               (if newline
                 (funcall (read-handler conn) newline (read-data conn)))))))


(defun peek-char-no-hang (strm)
  (let ((chr (read-char-no-hang strm NIL :eof)))
    (if (and (not (eq chr :eof)) chr)
      (unread-char chr strm))
    chr))

(defun read-line-no-hang (strm)
  (if (peek-char-no-hang strm)
    (read-line strm NIL NIL)))

(defun socket-alive (sock)
  (if sock
    (let ((strm (usocket:socket-stream sock)))
      (not (eq (peek-char-no-hang strm) :eof)))
    NIL))

(defun get-stream (conn)
  (let ((sock (read-socket conn)))
    ;; Reconnect if the socket is dead.
    (if (not (socket-alive sock))
      (setf sock (let* ((addr (read-address conn))
                     (newsocket (usocket:socket-connect (car addr) (cadr addr))))
                       (setf (read-socket conn) newsocket))))
    ;; Return the associated stream, or nil if we didn't manage to reconnect the socket.
    (if (socket-alive sock)
      (usocket:socket-stream sock))))

