;;;; TCP socket connection using usocket.
;;;; This subclasses connection:connection.

(in-package :connection)
(export 'tcp-connection)

(defclass tcp-connection (connection)
  ((socket
    :initform NIL)))
(thread:defslotints tcp-connection (socket))


;; External interface

; (defmethod send-message ((conn tcp-connection) message &key success-callback failure-callback)
  ; (thread:newthread 
    ; (modify-stream conn
      ; (lambda (strm)
        ; (


;; Internals





; (defun peek-char-no-hang (strm)
  ; (if strm
    ; (let ((chr (read-char-no-hang strm NIL :eof)))
      ; (if (and (not (eq chr :eof)) chr)
        ; (unread-char chr strm))
      ; chr)))

; (defun read-line-no-hang (strm)
  ; (if (peek-char-no-hang strm)
    ; (read-line strm NIL NIL)))

; (defun socket-alive (sock)
  ; (if sock
    ; (let ((strm (usocket:socket-stream sock)))
      ; (not (eq (peek-char-no-hang strm) :eof)))
    ; NIL))


; (defun get-stream (conn)
  ; (let ((sock (read-socket conn)))
    ; (usocket:socket-stream sock)))


    ; (if (not (socket-alive sock))
      ; (setf sock (let* ((addr (read-address conn))
                     ; (newsocket (usocket:socket-connect (car addr) (cadr addr))))
                       ; (setf (read-socket conn) newsocket))))