(in-package :clicc)

(defun echo-tcp-handler (stream)
  (declare (type stream stream))
  (loop for line = (read-line stream nil :eof)
    until (eq line :eof)
    do (write-line line stream)
       (force-output stream)))
      
(defun hello-tcp-handler (stream)
  (declare (type stream stream))
  (write-line "Hello world" stream)
  (force-output stream))

(defmacro newthread (fname &rest args)
  `(bordeaux-threads:make-thread
    (lambda ()
      (,fname ,@args))))
  
(defun multi-server (host port socket-handler)
  (let ((listener (usocket:socket-listen host port)))
    (loop for new-socket = (usocket:socket-accept listener)
      do (write-line "New connection accepted")
         (newthread funcall socket-handler new-socket)
         (write-line "Made thread"))))
      
(defun stream-server (host port stream-handler)
  (multi-server host port
    (lambda (socket)
      (funcall stream-handler (usocket:socket-stream socket))
      (usocket:socket-close socket))))
