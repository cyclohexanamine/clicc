(ql:quickload "usocket")
(rename-package 'usocket 'us)
(ql:quickload "bordeaux-threads")
(rename-package 'bordeaux-threads 'bt)

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
  `(bt:make-thread
    (lambda ()
      (,fname ,@args))))
  
(defun multi-server (host port socket-handler)
  (let ((listener (us:socket-listen host port)))
    (loop for new-socket = (us:socket-accept listener)
      do (write-line "New connection accepted")
         (newthread funcall socket-handler new-socket)
         (write-line "Made thread"))))
      
(defun stream-server (host port stream-handler)
  (multi-server host port
    (lambda (socket)
      (funcall stream-handler (us:socket-stream socket))
      (us:socket-close socket))))
  
(newthread stream-server #(0 0 0 0) 5678 #'echo-tcp-handler)
