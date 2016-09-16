(in-package :clicc)

(defun hello () (print "hello"))

(defun readln (socket)
  (read-line (usocket:socket-stream socket)))

(defun println (string socket)
  (write-line string (usocket:socket-stream socket))
  (force-output (usocket:socket-stream socket)))

(defun connectto (addr port)
  (usocket:socket-connect addr port))
  
(defun echo-test (thing)
  (let ((socket (connectto "127.0.0.1" 5678)))
    (println thing socket)
    (readln socket)))