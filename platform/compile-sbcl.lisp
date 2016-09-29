(load "platform.asd")
(asdf:load-system :platform)

(defun echo-handler (message data manager)
  (connection:send-message-to manager (format NIL "Echoing: ~a" message) (list :id (getf data :id))))
  
(mac:defnullf nil-handler (message data manager))

(defun print-handler (message data manager) (declare (ignore data manager)) (write-line message))

(defun start-manager ()
  (defvar *cm* (make-instance 'connection:tcp-connection-manager :address '(#(0 0 0 0) 5678) :handler #'echo-handler))
  (thread:start-processors *cm*))
  
(defun stop-manager ()
  (thread:stop-processors *cm*)
  (write-line "Stopped processors")
  (sleep 1)
  (write-line (format NIL "Processors: ~a" (slot-value *cm* 'thread::processors))))
    

(defun main ()
  (restart-case
      (progn
        (start-manager)
        (loop (sleep 0.1)))
    (abort () 
      (sb-ext:exit :abort T))
    (exit () 
      (progn
        (stop-manager)
        (sb-ext:exit :abort NIL :timeout 20)))))
    

(sb-ext:save-lisp-and-die "platform.exe" :toplevel #'main :executable T)
