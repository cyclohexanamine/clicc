(load "platform.asd")
(asdf:load-system :platform)

(defun echo-handler (message data manager)
  (connection:send-message-to manager (format NIL "Echoing: ~a" message) (list :id (getf data :id))))
  
(mac:defnullf nil-handler (message data manager))

(defun start-manager ()
  (let ((cm (make-instance 'connection:tcp-connection-manager :address '(#(0 0 0 0) 5678) :handler #'echo-handler)))
    (thread:start-processors cm)))
    
(start-manager)