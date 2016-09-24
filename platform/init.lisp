(load "platform.asd")
(asdf:load-system :platform)

(defun start-manager ()
  (let ((cm (make-instance 'connection:tcp-connection-manager :address '(#(0 0 0 0) 5678))))
    (thread:start-processors cm)))
    
(start-manager)