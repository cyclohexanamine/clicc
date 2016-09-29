(load "platform.asd")
(asdf:load-system :platform)
(asdf:load-system :unix-options)
(use-package :unix-options)

(defun echo-handler (message data manager)
  (connection:send-message-to manager (format NIL "Echoing: ~a" message) (list :id (getf data :id))))

(mac:defnullf nil-handler (message data manager))

(defun print-handler (message data manager) (declare (ignore data manager)) (write-line message))

(defun start-manager ()
  (defvar *cm* (make-instance 'connection:tcp-connection-manager :address '(#(0 0 0 0) 5678) :handler #'echo-handler))
  (thread:start-processors *cm*))

(defun stop-manager ()
  (thread:stop-processors *cm*))



(defun run-repl ()
  (format t "~&CLICC REPL~%Compiled with ~a ~a~%"
          (lisp-implementation-type)
          (lisp-implementation-version))
  (sb-thread:with-new-session ()
    (sb-impl::toplevel-repl nil)))

(defun main ()
  (with-cli-options ()
      (repl &parameters config)
    (restart-case
        (progn
          (start-manager)
          (if repl
            (run-repl)
            (loop (sleep 0.1))))
      (abort ()
        (sb-ext:exit :abort T))
      (exit ()
        (progn
          (stop-manager)
          (sb-ext:exit :abort NIL :timeout 20))))))


(sb-ext:save-lisp-and-die "platform.exe" :toplevel #'main :executable T)
