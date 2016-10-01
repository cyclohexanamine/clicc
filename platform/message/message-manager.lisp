;;;; Message manager. This is meant to be instantiated as-is.
;;;; This handles the connection manager (and state manager) - both for
;;;; initialisation and operation.

(in-package :message)


;;; The manager itself. Fields are
;;;   cm - connection manager
;;;   handlers - a structure determining how messages are handled
;;;              by higher-level components.
;;;   active-handlers - a structure showing which handlers are active,
;;;                     to enforce serialisation.
;;;   id-counter - a counter to generate unique IDs for handlers.
(thread:defclass-threaded message-manager ()
  (cm
   handlers
   (active-handlers :initform nil)
   (id-counter :initform 0)))


;;; External interface

;; Add a handler for messages matching criteria. Returns a handler id.
(defmethod-g add-handler ((manager message-manager) handler criteria)
  (thread:push-queue manager 'main (list :addh handler criteria)))

;; Remove the handler with the given id.
(defmethod-g remove-handler ((manager message-manager) handler-id)
  (thread:push-queue manager 'main (list :remh handler-id)))

;; Receive a message from a connection and apply the relevant handler to it.
(defmethod-g receive-conn-message ((manager message-manager) message data cm)
  (thread:push-queue manager 'main (list :recv message data)))


;;; Internals.

(defmethod-g queue-loop ((manager message-manager) msg)
  (case (car msg)
    (:send (apply #'connection:send-message-to (read-cm manager) (cdr msg)))
    (:open (apply #'connection:open-connection (read-cm manager) (cdr msg)))
    (:recv (apply #'internal-apply-handler manager (cdr msg)))
    (:addh (apply #'internal-add-handler manager (cdr msg)))
    (:remh (apply #'internal-remove-handler manager (cdr msg)))))

(thread:defprocessors message-manager
  (main (:loop-func #'queue-loop
         :queue T
         :threads 5)))

(defmethod-g internal-add-handler ((manager message-manager) handler criteria)
  (let ((id (new-id manager)))
    (thread:modify-slot manager handlers
      (append handlers (list (list id criteria handler)))))) ; temp

(defmethod-g internal-remove-handler ((manager message-manager) handler-id)
  (thread:modify-slot manager handlers
    (remove-if (lambda (handler) (equal (car handler) handler-id)) handlers))) ; temp

(defmethod-g internal-apply-handler ((manager message-manager) message conn-data)
  (let ((handlers (read-handlers manager)))
    (funcall (nth 2 (car handlers)) message conn-data))) ; temp


;; Return a new ID and increment the ID counter (keeping it unique).
(defmethod-g new-id ((manager message-manager))
  (thread:modify-slot manager id-counter
    (if (integerp id-counter)
      (1+ id-counter)
      0)))


