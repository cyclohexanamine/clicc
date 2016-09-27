;;;; A generic class with thread-safe slots and an internal thread.

(in-package :thread)


;;; The object itself.

(defclass-with-slots threaded-object ())


;; Push a message to the back of an internal queue.
(defgeneric push-queue (obj proc-name msg))
(defmethod push-queue ((obj threaded-object) proc-name msg)
  (modify-queue obj (queue proc-name)
    ;; Append the message to the queue.
    (append queue (list msg))))

;; Pop a message from the front of the internal queue.
(defgeneric pop-queue (obj proc-name &key wait))
(defmethod pop-queue ((obj threaded-object) proc-name &key wait)
  ;; Block until the queue is not empty, if we should.
  (if wait
    ;; A simple spinlock.
    (loop while (not (modify-queue obj (queue proc-name) queue))
      do (sleep 0.01)))
  (let ((msg))
    (modify-queue obj (queue proc-name)
      ;; Take the front and rest of the queue.
      (setf msg (car queue))
      (cdr queue))
    msg))


;; Methods for this are defined by defprocessors. When called, this will populate the object's
;; 'processors' slot with a list of processors of the form
;;  (name init-form loop-form number-of-threads thread-list)
(defgeneric make-processors (obj))

;; Call make-processors and set them all running, having called the init forms.
(defgeneric start-processors (obj))
(defmethod start-processors ((obj threaded-object))
  (make-processors obj)
  (destructuring-bind (names queues loop-funcs thread-lists num-threads init-funcs) (zipcar (slot-value obj 'processors))
    (declare (ignore names))
    ;; Clear all the queues.
    (loop for queue in queues do
      (setf queue NIL))

    ;; Run all the initialisations.
    (loop for init-func in init-funcs do
      (funcall init-func))

    ;; Then start the loops.
    (loop for loop-func in loop-funcs
          for num-thread in num-threads
          for thread-list in thread-lists do
            (setf thread-list
              (loop repeat num-thread collecting
                (thread:newthread funcall loop-func))))))

