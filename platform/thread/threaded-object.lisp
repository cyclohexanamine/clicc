;;;; A generic class with thread-safe slots and an internal thread.

(in-package :thread)


;;; The object itself. Thread will be its internal thread, and queue a message queue for this thread.
;;; Locks is an alist of locks made by (thread:make-lock), with names corresponding to the slots they protect.

(defclass-with-slots threaded-object ())


;; Push a message to the back of the internal queue.
(defgeneric push-queue (obj msg))
(defmethod push-queue ((obj threaded-object) msg)
  (modify-queue obj
    (lambda (queue)
      (append queue (list msg)))))
      
;; Pop a message from the front of the internal queue.
(defgeneric pop-queue (obj))
(defmethod pop-queue ((obj threaded-object))
  (with-slot-lock obj 'queue
    (let* ((queue (slot-value obj 'queue))
           (msg (car queue))
           (new-queue (cdr queue)))
      (setf (slot-value obj 'queue) new-queue)
      msg)))

(defgeneric make-processor (obj))

(defgeneric start-processor (obj))
(defmethod start-processor ((obj threaded-object))
  (let ((processor (make-processor obj)))
    (setf (read-thread obj) (newthread funcall processor))))
    