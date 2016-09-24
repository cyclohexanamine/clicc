;;;; A generic class with thread-safe slots and an internal thread.

(in-package :thread)


;;; The object itself. Thread will be its internal thread, and queue a message queue for this thread.
;;; Locks is an alist of locks made by (thread:make-lock), with names corresponding to the slots they protect.

(defclass-with-slots threaded-object ())


;; Push a message to the back of the internal queue.
(defgeneric push-queue (obj msg))
(defmethod push-queue ((obj threaded-object) msg)
  (modify-slot obj queue
    (append queue (list msg))))
      
;; Pop a message from the front of the internal queue.
(defgeneric pop-queue (obj))
(defmethod pop-queue ((obj threaded-object))
  (with-slot obj queue
    (let* ((msg (car queue))
           (new-queue (cdr queue)))
      (setf (slot-value obj 'queue) new-queue)
      msg)))

;; Methods for this are defined by defprocessors. When called, this will populate the object's
;; 'processors' slot with a list of processors of the form
;;  (name init-form loop-form number-of-threads thread-list)
(defgeneric make-processors (obj))

;; Call make-processors and set them all running, having called the init forms.
(defgeneric start-processors (obj))
(defmethod start-processors ((obj threaded-object))
  (make-processors obj)
  (destructuring-bind (names init-funcs loop-funcs num-threads thread-lists) (zipcar (slot-value obj 'processors))
    ;; Run all the initialisations first.
    (loop for init-func in init-funcs
      do (funcall init-func))
      
    ;; Then start the loops.
    (loop for loop-func in loop-funcs
          for num-thread in num-threads
          for thread-list in thread-lists do
            (setf thread-list
              (loop repeat num-thread collecting
                (thread:newthread funcall (lambda () (loop (funcall loop-func)))))))))
