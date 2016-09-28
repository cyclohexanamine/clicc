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

;; View the message from the front of the internal queue without removing it.
(defgeneric peek-queue (obj proc-name))
(defmethod peek-queue ((obj threaded-object) proc-name)
  (let ((msg))
    (modify-queue obj (queue proc-name)
      (setf msg (car queue))
      queue)
    msg))

;; Pop a message from the front of the internal queue.
(defgeneric pop-queue (obj proc-name &key wait))
(defmethod pop-queue ((obj threaded-object) proc-name &key wait)
  ;; Block until the queue is not empty, if we should.
  (if wait
    ;; A simple spinlock.
    (loop while (not (peek-queue obj proc-name))
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
;; This essentially assumes that the processors haven't been touched before.
(defgeneric start-processors (obj))
(defmethod start-processors ((obj threaded-object))
  (make-processors obj)
  (destructuring-bind (names queues loop-funcs thread-lists num-threads init-funcs) (zipcar (slot-value obj 'processors))
    (declare (ignore loop-funcs thread-lists num-threads))
    ;; Clear all the queues.
    (loop for queue in queues do
      (setf queue NIL))

    ;; Run all the initialisations.
    (loop for init-func in init-funcs do
      (funcall init-func))

    ;; Then start the loops.
    (loop for proc-name in names do
      (start-processor obj proc-name))))

;; Start the processor with name pname; wait until any stop messages
;; have been consumed, then execute the loop-func in a new thread.
;; If num-to-start is supplied, that many threads will be started;
;; otherwise it will start as many as are set in the processor definition.
;; This essentially assumes that all threads of this processor have been
;; stopped.
(defgeneric start-processor (obj pname &optional num-to-start))
(defmethod start-processor ((obj threaded-object) pname &optional num-to-start)
  ;; Spinlock while stop messages haven't been consumed.
  (loop while (equal (peek-queue obj pname) '(:thread-internal-stop))
    do (sleep 0.01))
  (with-processor obj (proc pname)
    (let ((loop-func (nth 2 proc))
          (num-thread (if num-to-start num-to-start (nth 4 proc))))
      (setf (nth 3 proc) ; Overwrite the thread list.
        (loop repeat num-thread collecting
          ;; Start a new thread with the loop function.
          (thread:newthread funcall loop-func)))
      (setf (nth 4 proc) num-thread)))) ; Overwrite the number of threads.

;; Stop all threads of the processor with the name pname. This assumes
;; that they are all already running, and does not take effect immediately;
;; the threads need to consume the stop message first.
(defgeneric stop-processor (obj pname))
(defmethod stop-processor ((obj threaded-object) pname)
  (with-processor obj (proc pname)
    (let ((nthreads (nth 4 proc))
          (old-queue (nth 1 proc)))
      (setf (nth 1 proc)
        ;; Put as many stop messages on the front of the queue as there are threads.
        (append (make-list nthreads :initial-element '(:thread-internal-stop)) old-queue)))))
