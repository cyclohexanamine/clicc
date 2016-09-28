;;;; Some macros to help definitions for threaded objects.

(in-package :thread)

;; The generic threaded object slots.
(defmacro threaded-object-slots ()
  ''(processors locks))

;; The possible messages to be sent via a message queue internally.
(defmacro threaded-object-msgs ()
  ''(:thread-internal-stop))


;;; Macros for interface definitions.

;; Wraps finding the lock to be held based on the name of the slot,
;; and binds the slot value to a given name. If the slot is unbound,
;; NIL will be used instead. If a name is not given, it will be taken
;; to be the same as the slot name.
(defmacro with-slot (obj slot-def &body body)
  (destructuring-bind (bind-name slot-name)
                        (if (consp slot-def)
                          (list (car slot-def) (cadr slot-def))
                          (list slot-def (mquote slot-def)))
    `(with-recursive-lock-held ((cdr (assoc ,slot-name (slot-value ,obj 'locks))))
      (let ((,bind-name
              (if (slot-boundp ,obj ,slot-name)
                (slot-value ,obj ,slot-name))))
        ,@body))))

;; For the case of holding a lock for a slot but not using the value.
(defmacro with-slot-lock (obj slot-name &body body)
  `(with-slot ,obj ,slot-name
    (declare (ignore ,slot-name))
    ,@body))

;; Defines a read-and-set macro for a slot, in the form of, e.g.,
;;  (modify-slot classabc (xyzval 'xyz)
;;    (do-something xyz)
;;    (value-to-set))
;; If a binding name is not given, it will be taken to be the same
;; as the slot name.
(defmacro modify-slot (obj slot-def &body body)
  (let ((slot-name (if (consp slot-def)
                      (cadr slot-def)
                      (mquote slot-def))))
  `(with-slot ,obj ,slot-def
    (setf (slot-value ,obj ,slot-name)
      (progn ,@body)))))

;; Wraps finding the queue of a particular processor (proc-name), binding it to queue-name.
(defmacro modify-queue (obj (queue-name proc-name) &body body)
  (let ((processors-sym (gensym))
        (proc-sym (gensym)))
    `(with-slot ,obj (,processors-sym 'thread::processors)
      ;; Find the processor list whose name (first element) is proc-name,
      (let* ((,proc-sym (find-if (lambda (,proc-sym) (equal (car ,proc-sym) ,proc-name)) ,processors-sym))
             ;; and its queue (second element).
             (,queue-name (if ,proc-sym (cadr ,proc-sym))))
        (if ,proc-sym
          ;; Execute the body.
          (setf (cadr ,proc-sym) (progn ,@body))
          ;; Throw an error if we didn't find the queue
          (error (format NIL "Queue for processor ~a not found" ,proc-name)))))))


;; Defines thread-safe accessors for object slots, based on with-slot.
(defmacro defaccessor (class-name accessor-name slot-name)
  `(defmethod-g ,accessor-name ((obj ,class-name))
    (with-slot obj (val ,slot-name)
      ;; Copy sequences in case the returned value is used destructively.
      (if (seqp val)
        (copy-seq val)
        val))))

;; Defines a setter for an above accessor.
(defmacro defsetter (class-name accessor-name slot-name)
  (let ((old-val-n (gensym)))
    `(defmethod-g (setf ,accessor-name) (new-val (obj ,class-name))
      (thread:with-slot obj (,old-val-n ,slot-name)
        (declare (ignore ,old-val-n))
        (setf (slot-value obj ,slot-name) new-val)))))

;; Defines accessor, setter and (optinally) modifier for a slot.
(defmacro defslotinterface (class-name slot-name accessor-name)
  `(progn
     (defaccessor ,class-name ,accessor-name (quote ,slot-name))
     (defsetter ,class-name ,accessor-name (quote ,slot-name))))

;; Wraps a class definition for a threaded-object, adding the correct lock initialiser and interfaces.
(defmacro defclass-threaded (class-name parents slots)
  (let ((slot-names (get-slot-names slots)))
   `(progn
      (cl:defclass ,class-name ,(cons 'threaded-object parents)
        ,slots)
      (defslotints ,class-name ,slot-names))))


;;; Macros to help us define the threaded-object class itself.

;; Generic interfaces with default names (read-slot) for all the slots in slot-names.
(defmacro defslotints (class-name slot-names)
  `(progn
    ,@(loop for slot-name in slot-names collecting
      `(defslotinterface ,class-name ,slot-name ,(mashup-symbol 'read- slot-name)))
    (defmethod initialize-instance :after ((obj ,class-name) &key)
      (setf (slot-value obj 'locks)
        (nconc (slot-value obj 'locks)
          (list ,@(loop for slot-name in slot-names
                        collecting `(cons (quote ,slot-name) (make-recursive-lock)))))))))


;; Creates a class with slots having names from threaded-object-slots, with initialisation forms of NIL, and generic interfaces.
(defmacro defclass-with-slots (class-name parents)
  `(progn
    (defclass ,class-name ,parents
      ,(loop for slot-name in (threaded-object-slots)
             collecting `(,slot-name :initform NIL)))
    (defslotints ,class-name ,(threaded-object-slots))))


;; Take a body form and turn it into a function to call on initialisation.
(defmacro make-init-func (init-form)
  `(lambda () ,init-form))

;; Take a body form and turn it into a function to start/loop it as a processor.
;; In future this should check for signals to pause/stop/start, etc.
(defmacro make-processor-func (loop-form)
  `(lambda () (loop ,loop-form)))


;; Take definitions of processors of the form
;;  (name (:loop-func #'func-name :queue T/NIL :threads N) (:init-func #'func-name))
;; and define the method that constructs them for start-processors to use later. Here,
;; all of the above items, except for the name, are optional. The form for a processor
;; in the 'processors slot is
;;  (name queue loop-func thread-list num-threads init-func)
(defmacro defprocessors (class-name &body forms)
  (let* ((bind-name (gensym)) ; Create a unique name for the object the processors belong to.
         (create-proc (lambda (form) (create-processor form bind-name)))
         (wrapped-processors (mapcar create-proc forms))) ; Create the forms that will go into the object's 'processors slot.
    `(defmethod-g thread:make-processors ((,bind-name ,class-name))
      (setf (slot-value ,bind-name 'processors) ; Put them into the slot.
        (list ,@wrapped-processors)))))


;;; Some helper functions for the above macros.

;; Given a processor definition (see comment for defprocessors), construct the form
;; that will end up as an item in the object's 'processors slot. bind-name is the name of the
;; object that this processor belongs to.
(defun create-processor (proc-form bind-name)
  (let* (;; Options for initialisation forms.
         (pname (car proc-form)) ; The name of the processor.
         (init-form (find-car :init-func proc-form))
         (init-func (getf init-form :init-func)) ; Function to call on initialisation.
         ;; Options for the main loop body of the processor.
         (loop-form (find-car :loop-func proc-form))
         (loop-func (getf loop-form :loop-func)) ; Function to call per loop.
         (should-queue (getf loop-form :queue NIL)) ; Whether this function takes a message from the queue.
         (thread-no (getf loop-form :threads 1))) ; How many threads should there be in the thread pool?
    ;; Create the form itself.
    `(list ',pname ; Name of the processor.
           NIL ; The processor's queue, currently empty.
           ,(create-loop-func loop-func pname bind-name should-queue) ; The loop function, in a form that can be called once.
           NIL ; The processor's thread list, currently empty.
           ,(if loop-form thread-no 0) ; The number of threads, or 0 if there's no loop function.
           ,(create-init-func init-func bind-name) ; The initialisation function.
    )))

;; Given a function to call per loop, and whether it takes a queue message, construct a
;; function that can be called once to run it indefinitely. This function will check the
;; associated queue for internal signals. If the processor itself is a queue consumer
;; (if should-queue), then it should wait for a message anyway, and will be passed one meant
;; for it. If not, it will loop as fast as it can, and won't be passed any messages.
;;  Any errors in the loop function itself will be printed to the console and then subsequently
;; ignored, for the sake of the processor staying alive.
(defun create-loop-func (loop-func pname bind-name should-queue)
  `(lambda ()
    ,(if loop-func
      `(loop named outer-thread-loop
        for msg = (pop-queue ,bind-name ',pname :wait ,should-queue) do
        (if (is-thread-sig msg)
          ,(thread-handle 'msg)
          (handler-case (funcall ,loop-func ,bind-name ,@(if should-queue '(msg)))
            (error (condition) (write-line (format NIL "Error in processor ~S: ~S" ',pname condition)))))))))

;; Check whether the given message is an internal thread signal.
(defun is-thread-sig (msg)
  (member (car msg) (threaded-object-msgs)))

;; Handle an internal thread signal (msg) given to a threaded-object (obj).
(defun thread-handle (msg)
  `(case (car ,msg)
    ;; Return from the enclosing loop, terminating the thread.
    (:thread-internal-stop (return-from outer-thread-loop))))

;; Create an initialisation function, given the function. Will be a null function if
;; init-func is NIL.
(defun create-init-func (init-func bind-name)
  `(lambda () ,(if init-func `(funcall ,init-func ,bind-name))))


(defun get-slot-names (slots)
  (loop for slot in slots
    collecting (if (consp slot)
                  (car slot)
                  slot)))

(defun find-car (match list-in)
  (find-if (lambda (form) (and (consp form) (eq (car form) match))) list-in))
