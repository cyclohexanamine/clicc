;;;; A generic class with thread-safe slots and an internal thread.

(in-package :thread)

;;; The object itself. Thread will be its internal thread, and queue a message queue for this thread.
;;; Locks is an alist of locks made by (thread:make-lock), with names corresponding to the slots they protect.

(defclass threaded-object ()
  ((thread
    :initform NIL)
   (queue
    :initform NIL)
   locks))

(defun mashup-symbol (&rest objects)
  (intern (format nil "~{~a~}" objects)))


;; Wraps finding the lock to be held based on the name of the slot.
(defmacro with-slot-lock (conn slot &body body)
  `(thread:with-lock-held 
     ((cdr (assoc ,slot (slot-value ,conn 'locks))))
     ,@body))


;; Defines thread-safe accessors for object slots, based on with-slot-lock.
(defmacro defaccessor (class-name accessor-name slot-name)
  `(progn
     (defgeneric ,accessor-name (obj))
     (defmethod ,accessor-name ((obj ,class-name))
       (with-slot-lock obj ,slot-name
         (slot-value obj ,slot-name)))))
         
;; Defines a setter for an above accessor.
(defmacro defsetter (class-name accessor-name slot-name)
  `(progn
     (defgeneric (setf ,accessor-name) (val obj))
     (defmethod (setf ,accessor-name) (val (obj ,class-name))
       (with-slot-lock obj ,slot-name
         (setf (slot-value obj ,slot-name) val)))))

;; Defines a read-and-set pattern for a value.
(defmacro defmodifier (class-name modifier-name slot-name)
  `(progn
     (defgeneric ,modifier-name (obj modifier))
     (defmethod ,modifier-name ((obj ,class-name) modifier)
       (with-slot-lock obj ,slot-name
         (let* ((old-val (slot-value obj ,slot-name))
               (new-val (funcall modifier old-val)))
           (setf (slot-value obj ,slot-name) new-val))))))

;; Defines accessor, setter and (optinally) modifier for a slot.
(defmacro defslotinterface (class-name slot-name accessor-name &optional (modifier-name NIL modifier-name-supplied-p))
  `(progn
     (defaccessor ,class-name ,accessor-name (quote ,slot-name))
     (defsetter ,class-name ,accessor-name (quote ,slot-name))
     ,(if modifier-name-supplied-p `(defmodifier ,class-name ,modifier-name (quote ,slot-name)))))
     
(defmacro defslotints (class-name slot-names)
  `(progn
    ,@(loop for slot-name in slot-names collecting
      `(defslotinterface ,class-name ,slot-name ,(mashup-symbol 'read- slot-name) ,(mashup-symbol 'modify- slot-name)))))
     
     
(defslotints threaded-object (thread queue locks))

(defgeneric push-queue (obj msg))
(defmethod push-queue ((obj threaded-object) msg)
  (modify-queue obj 
    (lambda (queue) 
      (append queue (list msg)))))
      
(defgeneric pop-queue (obj))
(defmethod pop-queue ((obj threaded-object))
  (with-slot-lock obj 'queue 
    (let* ((queue (slot-value obj 'queue))
           (msg (car queue))
           (new-queue (cdr queue)))
      (setf (slot-value obj 'queue) new-queue)
      msg)))


;; Wraps a class definition for a threaded-object, adding the correct lock initialiser and interfaces.
(defmacro defclass-threaded (class-name parents slots)
  (let ((slot-names (get-slot-names slots)))
   `(progn
      (cl:defclass ,class-name ,(cons 'threaded-object parents)
        ,(append slots `((locks
                          :initform (loop for lock-name in (quote ,(append slot-names '(thread queue locks)))
                                           collecting (cons lock-name (make-lock)))))))
      (defslotints ,class-name ,slot-names))))

(defun get-slot-names (slots)
  (loop for slot in slots
    collecting (if (consp slot)
                   (car slot)
                   slot)))