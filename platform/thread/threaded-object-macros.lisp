;;;; Some macros to help definitions for threaded objects.

(in-package :thread)

;; The generic threaded object slots.
(defmacro threaded-object-slots ()
  ''(threads queue locks))


;;; Macros for interface definitions.

;; Wraps finding the lock to be held based on the name of the slot.
(defmacro with-slot-lock (obj slot &body body)
  `(thread:with-lock-held
     ((cdr (assoc ,slot (slot-value ,obj 'locks))))
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

;; Wraps a class definition for a threaded-object, adding the correct lock initialiser and interfaces.
(defmacro defclass-threaded (class-name parents slots)
  (let ((slot-names (get-slot-names slots)))
   `(progn
      (cl:defclass ,class-name ,(cons 'threaded-object parents)
        ,slots)
      (defslotints ,class-name ,slot-names))))


;;; Macros to help us define the threaded-object class itself.

;; Generic interfaces with default names (read-slot, modify-slot) for all the slots in slot-names.
(defmacro defslotints (class-name slot-names)
  `(progn
    ,@(loop for slot-name in slot-names collecting
      `(defslotinterface ,class-name ,slot-name ,(mashup-symbol 'read- slot-name) ,(mashup-symbol 'modify- slot-name)))
    (defmethod initialize-instance :after ((obj ,class-name) &key)
      (setf (slot-value obj 'locks)
        (nconc (slot-value obj 'locks)
          (list ,@(loop for slot-name in slot-names
                        collecting `(cons (quote ,slot-name) (thread:make-lock)))))))))


;; Creates a class with slots having names from threaded-object-slots, with initialisation forms of NIL, and generic interfaces.
(defmacro defclass-with-slots (class-name parents)
  `(progn
    (defclass ,class-name ,parents
      ,(loop for slot-name in (threaded-object-slots)
             collecting `(,slot-name :initform NIL)))
    (defslotints ,class-name ,(threaded-object-slots))))
    
    
;; Builds an internal processor based on a list of forms of
;; (init-form body-form num-threads)
;; Each init form will be evaluated once, and each body-form will loop forever in its own thread, with num-threads separate threads for it.
(defmacro defprocessors (names &body forms)
  `(defmethod thread:make-processor (,names)
    (lambda ()
      ,@(mapcar #'car forms)
      ,@(loop for form in forms collecting
          `(loop repeat ,(caddr form) do
              (modify-threads ,(car names)
                (lambda (threads)
                  (nconc threads (list (thread:newthread loop
                                          ,(cadr form)))))))))))


;;; Some helper macros and macro functions.

(defun mashup-symbol (&rest objects)
  (intern (format nil "~{~a~}" objects)))

(defun get-slot-names (slots)
  (loop for slot in slots
    collecting (if (consp slot)
                   (car slot)
                   slot)))

(defmacro eval-when-compile (&body body)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     ,@body))
