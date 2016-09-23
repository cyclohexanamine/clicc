;;;; Some macros to help definitions for threaded objects.

(in-package :thread)

;; The generic threaded object slots.
(defmacro threaded-object-slots ()
  ''(threads queue locks))


;;; Macros for interface definitions.

;; Wraps finding the lock to be held based on the name of the slot,
;; and binds the slot value to a given name. If the slot is unbound,
;; NIL will be used instead.
(defmacro with-slot (obj (bind-name slot-name) &body body)
  `(with-lock-held ((cdr (assoc ,slot-name (slot-value ,obj 'locks))))
    (let ((,bind-name 
            (if (slot-boundp ,obj ,slot-name)
              (slot-value ,obj ,slot-name))))
      ,@body)))

;; Defines a read-and-set macro for a slot, in the form of, e.g.,
;;  (modify-slot classabc (xyzval 'xyz)
;;    (do-something xyz)
;;    (value-to-set))
(defmacro modify-slot (obj (bind-name slot-name) &body body)
  `(with-slot ,obj (,bind-name ,slot-name)
    (setf (slot-value ,obj ,slot-name)
      (progn ,@body))))

;; Defines thread-safe accessors for object slots, based on with-slot.
(defmacro defaccessor (class-name accessor-name slot-name)
  `(defmethod ,accessor-name ((obj ,class-name))
    (with-slot obj (val ,slot-name)
      ;; Copy sequences in case the returned value is used destructively.
      (if (typep val 'sequence)
        (copy-seq val)
        val))))

;; Defines a setter for an above accessor.
(defmacro defsetter (class-name accessor-name slot-name)
  `(defmethod (setf ,accessor-name) (new-val (obj ,class-name))
    (thread:with-slot obj (old-val ,slot-name)
      (setf (slot-value obj ,slot-name) new-val))))

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
                        collecting `(cons (quote ,slot-name) (make-lock)))))))))


;; Creates a class with slots having names from threaded-object-slots, with initialisation forms of NIL, and generic interfaces.
(defmacro defclass-with-slots (class-name parents)
  `(progn
    (defclass ,class-name ,parents
      ,(loop for slot-name in (threaded-object-slots)
             collecting `(,slot-name :initform NIL)))
    (defslotints ,class-name ,(threaded-object-slots))))


;; Builds an internal processor based on a list of forms of
;; (init-form body-form num-threads)
;; Each init form will be evaluated once, and each body-form will loop forever
;; in its own thread, with num-threads separate threads for it.
(defmacro defprocessors ((bind-name class-name) &body forms)
  `(defmethod thread:make-processor ((,bind-name ,class-name))
    (lambda ()
      ;; Collect init forms here.
      ,@(mapcar #'car forms)
      ;; Then for each loop form, spawn a new thread however many times requested
      ;; which loops that form forever.
      ,@(loop for form in forms collecting
          `(loop repeat ,(caddr form) do
              (modify-slot ,bind-name (threads 'threads)
                  (nconc threads (list (newthread loop
                                          ,(cadr form))))))))))


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
