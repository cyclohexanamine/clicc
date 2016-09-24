;;;; Some macros to help definitions for threaded objects.

(in-package :thread)

;; The generic threaded object slots.
(defmacro threaded-object-slots ()
  ''(threads queue locks processors))


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
  `(defmethod-g (setf ,accessor-name) (new-val (obj ,class-name))
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


;; Builds internal processors based on a list of forms of
;; (name init-form body-form num-threads)
;; Each init form will be evaluated once, and each body-form will loop forever
;; in its own thread, with num-threads separate threads for it.
(defmacro defprocessors ((bind-name class-name) &body forms)
  ;; First, wrap the forms given in lambdas.
  (let ((wrapped-forms (loop for form in forms collecting
                        `(list ,(car form)
                               (make-init-func ,(cadr form))
                               (make-processor-func ,(caddr form))
                               ,(cadddr form)
                               NIL))))
    ;; Then define the make-processors method populating the processors slot with
    ;; these processors.
    `(defmethod-g thread:make-processors ((,bind-name ,class-name))
      (setf (slot-value ,bind-name 'processors)
        (list ,@wrapped-forms)))))


;;; Some helper macros and macro functions.

(defun get-slot-names (slots)
  (loop for slot in slots
    collecting (if (consp slot)
                  (car slot)
                  slot)))
