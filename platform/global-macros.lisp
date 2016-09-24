(in-package :mac)

;; Is val a sequence?
(defmacro seqp (val)
  `(typep ,val 'sequence))

;; Wraps defining a generic function implicitly in a method definition.
(defmacro defmethod-g (fname args &body body)
  (let ((genargs (loop for arg in args collecting
                  ;; If the argument is a list (i.e., has type specifier)
                  ;; take the first element (i.e., intended name) only.
                  (if (seqp arg)
                    (car arg)
                    arg))))
  `(progn
    ;; Define a generic function for the method if there isn't already one.
    ,(if (not (fboundp fname))
      `(defgeneric ,fname ,genargs))
    ;; Define the method itself.
    (defmethod ,fname ,args ,@body))))


;; Concatenate the given symbols literally.
(defun mashup-symbol (&rest objects)
  (intern (format nil "~{~a~}" objects)))

;; Quote a symbol at macro expansion time (i.e., prepend ' to it).
(defun mquote (sym) `',sym)

;; Evaluate body at compile time.
(defmacro eval-when-compile (&body body)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     ,@body))
     
;; Zips a list of lists.
(defmacro zipcar (listlist)
  `(apply #'mapcar #'list ,listlist))
  
(defmacro nullf (&rest args)
  `(lambda (,@args) (declare (ignore ,@args))))
  
(defmacro defnullf (fname args)
  `(defun ,fname (,@args)
    (declare (ignore ,@args))))