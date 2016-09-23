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