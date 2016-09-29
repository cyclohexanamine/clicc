;;;; Wraps the Bordeaux thread library into a generic thread interface

(in-package :thread)

(shadowing-import '(bordeaux-threads:make-thread bordeaux-threads:make-recursive-lock bordeaux-threads:with-recursive-lock-held))

(defmacro newthread (thread-name fname &rest args)
  `(make-thread
    (lambda ()
      (,fname ,@args))
    :name ,thread-name))