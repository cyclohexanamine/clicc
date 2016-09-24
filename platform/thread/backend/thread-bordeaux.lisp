;;;; Wraps the Bordeaux thread library into a generic thread interface

(in-package :thread)

(shadowing-import '(bordeaux-threads:make-thread bordeaux-threads:current-thread bordeaux-threads:threadp bordeaux-threads:thread-name
                    bordeaux-threads:make-recursive-lock bordeaux-threads:acquire-recursive-lock bordeaux-threads:release-recursive-lock bordeaux-threads:with-recursive-lock-held))
                    
(export '(make-thread current-thread threadp thread-name
          make-recursive-lock acquire-recursive-lock release-recursive-lock with-recursive-lock-held))
          
(defmacro newthread (fname &rest args)
  `(make-thread
    (lambda ()
      (,fname ,@args))))