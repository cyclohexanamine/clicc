;;;; Wraps the Bordeaux thread library into a generic thread interface

(in-package :thread)

(shadowing-import '(bordeaux-threads:make-thread bordeaux-threads:current-thread bordeaux-threads:threadp bordeaux-threads:thread-name
                    bordeaux-threads:make-lock bordeaux-threads:acquire-lock bordeaux-threads:release-lock bordeaux-threads:with-lock-held))
                    
(export '(make-thread current-thread threadp thread-name
          make-lock acquire-lock release-lock with-lock-held))