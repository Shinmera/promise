(defpackage #:org.shirakumo.promise
  (:use #:cl)
  (:export
   #:clear
   #:promise
   #:state
   #:done-p
   #:succeed
   #:fail
   #:timeout
   #:make
   #:with-promise
   #:pend
   #:tick
   #:tick-all
   #:ensure-promise
   #:after
   #:then
   #:handle
   #:finally
   #:all
   #:any
   #:iterate
   #:each
   #:do-promised
   #:do-times-promised
   #:->
   #:with-handlers))
