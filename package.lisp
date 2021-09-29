#|
 This file is a part of promise
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

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
   #:after
   #:then
   #:handle
   #:finally
   #:all
   #:any
   #:iterate
   #:each
   #:do-promised
   #:->))
