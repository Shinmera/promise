#|
 This file is a part of promise
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.promise.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:promise #:org.shirakumo.promise))
  (:export #:promise))

(in-package #:org.shirakumo.promise.test)

(define-test promise)

(define-test constructor
  :parent promise
  (of-type promise:promise (promise:make))
  (is eq :pending (promise:state (promise:with-promise (succeed fail))))
  (is eq :success (promise:state (promise:with-promise (succeed fail) (funcall succeed))))
  (is eq :failure (promise:state (promise:with-promise (succeed fail) (funcall fail)))))

(define-test state-transitions
  :parent promise
  :depends-on (constructor)
  (group (success)
    (let ((promise (promise:pend)))
      (is eq :pending (promise:state promise))
      (is eq :success (promise:state (promise:succeed promise NIL)))
      (fail (promise:succeed promise NIL))
      (fail (promise:fail promise NIL))
      (fail (promise:timeout promise))))
  (group (failure)
    (let ((promise (promise:pend)))
      (is eq :pending (promise:state promise))
      (is eq :failure (promise:state (promise:fail promise NIL)))
      (fail (promise:succeed promise NIL))
      (fail (promise:fail promise NIL))
      (fail (promise:timeout promise))))
  (group (timeout)
    (let ((promise (promise:pend)))
      (is eq :pending (promise:state promise))
      (is eq :timeout (promise:state (promise:timeout promise)))
      (finish (promise:succeed promise NIL))
      (finish (promise:fail promise NIL))
      (finish (promise:timeout promise)))))

(define-test handler-invocation
  :parent promise
  :depends-on (state-transitions)
  (group (success)
    (let ((promise (promise:pend))
          success failure timeout)
      (of-type promise:promise
               (promise:after promise :success (lambda (v) (setf success v))
                                      :failure (lambda (v) (setf failure v))
                                      :timeout (lambda () (setf timeout :x))))
      (finish (promise:succeed promise :x))
      (isnt eq :x success)
      (finish (promise:tick promise 0))
      (is eq :x success)
      (isnt eq :x failure)
      (isnt eq :x timeout)))
  (group (failure)
    (let ((promise (promise:pend))
          success failure timeout)
      (of-type promise:promise
               (promise:after promise :success (lambda (v) (setf success v))
                                      :failure (lambda (v) (setf failure v))
                                      :timeout (lambda () (setf timeout :x))))
      (finish (promise:fail promise :x))
      (isnt eq :x failure)
      (finish (promise:tick promise 0))
      (isnt eq :x success)
      (is eq :x failure)
      (isnt eq :x timeout)))
  (group (timeout)
    (let ((promise (promise:pend))
          success failure timeout)
      (of-type promise:promise
               (promise:after promise :success (lambda (v) (setf success v))
                                      :failure (lambda (v) (setf failure v))
                                      :timeout (lambda () (setf timeout :x))))
      (finish (promise:timeout promise))
      (isnt eq :x failure)
      (finish (promise:tick promise 0))
      (isnt eq :x success)
      (isnt eq :x failure)
      (is eq :x timeout))))

(define-test timeout
  :parent promise
  :depends-on (handler-invocation)
  (let ((promise (promise:pend :lifetime -1)))
    (is eq :pending (promise:state promise))
    (promise:tick promise (get-universal-time))
    (is eq :timeout (promise:state promise)))
  (let ((promise (promise:pend :lifetime 1)))
    (is eq :pending (promise:state promise))
    (promise:tick promise (+ 0 (get-universal-time)))
    (is eq :pending (promise:state promise))
    (promise:tick promise (+ 1 (get-universal-time)))
    (is eq :timeout (promise:state promise))))

(define-test chain
  :parent promise
  :depends-on (handler-invocation)
  (let* ((first (promise:pend))
         (second (promise:with-promise (succeed)
                   (succeed first)))
         value)
    (promise:then second (lambda (v) (setf value v)))
    (is eq :pending (promise:state first))
    (is eq :pending (promise:state second))
    (promise:succeed first :x)
    (is eq :success (promise:state first))
    (is eq :pending (promise:state second))
    (promise:tick first 0)
    (promise:tick second 0)
    (is eq :success (promise:state first))
    (is eq :success (promise:state second))
    (is eq :x value)))

(define-test registry
  :parent promise
  :depends-on (state-transitions)
  (finish (promise:clear))
  (let ((promise (promise:pend)))
    (true (promise:tick-all 0))
    (true (promise:tick-all 0))
    (finish (promise:succeed promise :x))
    (true (promise:tick-all 0))
    (false (promise:tick-all 0))
    (finish (promise:after promise :success (lambda (v) v)))
    (true (promise:tick-all 0))
    (false (promise:tick-all 0))))

(define-test combinators
  :parent promise
  :depends-on (handler-invocation)
  (group (all)
    (let* ((a (promise:pend))
           (b (promise:pend))
           (promise (finish (promise:all (list a b)))))
      (is eq :pending (promise:state promise))
      (finish (promise:succeed a :x))
      (is eq :pending (promise:state promise))
      (finish (promise:succeed b :y))
      (is eq :pending (promise:state promise))
      (promise:tick a 0)
      (promise:tick b 0)
      (is eq :success (promise:state promise))
      (is equal '(:x :y) (promise::value promise))))
  (group (any)
    (let* ((a (promise:pend))
           (b (promise:pend))
           (promise (finish (promise:any (list a b)))))
      (is eq :pending (promise:state promise))
      (finish (promise:succeed a :x))
      (is eq :pending (promise:state promise))
      (promise:tick a 0)
      (is eq :success (promise:state promise))
      (is eq :x (promise::value promise))
      (finish (promise:succeed b :y))
      (promise:tick b 0)
      (is eq :success (promise:state promise))
      (is eq :x (promise::value promise)))))
