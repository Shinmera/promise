#|
 This file is a part of promise
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.promise)

(defvar *promises* (make-hash-table :test 'eq))

(defun register (promise)
  (setf (gethash promise *promises*) promise))

(defun deregister (promise)
  (remhash promise *promises*))

(defun clear ()
  (clrhash *promises*))

(defstruct (promise
            (:constructor %%make (deadline &key on-success on-failure on-timeout))
            (:conc-name NIL)
            (:predicate NIL)
            (:copier NIL))
  (state :pending :type symbol)
  (deadline 0 :type (unsigned-byte 64) :read-only T)
  (value NIL)
  (on-success () :type list)
  (on-failure () :type list)
  (on-timeout () :type list))

(defmethod print-object ((promise promise) stream)
  (print-unreadable-object (promise stream :type T :identity T)
    (format stream "~s" (state promise))))

(defun %make (lifetime)
  (register (%%make (if lifetime (+ (get-universal-time) lifetime) most-positive-fixnum))))

(defun done-p (promise)
  (not (eq :pending (state promise))))

(defun chain (promise predecessor)
  (ecase (state predecessor)
    (:pending
     (push (lambda (v) (succeed promise v)) (on-success predecessor))
     (push (lambda (v) (fail promise v)) (on-failure predecessor))
     (push (lambda () (timeout promise)) (on-timeout predecessor)))
    (:success
     (succeed promise (value predecessor)))
    (:failure
     (fail promise (value predecessor)))
    (:timeout
     (timeout promise))))

(defun succeed (promise value)
  (ecase (state promise)
    (:pending
     (cond ((typep value 'promise)
            (chain promise value))
           (T
            (setf (value promise) value
                  (state promise) :success))))
    (:timeout
     #|Too late, don't care.|#)
    ((:success :failure)
     (error "The promise~%  ~a~%is already done." promise)))
  promise)

(defun fail (promise value)
  (ecase (state promise)
    (:pending
     (cond ((typep value 'promise)
            (chain promise value))
           (T
            (setf (value promise) value
                  (state promise) :failure))))
    (:timeout
     #|Too late, don't care.|#)
    ((:success :failure)
     (error "The promise~%  ~a~%is already done." promise)))
  promise)

(defun timeout (promise)
  (ecase (state promise)
    (:pending
     (setf (state promise) :timeout))
    (:timeout
     #|Too late, don't care.|#)
    ((:success :failure)
     (error "The promise~%  ~a~%is already done." promise)))
  promise)

(defun make (&optional constructor &key lifetime)
  (let ((promise (%make lifetime)))
    (flet ((succeed (&optional value)
             (succeed promise value))
           (fail (&optional value)
             (fail promise value)))
      (when constructor
        (handler-case
            (funcall constructor #'succeed #'fail)
          (error (e)
            (fail e))))
      promise)))

(defmacro with-promise ((succeed &optional fail &key lifetime) &body body)
  (let ((fail (or fail (gensym "FAIL"))))
    `(make (lambda (,succeed ,fail)
             (declare (ignorable ,succeed ,fail))
             (flet ((,succeed (&optional value)
                      (funcall ,succeed value))
                    (,fail (&optional value)
                        (funcall ,fail value)))
               (declare (ignorable #',succeed #',fail))
               ,@body))
           :lifetime ,lifetime)))

(defun pend (&key lifetime success failure)
  (with-promise (s f :lifetime lifetime)
    (when success (funcall s success))
    (when failure (funcall f failure))))

(defun tick (promise time)
  (ecase (state promise)
    (:pending
     (when (<= (deadline promise) time)
       (timeout promise)
       (tick promise time)))
    (:success
     (loop with value = (value promise)
           for fun = (pop (on-success promise))
           while fun do (funcall fun value))
     (deregister promise))
    (:failure
     (loop with value = (value promise)
           for fun = (pop (on-failure promise))
           while fun do (funcall fun value))
     (deregister promise))
    (:timeout
     (loop for fun = (pop (on-timeout promise))
           while fun do (funcall fun))
     (deregister promise))))

(defun tick-all (time)
  (let ((res NIL))
    (loop for promise being the hash-keys of *promises*
          do (tick promise time)
             (setf res T))
    res))

(defun after (promise &key success failure timeout lifetime)
  (let ((next (%make lifetime)))
    (flet ((handler (func)
             (lambda (value)
               (handler-case
                   (succeed next (funcall func value))
                 (error (e)
                   (fail next e)))))
           (thandler (func)
             (lambda ()
               (handler-case
                   (succeed next (funcall func))
                 (error (e)
                   (fail next e))))))
      (case (state promise)
        (:pending
         (when success
           (push (handler success) (on-success promise)))
         (when failure
           (push (handler failure) (on-failure promise)))
         (when timeout
           (push (thandler timeout) (on-timeout promise))))
        (:success
         (when success
           (funcall (handler success) (value promise))))
        (:failure
         (when failure
           (funcall (handler failure) (value promise))))
        (:timeout
         (when timeout
           (funcall (thandler timeout) (value promise))))))
    next))

(defun then (promise on-success)
  (after promise :success on-success))

(defun handle (promise on-failure)
  (after promise :failure on-failure))

(defun finally (promise on-done)
  (flet ((wrap (v)
           (declare (ignore v))
           (funcall on-done)))
    (after promise :success #'wrap :failure #'wrap :timeout on-done)))

(defun all (promises &key lifetime)
  (let ((count (length promises)))
    (make (lambda (ok fail)
            (dolist (promise promises)
              (ecase (state promise)
                (:pending
                 (push (lambda (v)
                         (declare (ignore v))
                         (when (= 0 (decf count))
                           (funcall ok (mapcar #'value promises))))
                       (on-success promise))
                 (push (lambda (e)
                         (ignore-errors (funcall fail e)))
                       (on-failure promise)))
                (:success
                 (when (= 0 (decf count))
                   (funcall ok (mapcar #'value promises))))
                (:failure
                 (funcall fail (value promise)))
                (:timeout))))
          :lifetime lifetime)))

(defun any (promises &key lifetime)
  (let ((count (length promises)))
    (make (lambda (ok fail)
            (dolist (promise promises)
              (case (state promise)
                (:pending
                 (push (lambda (v)
                         (ignore-errors (funcall ok v)))
                       (on-success promise))
                 (push (lambda (e)
                         (when (= 0 (decf count))
                           (funcall fail e)))
                       (on-failure promise)))
                (:success
                 (funcall ok (value promise)))
                (:failure
                 (when (= 0 (decf count))
                   (funcall fail (value promise))))
                (:timeout))))
          :lifetime lifetime)))

(defmacro -> (&rest promises)
  (if (rest promises)
      (let ((first (pop promises)))
        (destructuring-bind (func . args) (pop promises)
          `(-> ,(ecase func
                  (after `(after ,first ,@args))
                  (then `(then ,first (lambda ,@args)))
                  (handle `(handle ,first (lambda ,@args)))
                  (finally `(finally ,first (lambda () ,@args))))
               ,@promises)))
      (first promises)))
