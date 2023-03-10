#|
 This file is a part of promise
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.promise)

;; FIXME: We can't use a hash table due to modification during iteration
;;        however, this makes deregistering potentially very expensive.
(defvar *promises* ())

(defun register (promise)
  (push promise *promises*)
  promise)

(defun deregister (promise)
  (setf *promises* (delete promise *promises*))
  promise)

(defun clear ()
  (setf *promises* NIL))

(defmacro with-promise-handling ((promise) &body body)
  (let ((failure (gensym "FAILURE"))
        (done (gensym "DONE")))
    `(let ((,failure NIL)
           (,done NIL))
       (unwind-protect
            (restart-case
                (multiple-value-prog1
                    (handler-bind ((error (lambda (e) (setf ,failure e))))
                      ,@body)
                  (setf ,done T))
              (abort (&optional e)
                :report "Abort the handler and fail the promise."
                (when e
                  (setf ,failure e))))
         (unless ,done
           (fail ,promise ,failure))))))

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
    (format stream "~s" (state promise))
    (unless (done-p promise)
      (let ((lifetime (- (deadline promise) (get-universal-time))))
        (format stream " ~:[ETERNAL~;~ds~]"
                (< lifetime 3600) lifetime)))))

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

(defun succeed (promise &optional value)
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

(defun fail (promise &optional value)
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
    (when constructor
      (with-promise-handling (promise)
        (funcall constructor
                 (lambda (&optional value) (succeed promise value))
                 (lambda (&optional value) (fail promise) value))))
    promise))

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

(defun pend (&key lifetime (success NIL success-p) (failure NIL failure-p))
  (with-promise (s f :lifetime lifetime)
    (when success-p (funcall s success))
    (when failure-p (funcall f failure))))

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
  (when *promises*
    (dolist (promise *promises* T)
      (tick promise time))))

(defgeneric ensure-promise (promise-ish))

(defmethod ensure-promise ((promise promise))
  promise)

(defmethod ensure-promise ((function function))
  (make function))

(defun after (promise &key success failure timeout lifetime)
  (let* ((promise (ensure-promise promise))
         (next (%make lifetime)))
    (flet ((handler (func)
             (lambda (value)
               (with-promise-handling (next)
                 (succeed next (funcall func value)))))
           (thandler (func)
             (lambda ()
               (with-promise-handling (next)
                 (succeed next (funcall func))))))
      (when success
        (push (handler success) (on-success promise)))
      (when failure
        (push (handler failure) (on-failure promise)))
      (when timeout
        (push (thandler timeout) (on-timeout promise)))
      ;; Re-register to ensure the callbacks run asynchronously.
      (when (done-p promise)
        (register promise)))
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
            (loop for promise-ish in promises
                  for promise = (ensure-promise promise-ish)
                  do (ecase (state promise)
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
            (loop for promise-ish in promises
                  for promise = (ensure-promise promise-ish)
                  do (case (state promise)
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

(defun iterate (end-p cur-fun step-fun start function)
  (labels ((next (object)
             (unless (funcall end-p object)
               (let* ((cur (funcall cur-fun object))
                      (next (funcall step-fun object))
                      (result (funcall function cur)))
                 (if (typep result 'promise)
                     (then result
                           (lambda (v)
                             (declare (ignore v))
                             (funcall #'next next)))
                     (then (pend :success next)
                           #'next))))))
    (then (pend :success start) #'next)))

(defun each (sequence function)
  (etypecase sequence
    (null
     (pend :success NIL))
    (list
     (iterate #'null #'car #'rest sequence function))
    (vector
     (let ((length (length sequence)))
       (iterate (lambda (i) (<= length i))
                (lambda (i) (aref sequence i))
                #'1+ 0 function)))
    #+sbcl
    (sequence
     (sb-sequence:with-sequence-iterator (iterator limit from-end-p step endp element) (sequence)
       (declare (ignore from-end-p))
       (iterate (lambda (it) (funcall endp it limit NIL))
                element
                (lambda (it) (funcall step it NIL))
                iterator function)))
    #-sbcl
    (sequence
     (let ((length (length sequence)))
       (iterate (lambda (i) (<= length i))
                (lambda (i) (elt sequence i))
                #'1+ 0 function)))))

(defmacro do-promised ((element sequence) &body body)
  `(each ,sequence (lambda (,element) ,@body)))

(defmacro do-times-promised ((i limit) &body body)
  (let ((limitg (gensym "LIMIT")))
    `(let ((,limitg ,limit))
       (iterate (lambda (it) (<= ,limitg it))
                #'identity #'1+ 0 (lambda (,i) ,@body)))))

(defmacro -> (promise &body promises)
  (if promises
      (destructuring-bind (func . args) (pop promises)
        `(-> ,(ecase func
                ((after :after)
                 `(after ,promise ,@args))
                ((then :then)
                 (let ((arglist (or (first args) (list (gensym "VALUE")))))
                   `(then ,promise (lambda ,arglist
                                     (declare (ignorable ,@arglist))
                                     ,@(rest args)))))
                ((handle :handle)
                 (let ((arglist (or (first args) (list (gensym "VALUE")))))
                   `(handle ,promise (lambda ,arglist
                                       (declare (ignorable ,@arglist))
                                       ,@(rest args)))))
                ((finally :finally)
                 `(finally ,promise (lambda () ,@args))))
           ,@promises))
      promise))
