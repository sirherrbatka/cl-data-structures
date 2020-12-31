(cl:in-package #:cl-data-structures.common)


(defclass forward-tree-range (cl-ds:fundamental-forward-range)
  ((%forward-stack :type list
                   :accessor access-forward-stack
                   :initarg :forward-stack
                   :initform nil)
   (%obtain-value :type (-> ((-> () t) (-> (t) t)) (values t boolean))
                  :initarg :obtain-value
                  :reader read-obtain-value)
   (%initial-stack :type list
                   :initarg :initial-stack
                   :reader read-initial-stack
                   :initform nil)
   (%key :type (-> (t) t)
         :initarg :key
         :reader read-key)
   (%container :type cl-ds:fundamental-container
               :initarg :container
               :reader read-container)))


(defmethod cl-ds.utils:cloning-information
    append ((object forward-tree-range))
  '((:obtain-value read-obtain-value)
    (:initial-stack access-forward-stack)
    (:forward-stack access-forward-stack)
    (:key read-key)
    (:container read-container)))


(defclass assignable-forward-tree-range
    (cl-ds:fundamental-assignable-forward-range forward-tree-range)
  ((%store-value :type (-> (t t) t)
                 :initarg :store-value
                 :reader read-store-value)))


(declaim (inline read-implementation))
(defun read-implementation (stack obtain-value)
  (if (null stack)
      (values stack nil nil)
      (bind (((:dflet push-to-stack (x))
              (push x stack))
             ((:dflet pop-stack ())
              (when (endp stack)
                (return-from read-implementation
                  (values stack nil nil)))
              (pop stack))
             (result (funcall obtain-value #'pop-stack #'push-to-stack)))
        (values stack result t))))


(defmethod cl-ds:across ((range forward-tree-range) function)
  (let* ((stack (access-forward-stack range))
         (obtain-value (read-obtain-value range))
         (key (read-key range)))
    (iterate
      (until (null stack))
      (multiple-value-bind (new-stack value)
          (read-implementation stack obtain-value)
        (setf stack new-stack)
        (funcall function (funcall key value))))
    range))


(defmethod cl-ds:traverse ((range forward-tree-range) function)
  (bind (((:accessors (stack access-forward-stack))
          range)
         (obtain-value (read-obtain-value range))
         (key (read-key range)))
    (iterate
      (until (null stack))
      (multiple-value-bind (new-stack value)
          (read-implementation stack obtain-value)
        (setf stack new-stack
              (access-forward-stack range) stack)
        (funcall function (funcall key value))))
    range))


(defmethod cl-ds:peek-front ((range forward-tree-range))
  (bind (((:accessors (stack access-forward-stack)
                      (obtain-value read-obtain-value)
                      (key read-key))
          range)
         ((:values _ result found)
          (read-implementation stack
                               obtain-value)))
    (values (when found (funcall key result)) found)))


(defmethod (setf cl-ds:peek-front)
    (new-value (range assignable-forward-tree-range))
  (bind (((:accessors (stack access-forward-stack)
                      (obtain-value read-obtain-value)
                      (container read-container)
                      (key read-key)
                      (store-value read-store-value))
          range)
         ((:values stack result found)
          (read-implementation stack
                               obtain-value)))
    (declare (ignore result))
    (let ((stack-front (first stack)))
      (when found
        (funcall store-value container new-value)
        (setf (elt stack-front 1) t
              (elt stack-front 2) new-value))
      (values (when found new-value found)))))


(defmethod cl-ds:consume-front ((range forward-tree-range))
  (bind (((:accessors (stack access-forward-stack)
                      (obtain-value read-obtain-value)
                      (mutex read-mutex)
                      (key read-key))
          range)
         ((:values new-stack result found)
          (read-implementation stack obtain-value)))
    (setf stack new-stack)
    (values (when found (funcall key result)) found)))


(defmethod cl-ds:drop-front ((range forward-tree-range) count)
  (let ((stack (access-forward-stack range))
        (obtain-value (read-obtain-value range)))
    (iterate
      (until (null stack))
      (repeat count)
      (setf stack (nth-value 1 (read-implementation stack
                                                    obtain-value))))
    (setf (access-forward-stack range) stack))
  range)


(defmethod cl-ds:clone ((range forward-tree-range))
  (cl-ds.utils:clone range))


(defmethod cl-ds:reset! ((range forward-tree-range))
  (bind (((:slots %initial-stack %forward-stack %mutex) range))
    (setf %forward-stack (mapcar #'cl-ds:clone %initial-stack)))
  range)


(defmacro defmethod-with-stack ((method-name lambda-list stack stack-place)
                                &body body)
  `(defmethod ,method-name ,lambda-list
     (symbol-macrolet ((,stack ,stack-place))
       (labels ((,method-name ,(cl-ds.utils:method-lambda-list-to-function-lambda-list
                                lambda-list)
                  ,@body))
         (,method-name ,@(cl-ds.utils:lambda-list-to-bindings lambda-list))))))


(defmacro defmethod-with-peek-stack ((method-name lambda-list stack init)
                                     &body body)
  `(defmethod ,method-name ,lambda-list
     (let ((,stack ,init))
       (labels ((,method-name ,(cl-ds.utils:method-lambda-list-to-function-lambda-list
                                lambda-list)
                  ,@body))
         (,method-name ,@(cl-ds.utils:lambda-list-to-bindings lambda-list))))))
