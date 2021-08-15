(cl:in-package #:cl-ds.dicts.skip-list)


(defclass mutable-skip-list-dictionary (cl-ds.dicts:mutable-dictionary
                                        cl-ds.common.skip-list:fundamental-skip-list)
  ())


(defclass mutable-skip-list-dictionary-range (cl-ds.common.skip-list:fundamental-skip-list-range)
  ())


(defmethod cl-ds:whole-range ((object mutable-skip-list-dictionary))
  (make 'mutable-skip-list-dictionary-range
        :current-node (~> object
                          cl-ds.common.skip-list:read-pointers
                          (aref 0))))


(defmethod cl-ds.common.skip-list:make-range ((container mutable-skip-list-dictionary)
                                              current-node
                                              last-node)
  (make 'mutable-skip-list-dictionary-range
        :current-node current-node
        :last-node last-node))


(defmethod cl-ds:consume-front ((range mutable-skip-list-dictionary-range))
  (let ((result (call-next-method)))
    (if (null result)
        (values nil nil)
        (values (cons (cl-ds.common.skip-list:skip-list-node-content result)
                      (cl-ds.common.skip-list:assoc-skip-list-node-value result))
                t))))


(defmethod cl-ds:peek-front ((range mutable-skip-list-dictionary-range))
  (let ((result (call-next-method)))
    (if (null result)
        (values nil nil)
        (cons (cl-ds.common.skip-list:skip-list-node-content result)
              (cl-ds.common.skip-list:assoc-skip-list-node-value result)))))


(defmethod cl-ds:traverse ((range mutable-skip-list-dictionary-range)
                           function)
  (ensure-functionf function)
  (call-next-method range
                    (lambda (node)
                      (declare (type cl-ds.common.skip-list:skip-list-node
                                     node)
                               (optimize (speed 3)))
                      (~>> (cl-ds.common.skip-list:assoc-skip-list-node-value node)
                           (cons (cl-ds.common.skip-list:skip-list-node-content node))
                           (funcall function))))
  range)


(defmethod cl-ds:across ((range mutable-skip-list-dictionary-range)
                         function)
  (ensure-functionf function)
  (call-next-method range
                    (lambda (node)
                      (declare (type cl-ds.common.skip-list:skip-list-node
                                     node)
                               (optimize (speed 3)))
                      (~>> (cl-ds.common.skip-list:assoc-skip-list-node-value node)
                           (cons (cl-ds.common.skip-list:skip-list-node-content node))
                           (funcall function))))
  range)


(defmethod cl-ds:at ((container mutable-skip-list-dictionary)
                     location
                     &rest more-locations)
  (cl-ds:assert-one-dimension more-locations)
  (let* ((pointers (cl-ds.common.skip-list:skip-list-locate-node container
                                                                 location))
         (result (aref pointers 0)))
    (when (null result)
      (return-from cl-ds:at (values nil nil)))
    (let ((content (cl-ds.common.skip-list:skip-list-node-content result)))
      (if (~> container cl-ds.common.skip-list:access-test-function
              (funcall content location))
          (values (cl-ds.common.skip-list:assoc-skip-list-node-value result) t)
          (values nil nil)))))


(defun make-mutable-skip-list-dictionary (ordering test &key (maximum-level 32))
  (check-type maximum-level positive-fixnum)
  (make-instance 'mutable-skip-list-dictionary
                 :ordering-function ordering
                 :maximum-level maximum-level
                 :test-function test
                 :pointers (make-array maximum-level
                                       :initial-element nil)))


(defmethod cl-ds:make-from-traversable (traversable
                                        (class (eql 'mutable-skip-list-dictionary))
                                        &rest arguments)
  (lret ((result (apply #'make-mutable-skip-list-dictionary arguments)))
    (cl-ds:traverse traversable
                    (lambda (x)
                      (bind (((key . value) x))
                        (setf (cl-ds:at result key) value))))))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:insert!-function)
                                             (structure mutable-skip-list-dictionary)
                                             container
                                             location
                                             &rest all
                                             &key value)
  (declare (ignore all))
  (cl-ds.common.skip-list:insert-or
   structure
   location
   t
   t
   value))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:update!-function)
                                             (structure mutable-skip-list-dictionary)
                                             container
                                             location
                                             &rest all
                                             &key value)
  (declare (ignore all))
  (cl-ds.common.skip-list:insert-or
   structure
   location
   nil
   t
   value))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:add!-function)
                                             (structure mutable-skip-list-dictionary)
                                             container
                                             location
                                             &rest all
                                             &key value)
  (declare (ignore all))
  (cl-ds.common.skip-list:insert-or
   structure
   location
   t
   nil
   value))
