(cl:in-package #:cl-ds.sets.skip-list)


(defclass mutable-skip-list-set (cl-ds.sets:mutable-set
                                 cl-ds.common.skip-list:fundamental-skip-list)
  ())


(defclass mutable-skip-list-set-range (cl-ds.common.skip-list:fundamental-skip-list-range)
  ())


(defmethod cl-ds.common.skip-list:make-range ((container mutable-skip-list-set)
                                              current-node
                                              last-node)
  (make 'mutable-skip-list-set-range
        :current-node current-node
        :last-node last-node))


(defmethod cl-ds:whole-range ((object mutable-skip-list-set))
  (make-instance 'mutable-skip-list-set-range
                 :current-node (~> object
                                   cl-ds.common.skip-list:read-pointers
                                   (aref 0))))


(defmethod cl-ds:consume-front ((range mutable-skip-list-set-range))
  (let ((result (call-next-method)))
    (if (null result)
        (values nil nil)
        (values (cl-ds.common.skip-list:skip-list-node-content result)
                t))))


(defmethod cl-ds:peek-front ((range mutable-skip-list-set-range))
  (let ((result (call-next-method)))
    (if (null result)
        (values nil nil)
        (values (cl-ds.common.skip-list:skip-list-node-content result)
                t))))


(defmethod cl-ds:traverse ((range mutable-skip-list-set-range)
                           function)
  (ensure-functionf function)
  (call-next-method range
                    (lambda (node)
                      (declare (type cl-ds.common.skip-list:skip-list-node
                                     node)
                               (optimize (speed 3)))
                      (~>> node
                           cl-ds.common.skip-list:skip-list-node-content
                           (funcall function))))
  range)


(defmethod cl-ds:across ((range mutable-skip-list-set-range)
                         function)
  (ensure-functionf function)
  (call-next-method range
                    (lambda (node)
                      (declare (type cl-ds.common.skip-list:skip-list-node
                                     node)
                               (optimize (speed 3)))
                      (~>> node
                           cl-ds.common.skip-list:skip-list-node-content
                           (funcall function))))
  range)


(defmethod cl-ds.meta:position-modification
    ((function cl-ds.meta:put!-function)
     (structure mutable-skip-list-set)
     container
     location
     &rest all)
  (declare (ignore all container))
  (cl-ds.common.skip-list:insert-or
   structure
   location
   (lambda (node)
     (if (null node)
         (let ((result
                 (cl-ds.common.skip-list:make-skip-list-node-of-random-level
                  (cl-ds.common.skip-list:access-maximum-level structure))))
           (setf (cl-ds.common.skip-list:skip-list-node-content result) location)
           (values result
                   cl-ds.common:empty-changed-eager-modification-operation-status))
         (values nil cl-ds.common:empty-eager-modification-operation-status)))))


(defmethod cl-ds:at ((container mutable-skip-list-set)
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
          (values t t)
          (values nil nil)))))


(defun make-mutable-skip-list-set (ordering test &key (maximum-level 32))
  (check-type maximum-level positive-fixnum)
  (make-instance 'mutable-skip-list-set
                 :ordering-function ordering
                 :maximum-level maximum-level
                 :test-function test
                 :pointers (make-array maximum-level
                                       :initial-element nil)))


(defmethod cl-ds:make-from-traversable (traversable
                                        (class (eql 'mutable-skip-list-set))
                                        &rest arguments)
  (lret ((result (apply #'make-mutable-skip-list-set arguments)))
    (cl-ds:traverse traversable
                    (lambda (x) (cl-ds:put! result x)))))


(defmethod cl-ds:lower-bound ((container mutable-skip-list-set)
                              location)
  (if-let ((result-node (aref (cl-ds.common.skip-list:skip-list-locate-node
                               container
                               location)
                              0)))
    (values (cl-ds.common.skip-list:skip-list-node-content result-node) t)
    (values nil nil)))
