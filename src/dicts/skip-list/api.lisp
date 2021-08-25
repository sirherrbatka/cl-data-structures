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



(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:grow-function)
                                             (structure mutable-skip-list-dictionary)
                                             container
                                             location
                                             &rest all
                                             &key value)
  (cl-ds.common.skip-list:insert-or structure location
                                    (lambda (node)
                                      (if (null node)
                                          (bind (((:values bucket status) (apply #'cl-ds.meta:make-bucket
                                                                                 operation
                                                                                 container
                                                                                 value
                                                                                 all)))
                                            (if (cl-ds:changed status)
                                                (let ((result (~> container
                                                                  cl-ds.common.skip-list:access-maximum-level
                                                                  (cl-ds.common.skip-list:make-skip-list-node-of-random-level value))))
                                                  (setf (cl-ds.common.skip-list:skip-list-node-content result) location)
                                                  (values result status))
                                                (values nil status)))
                                          (bind (((:values bucket status) (apply #'cl-ds.meta:alter-bucket!
                                                                                 operation container
                                                                                 value
                                                                                 (cl-ds.common.skip-list:assoc-skip-list-node-value node)
                                                                                 all)))
                                            (when (cl-ds:changed status)
                                              (setf (cl-ds.common.skip-list:assoc-skip-list-node-value node) bucket))
                                            (values node status))))))


(defmethod cl-ds.meta:position-modification ((function cl-ds.meta:shrink-function)
                                             (structure mutable-skip-list-dictionary)
                                             container
                                             location
                                             &rest all)
  (bind ((pointers (cl-ds.common.skip-list:read-pointers structure))
         (test (cl-ds.common.skip-list:read-ordering-function structure))
         ((:values current prev)
          (cl-ds.common.skip-list:locate-node pointers location test))
         (result (aref current 0)))
    (when (null result)
      (return-from cl-ds.meta:position-modification
        (values structure
                cl-ds.common:empty-eager-modification-operation-status)))
    (let ((content (cl-ds.common.skip-list:skip-list-node-content result)))
      (if (~> structure access-test-function (funcall content location))
          (bind ((rests (cl-ds.common.skip-list:skip-list-node-pointers result))
                 (level (cl-ds.common.skip-list:skip-list-node-level result))
                 (value (cl-ds.common.skip-list:assoc-skip-list-node-value result))
                 ((:values new-value status)
                  (apply #'cl-ds.meta:alter-bucket! function container nil value all)))
            (unless (cl-ds:changed status)
              (return-from cl-ds.meta:position-modification (values structure status)))
            (unless (cl-ds.meta:null-bucket-p new-value)
              (setf (cl-ds.common.skip-list:assoc-skip-list-node-value result) new-value)
              (return-from cl-ds.meta:position-modification (values structure status)))
            (iterate
              (declare (type fixnum i))
              (for i from (1- level) downto 0)
              (if (eq (aref pointers i) result)
                  (setf (aref pointers i)
                        (if (< i level)
                            (aref rests i)
                            nil))
                  (finish)))
            (when (<= level (length pointers))
              (iterate
                (declare (type fixnum j))
                (for j from 0 below (length prev))
                (for previous = (aref prev j))
                (when (or (null previous)
                          (eq previous result))
                  (next-iteration))
                (iterate
                  (declare (type fixnum i))
                  (for i from 0
                       below (min (cl-ds.common.skip-list:skip-list-node-level previous)
                                  (cl-ds.common.skip-list:skip-list-node-level result)))
                  (for node-at = (cl-ds.common.skip-list:skip-list-node-at previous i))
                  (for rest = (cl-ds.common.skip-list:skip-list-node-at result i))
                  (when (eq node-at result)
                    (setf (cl-ds.common.skip-list:skip-list-node-at previous i)
                          rest)))))
            (values structure status))
          (values structure
                  cl-ds.common:empty-eager-modification-operation-status)))))
