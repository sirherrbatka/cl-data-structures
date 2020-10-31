(cl:in-package #:cl-data-structures.sets.qp-trie)


(define-condition empty-array-key (cl-ds:invalid-value)
  ()
  (:default-initargs :format-control "Empty array as key in the qp-trie-set.~%"))


(defclass fundamental-qp-trie-set (cl-data-structures.common.qp-trie:qp-trie)
  ((%size :type non-negative-integer
           :initarg :size
           :reader cl-ds:size
           :accessor access-size))
  (:default-initargs
   :size 0))


(defclass mutable-qp-trie-set (fundamental-qp-trie-set
                               cl-ds.sets:mutable-set)
  ())


(defun make-mutable-qp-trie-set ()
  (make 'mutable-qp-trie-set))


(defmethod cl-ds.meta:position-modification
    ((function cl-ds.meta:put!-function)
     (structure mutable-qp-trie-set)
     container
     location
     &rest all)
  (declare (ignore all))
  (check-type location (simple-array (unsigned-byte 8) (*)))
  (when (emptyp location)
    (error 'empty-array-key :value location))
  (let ((result (cl-ds.common.qp-trie:qp-trie-insert!
                 structure location
                 (cl-ds.common.qp-trie:make-qp-trie-node))))
    (when result (incf (access-size structure)))
    (values structure
            (cl-ds.common:make-eager-modification-operation-status
             (not result)
             (if result nil location)
             result))))


(defmethod cl-ds.meta:position-modification
    ((function cl-ds.meta:erase!-function)
     (structure mutable-qp-trie-set)
     container
     location
     &rest all)
  (declare (ignore all))
  (check-type location (simple-array (unsigned-byte 8) (*)))
  (when (emptyp location)
    (error 'empty-array-key :value location))
  (let ((result (cl-ds.common.qp-trie:qp-trie-delete! structure location)))
    (when result (decf (access-size structure)))
    (values structure
            (cl-ds.common:make-eager-modification-operation-status
             result
             (if result location nil)
             result))))


(defmethod cl-ds:at ((container fundamental-qp-trie-set)
                     location
                     &rest more-locations)
  (cl-ds:assert-one-dimension more-locations)
  (check-type location (simple-array (unsigned-byte 8)))
  (when (emptyp location)
    (error 'empty-array-key :value location))
  (let* ((length (length location))
         (result (cl-ds.common.qp-trie:qp-trie-find container location))
         (result-found (eql length result)))
    (values result-found result-found result)))


(defmethod cl-ds:empty-clone ((container fundamental-qp-trie-set))
  (~> container class-of make))


(defmethod cl-ds:make-from-traversable (traversable
                                        (class (eql 'mutable-qp-trie-set))
                                        &rest arguments)
  (declare (ignore arguments))
  (lret ((result (make 'mutable-qp-trie-set)))
    (cl-ds:across (lambda (key) (cl-ds:put! result key))
                  traversable)))


(defmethod cl-ds:across ((object fundamental-qp-trie-set)
                         function)
  (ensure-functionf function)
  (~>> object cl-ds.common.qp-trie:access-root
       (cl-ds.common.qp-trie:map-qp-trie-nodes function))
  object)


(defmethod cl-ds:clone ((object fundamental-qp-trie-set))
  (make (class-of object)
        :size (cl-ds:size object)
        :root (~> object cl-ds.common.qp-trie:access-root
                  cl-ds.common.qp-trie:qp-trie-node-clone)))


(defmethod cl-ds:reset! ((object mutable-qp-trie-set))
  (setf (access-size object) 0
        (cl-ds.common.qp-trie:access-root object)
        (cl-ds.common.qp-trie:make-qp-trie-node))
  object)


(defun obtain-value (pull push)
  (declare (optimize (speed 3))
           (type function pull push))
  (flet ((push-next (node parents)
           (declare (type list parents)
                    (type cl-ds.common.qp-trie:qp-trie-node node))
           (iterate
             (declare (type fixnum i))
             (for i from 0 below 16)
             (when (cl-ds.common.qp-trie:qp-trie-node-present-p node i)
               (funcall
                push (list (cl-ds.common.qp-trie:qp-trie-node-ref node i)
                           (cons i parents)))))
           (iterate
             (declare (type fixnum i))
             (for i from 0 below 16)
             (when (cl-ds.common.qp-trie:qp-trie-node-leaf-present-p node i)
               (funcall push (list nil (cons i parents)))))))
    (iterate
      (for (node parents) = (funcall pull))
      (when (null node)
        (leave (cl-ds.common.qp-trie:half-byte-list-to-array parents)))
      (push-next node parents))))


(defmethod cl-ds:whole-range ((container fundamental-qp-trie-set))
  (make-instance 'cl-ds.common:forward-tree-range
                 :obtain-value #'obtain-value
                 :forward-stack (~> container
                                    cl-ds.common.qp-trie:access-root
                                    (list '())
                                    list)
                 :key #'identity
                 :container container))
