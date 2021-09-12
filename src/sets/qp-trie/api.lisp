(cl:in-package #:cl-data-structures.sets.qp-trie)


(define-condition empty-array-key (cl-ds:invalid-value)
  ()
  (:default-initargs :format-control "Empty array as key in the qp-trie-set.~%"))


(defclass fundamental-qp-trie-set (cl-data-structures.common.qp-trie:qp-trie)
  ())


(defclass mutable-qp-trie-set (fundamental-qp-trie-set
                               cl-ds.sets:mutable-set)
  ())


(defun make-mutable-qp-trie-set ()
  (make 'mutable-qp-trie-set))


(defmethod cl-ds.meta:position-modification
    ((function cl-ds.meta:put!-function)
     container
     (structure mutable-qp-trie-set)
     location
     &rest all)
  (declare (ignore all))
  (check-type location (simple-array (unsigned-byte 8) (*)))
  (when (emptyp location)
    (error 'empty-array-key :value location))
  (let ((result (cl-ds.common.qp-trie:qp-trie-insert!
                 structure location
                 (cl-ds.common.qp-trie:make-qp-trie-node))))
    (values structure
            (cl-ds.common:make-eager-modification-operation-status
             (not result)
             (if result nil location)
             result))))


(defmethod cl-ds.meta:position-modification
    ((function cl-ds.meta:erase!-function)
     container
     (structure mutable-qp-trie-set)
     location
     &rest all)
  (declare (ignore all))
  (check-type location (simple-array (unsigned-byte 8) (*)))
  (when (emptyp location)
    (error 'empty-array-key :value location))
  (let ((result (cl-ds.common.qp-trie:qp-trie-delete! structure location)))
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
        :root (~> object cl-ds.common.qp-trie:access-root
                  cl-ds.common.qp-trie:qp-trie-node-clone)))


(defmethod cl-ds:reset! ((object mutable-qp-trie-set))
  (setf (cl-ds.common.qp-trie:access-root object)
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


(defclass qp-trie-set-range (cl-ds:fundamental-forward-range)
  ((%stack :initarg :stack
           :accessor stack)
   (%initial-stack :initarg :stack
                   :reader initial-stack)
   (%start :initarg :start
           :reader start)
   (%end :initarg :end
         :reader end)))


(defmethod cl-ds:reset! ((range qp-trie-set-range))
  (setf (stack range) (initial-stack range)))


(defmethod cl-ds:clone ((range qp-trie-set-range))
  (make (class-of range)
        :stack (stack range)
        :start (start range)
        :end (end range)))


(defclass qp-trie-range-stack-cell ()
  ((%more-checks-start :initarg :more-checks-start
                       :reader more-checks-start)
   (%more-checks-end :initarg :more-checks-end
                     :reader more-checks-end)
   (%node :initarg :node
          :reader node)
   (%depth :initarg :depth
           :reader depth)
   (%parents :initarg :parents
             :reader parents))
  (:default-initargs :more-checks-start t
                     :more-checks-end t
                     :parents '()
                     :depth 0))


(defun new-cell (old-cell start end half-byte &optional leaf)
  (let* ((node (node old-cell))
         (more-checks-start (more-checks-start old-cell))
         (more-checks-end (more-checks-end old-cell))
         (i (depth old-cell))
         (parents (parents old-cell))
         (node-p (cl-ds.common.qp-trie:qp-trie-node-present-p node half-byte))
         (leaf-p (cl-ds.common.qp-trie:qp-trie-node-leaf-present-p node half-byte)))
    (when (or (nor node-p leaf-p)
              (xor leaf-p leaf)
              (and (not node-p) (not leaf)))
      (return-from new-cell nil))
    (if (and (cond ((not more-checks-start)
                    t)
                   ((null start)
                    (setf more-checks-start nil)
                    t)
                   ((< (length start) i)
                    (setf more-checks-start nil)
                    t)
                   ((< i (length start))
                    (setf more-checks-start (= (aref start i) half-byte))
                    (<= (aref start i) half-byte))
                   (t nil))
             (cond ((not more-checks-end)
                    t)
                   ((null end)
                    (setf more-checks-end nil)
                    t)
                   ((< i (length end))
                    (setf more-checks-end (= half-byte (aref end i)))
                    (if (and leaf (= (1+ i) (length end)))
                        (< half-byte (aref end i))
                        (<= half-byte (aref end i))))
                   (t nil)))
        (make 'qp-trie-range-stack-cell
              :parents (cons (cons half-byte node) parents)
              :node (if (and leaf leaf-p)
                        nil
                        (cl-ds.common.qp-trie:qp-trie-node-ref node half-byte))
              :depth (1+ i)
              :more-checks-start more-checks-start
              :more-checks-end more-checks-end)
        nil)))


(defmethod cl-ds:consume-front ((range qp-trie-set-range))
  (with-accessors ((stack stack)) range
    (iterate
      (with end = (end range))
      (with start = (start range))
      (when (emptyp stack)
        (leave (values nil nil)))
      (for cell = (pop stack))
      (for node = (node cell))
      (for parents = (parents cell))
      (when (null node)
        (leave (values (cl-ds.common.qp-trie:half-byte-list-to-array
                        parents :key #'car)
                       t)))
      (iterate
        (declare (type fixnum i))
        (for i from 0 below 16)
        (when-let ((new-cell (new-cell cell start end i)))
          (push new-cell stack))
        (when-let ((new-cell (new-cell cell start end i t)))
          (push new-cell stack))))))


(defmethod cl-ds:traverse ((object qp-trie-set-range) function)
  (ensure-functionf function)
  (iterate
    (for (values value more) = (cl-ds:consume-front object))
    (while more)
    (funcall function value)
    (finally (return object))))


(defmethod cl-ds:across ((object qp-trie-set-range) function)
  (cl-ds:traverse (cl-ds:clone object) function)
  object)


(defmethod cl-ds:peek-front ((object qp-trie-set-range))
  (cl-ds:consume-front (cl-ds:clone object)))


(defmethod cl-ds:whole-range ((container fundamental-qp-trie-set))
  (make-instance 'qp-trie-set-range
                  :start nil
                  :end nil
                  :stack (~> container
                             cl-ds.common.qp-trie:access-root
                             (make 'qp-trie-range-stack-cell :node _)
                             list)))


(defmethod cl-ds:between* ((container fundamental-qp-trie-set) &key low high)
  (make-instance 'qp-trie-set-range
                  :start (if (null low)
                             nil
                             (cl-ds.common.qp-trie:array-to-half-byte-array low))
                  :end (if (null high)
                           nil
                           (cl-ds.common.qp-trie:array-to-half-byte-array high))
                  :stack (~> container
                             cl-ds.common.qp-trie:access-root
                             (make 'qp-trie-range-stack-cell :node _)
                             list)))


(defmethod cl-ds.meta:position-modification ((function cl-ds.meta:erase*!-function)
                                             container
                                             (structure mutable-qp-trie-set)
                                             (location qp-trie-set-range)
                                             &rest all)
  (declare (ignore all))
  (iterate
    (with end = (end location))
    (with start = (start location))
    (with stack = (stack location))
    (when (emptyp stack)
      (leave (values nil nil)))
    (for cell = (pop stack))
    (for node = (node cell))
    (for parents = (parents cell))
    (if (null node)
      (bind (((leaf-index . leaf) (first parents)))
        (cl-ds.common.qp-trie:qp-trie-node-unmark-leaf! leaf leaf-index)
        (iterate
          (for (index . node) in (rest parents))
          (for prev-node previous node initially leaf)
          (until (~> prev-node cl-ds.common.qp-trie:qp-trie-node-bitmask zerop))
          (cl-ds.common.qp-trie:qp-trie-node-delete! node index)))
      (iterate
        (declare (type fixnum i))
        (for i from 0 below 16)
        (when-let ((new-cell (new-cell cell start end i)))
          (push new-cell stack))
        (when-let ((new-cell (new-cell cell start end i t)))
          (push new-cell stack))))))


(defmethod cl-ds:size ((set fundamental-qp-trie-set))
  (bind ((size 0)
         ((:labels impl (node))
          (incf size (~> node
                         cl-ds.common.qp-trie:qp-trie-node-store-bitmask
                         logcount))
          (map nil #'impl (cl-ds.common.qp-trie:qp-trie-node-content node))))
    (impl (cl-ds.common.qp-trie:access-root set))
    size))
