(cl:in-package #:cl-data-structures.dicts.srrb)


(defmethod cl-ds.meta:position-modification
    ((operation cl-ds.meta:grow-function)
     (structure transactional-sparse-rrb-vector)
     container
     position &rest all &key value)
  (transactional-sparse-rrb-vector-grow
   operation structure container position all value))


(defmethod cl-ds:replica ((container transactional-sparse-rrb-vector)
                          &optional isolate)
  (cl-ds.common.abstract:replica container isolate))


(defmethod cl-ds.meta:position-modification
    ((operation cl-ds.meta:grow-function)
     (structure mutable-sparse-rrb-vector)
     container
     position &rest all &key value)
  (mutable-sparse-rrb-vector-grow
   operation structure container position all value))


(defmethod cl-ds.meta:position-modification
    ((operation cl-ds.meta:grow-function)
     (structure functional-sparse-rrb-vector)
     container
     position &rest all &key value)
  (functional-sparse-rrb-vector-grow
   operation structure container position all value))


(defmethod cl-ds.meta:position-modification
    ((operation cl-ds.meta:shrink-function)
     (structure mutable-sparse-rrb-vector)
     container
     position &rest all)
  (declare (optimize (speed 3) (space 0) (debug 0))
           (type integer position))
  (let ((tree-bound (access-tree-index-bound structure)))
    (declare (type fixnum tree-bound))
    (cond ((negative-integer-p position)
           (error 'cl-ds:argument-value-out-of-bounds
                  :argument 'position
                  :value position
                  :bounds "Must be non-negative."
                  :format-control "Sparse vector index can not be negative."))
          ((< position tree-bound)
           (shrink-tree! operation structure
                         container position all))
          ((< position (access-index-bound structure))
           (unset-in-tail! operation structure container
                           (logandc2 (the fixnum position)
                                     cl-ds.common.rrb:+tail-mask+)
                           all))
          (t (values structure
                     cl-ds.common:empty-eager-modification-operation-status)))))


(defmethod cl-ds.meta:position-modification
    ((operation cl-ds.meta:shrink-function)
     (structure transactional-sparse-rrb-vector)
     container
     position &rest all)
  (declare (optimize (speed 3) (space 0) (debug 0))
           (type integer position))
  (let ((tree-bound (access-tree-index-bound structure)))
    (declare (type fixnum tree-bound))
    (cond ((negative-integer-p position)
           (error 'cl-ds:argument-value-out-of-bounds
                  :argument 'position
                  :value position
                  :bounds "Must be non-negative."
                  :format-control "Sparse vector index can not be negative."))
          ((< position tree-bound)
           (transactional-shrink-tree! operation structure
                                       container position all))
          ((< position (access-index-bound structure))
           (unset-in-tail! operation structure container
                           (logandc2 (the fixnum position)
                                     cl-ds.common.rrb:+tail-mask+)
                           all))
          (t (values structure
                     cl-ds.common:empty-eager-modification-operation-status)))))


(defmethod cl-ds.meta:position-modification
    ((operation cl-ds.meta:shrink-function)
     (structure functional-sparse-rrb-vector)
     container
     position &rest all)
  (declare (optimize (speed 3) (space 0) (debug 0))
           (type integer position))
  (let ((tree-bound (access-tree-index-bound structure)))
    (declare (type fixnum tree-bound))
    (cond ((negative-integer-p position)
           (error 'cl-ds:argument-value-out-of-bounds
                  :argument 'position
                  :value position
                  :bounds "Must be non-negative."
                  :format-control "Sparse vector index can not be negative."))
          ((< position tree-bound)
           (shrink-tree operation structure
                        container position all))
          ((< position (access-index-bound structure))
           (unset-in-tail operation structure container
                          (logandc2 (the fixnum position)
                                    cl-ds.common.rrb:+tail-mask+)
                          all))
          (t (values structure
                     cl-ds.common:empty-eager-modification-operation-status)))))


(defmethod cl-ds:size ((vect fundamental-sparse-rrb-vector))
  (declare (optimize (speed 3) (space 0) (debug 0)))
  (the fixnum (+ (the fixnum (access-tree-size vect))
                 (logcount (the fixnum (access-tail-mask vect))))))


(defun sparse-rrb-vector-at (vect position)
  (declare (optimize (speed 3) (space 0) (debug 0) (safety 0))
           (type integer position))
  (let ((bound (access-index-bound vect))
        (tree-bound (access-tree-index-bound vect)))
    (declare (type fixnum bound tree-bound))
    (cond ((not (< -1 position bound))
           (values nil nil))
          ((< position tree-bound)
           (let ((tree (slot-value vect '%tree)))
             (if (cl-ds.meta:null-bucket-p tree)
                 (values nil nil)
                 (iterate
                   (declare (type fixnum byte-position position i)
                            (type cl-ds.common.rrb:shift shift))
                   (with node = tree)
                   (with shift = (slot-value vect '%shift))
                   (for byte-position
                        from (* cl-ds.common.rrb:+bit-count+
                                shift)
                        downto 0
                        by cl-ds.common.rrb:+bit-count+)
                   (for i = (ldb (byte cl-ds.common.rrb:+bit-count+
                                       byte-position)
                                 position))
                   (let* ((present (cl-ds.common.rrb:sparse-rrb-node-contains
                                    node
                                    i)))
                     (unless present
                       (return-from sparse-rrb-vector-at
                         (values nil nil)))
                     (setf node (cl-ds.common.rrb:sparse-nref
                                 (the cl-ds.common.rrb:sparse-rrb-node node)
                                 i)))
                   (finally (return (values node t)))))))
          (t (let* ((offset (logandc2 position cl-ds.common.rrb:+tail-mask+))
                    (present (ldb-test (byte 1 offset)
                                       (access-tail-mask vect))))
               (declare (type cl-ds.common.rrb:rrb-node-position offset))
               (if present
                   (values (aref (the simple-vector (access-tail vect)) offset) t)
                   (values nil nil)))))))


(defmethod cl-ds:at ((vect fundamental-sparse-rrb-vector)
                     position
                     &rest more-positions)
  (declare (optimize (speed 1) (space 0) (debug 0) (safety 3))
           (type integer position))
  (cl-ds:assert-one-dimension more-positions)
  (sparse-rrb-vector-at vect position))


(defmethod cl-ds:become-functional ((container fundamental-sparse-rrb-vector))
  (apply #'make 'functional-sparse-rrb-vector
         (cl-ds.utils:cloning-list container)))


(defmethod cl-ds:become-mutable ((container fundamental-sparse-rrb-vector))
  (apply #'make 'mutable-sparse-rrb-vector
         (cl-ds.utils:cloning-list container)))


(defmethod cl-ds:become-transactional ((container transactional-sparse-rrb-vector))
  (cl-ds.utils:quasi-clone* container
    :ownership-tag (cl-ds.common.abstract:make-ownership-tag)
    :tail (deep-copy-of-tail container)))


(defmethod cl-ds:become-transactional ((container fundamental-sparse-rrb-vector))
  (apply #'make 'transactional-sparse-rrb-vector
         :tail (deep-copy-of-tail container)
         (cl-ds.utils:cloning-information container)))


(defun make-transactional-sparse-rrb-vector (&key (element-type t))
  (make-instance 'transactional-sparse-rrb-vector
                 :ownership-tag (cl-ds.common.abstract:make-ownership-tag)
                 :element-type element-type))


(defun make-mutable-sparse-rrb-vector (&key (element-type t))
  (make-instance 'mutable-sparse-rrb-vector
                 :element-type element-type))


(defun make-functional-sparse-rrb-vector (&key (element-type t))
  (make-instance 'functional-sparse-rrb-vector
                 :element-type element-type))


(defstruct srrb-range-stack-cell
  (start 0 :type cl-ds.common.rrb:node-size)
  (end 0 :type cl-ds.common.rrb:node-size)
  (depth 0 :type non-negative-fixnum)
  (upper-bits 0 :type non-negative-fixnum)
  (is-tail nil :type boolean)
  container
  node-content
  node-bitmask)


(defun forward-cell (cell)
  (check-type cell srrb-range-stack-cell)
  (let* ((start (srrb-range-stack-cell-start cell))
         (end (srrb-range-stack-cell-end cell))
         (depth (srrb-range-stack-cell-depth cell))
         (upper-bits (srrb-range-stack-cell-upper-bits cell))
         (reached-end (eql start end))
         (container (srrb-range-stack-cell-container cell))
         (node-content (srrb-range-stack-cell-node-content cell))
         (node-bitmask (srrb-range-stack-cell-node-bitmask cell))
         (is-tail (srrb-range-stack-cell-is-tail cell))
         (is-leaf (eql depth (access-shift container))))
    (if reached-end
        (values nil nil)
        (if is-tail
            (let* ((index (iterate
                            (for i from 0
                                 below cl-ds.common.rrb:+maximum-children-count+)
                            (summing (ldb (byte 1 i) node-bitmask) into sum)
                            (until (eql (1- sum) start))
                            (finally (return i)))))
              (if (ldb-test (byte index 0) node-bitmask)
                  (values (list* (+ index (access-tree-index-bound container))
                                 (aref node-content index))
                          (make-srrb-range-stack-cell
                           :start (1+ start)
                           :end end
                           :depth depth
                           :upper-bits upper-bits
                           :is-tail t
                           :container container
                           :node-content node-content
                           :node-bitmask node-bitmask))
                  (values nil nil)))
            (let* ((at (~> node-content
                           (aref start)))
                   (position (~> start
                                 (byte _ 0)
                                 (ldb node-bitmask)
                                 logcount))
                   (next-bits (dpb position
                                   (byte cl-ds.common.rrb:+bit-count+
                                         (* depth cl-ds.common.rrb:+bit-count+))
                                   upper-bits))
                   (next-cell (make-srrb-range-stack-cell
                               :start (1+ start)
                               :end end
                               :depth depth
                               :upper-bits upper-bits
                               :container container
                               :node-content node-content
                               :node-bitmask node-bitmask)))
              (if is-leaf
                  (values (list* next-bits at)
                          next-cell)
                  (values
                   (make-srrb-range-stack-cell
                    :start 0
                    :end (~> at cl-ds.common.rrb:sparse-rrb-node-bitmask
                             logcount)
                    :depth (1+ depth)
                    :upper-bits next-bits
                    :container container
                    :node-content (cl-ds.common.rrb:sparse-rrb-node-content at)
                    :node-bitmask (cl-ds.common.rrb:sparse-rrb-node-bitmask at))
                   next-cell)))))))


(defun obtain-value (pull push)
  (iterate
    (for old-cell = (funcall pull))
    (for (values new-cell modified-cell) = (forward-cell old-cell))
    (unless (null modified-cell)
      (funcall push modified-cell))
    (if (and (listp new-cell) (not (null new-cell)))
        (return-from obtain-value (values new-cell t))
        (unless (null new-cell)
          (funcall push new-cell)))))


(defun make-range-from-root (container)
  (let ((root (access-tree container)))
    (when root
      (let ((content (cl-ds.common.rrb:sparse-rrb-node-content root))
            (bitmask (cl-ds.common.rrb:sparse-rrb-node-bitmask root)))
        (make-srrb-range-stack-cell
         :start 0
         :end (logcount bitmask)
         :node-content content
         :node-bitmask bitmask
         :container container)))))


(defun make-range-from-tail (container)
  (let ((tail (access-tail container))
        (tail-mask (access-tail-mask container)))
    (unless (zerop tail-mask)
      (make-srrb-range-stack-cell
       :depth (access-shift container)
       :start 0
       :end cl-ds.common.rrb:+maximum-children-count+
       :node-content tail
       :node-bitmask tail-mask
       :is-tail t
       :container container))))


(defmethod cl-ds:whole-range ((container fundamental-sparse-rrb-vector))
  (make 'cl-ds.common:forward-tree-range
        :obtain-value #'obtain-value
        :key #'identity
        :forward-stack (~>> (list (make-range-from-root container)
                                  (make-range-from-tail container))
                            (delete nil))
        :container container))


(defmethod cl-ds:reset! ((vector mutable-sparse-rrb-vector))
  (setf (access-tree vector) cl-ds.meta:null-bucket
        (access-tail-mask vector) 0
        (access-shift vector) 0
        (access-tree-size vector) 0
        (access-tree-index-bound vector) 0
        (access-index-bound vector) cl-ds.common.rrb:+maximum-children-count+)
  vector)


(defmethod cl-ds:reset! ((vector transactional-sparse-rrb-vector))
  (setf (access-tree vector) cl-ds.meta:null-bucket
        (access-tail-mask vector) 0
        (access-shift vector) 0
        (access-tree-size vector) 0
        (access-tree-index-bound vector) 0
        (access-index-bound vector) cl-ds.common.rrb:+maximum-children-count+)
  vector)


(defmethod cl-ds:empty-clone ((vector fundamental-sparse-rrb-vector))
  (make (class-of vector) :element-type (read-element-type vector)))


(defmethod cl-ds:across ((vector fundamental-sparse-rrb-vector) function)
  (declare (optimize (speed 3)))
  (ensure-functionf function)
  (labels ((impl (node depth upper-bits
                  &aux (byte-position (* cl-ds.common.rrb:+bit-count+
                                         depth)))
             (declare (type fixnum depth upper-bits))
             (if (zerop depth)
                 (unless (cl-ds.meta:null-bucket-p node)
                   (iterate
                     (declare (type simple-vector content)
                              (type fixnum j i))
                     (with content =
                           (cl-ds.common.rrb:sparse-rrb-node-content node))
                     (with bitmask =
                           (cl-ds.common.rrb:sparse-rrb-node-bitmask node))
                     (with j = 0)
                     (for i
                          from 0
                          below cl-ds.common.rrb:+maximum-children-count+)
                     (when (ldb-test (byte 1 i) bitmask)
                       (~> (dpb i (byte cl-ds.common.rrb:+bit-count+
                                        byte-position)
                                upper-bits)
                           (list* (aref content j))
                           (funcall function _))
                       (incf j))))
                 (iterate
                   (declare (type simple-vector content)
                            (type fixnum j i))
                   (with content =
                         (cl-ds.common.rrb:sparse-rrb-node-content node))
                   (with bitmask =
                         (cl-ds.common.rrb:sparse-rrb-node-bitmask node))
                   (with j = 0)
                   (for i
                        from 0
                        below cl-ds.common.rrb:+maximum-children-count+)
                   (when (ldb-test (byte 1 i) bitmask)
                     (impl (aref content j) (1- depth)
                           (dpb i
                                (byte cl-ds.common.rrb:+bit-count+
                                      byte-position)
                                upper-bits))
                     (incf j))))))
    (impl (access-tree vector)
          (access-shift vector)
          0)
    (iterate
      (declare (type simple-vector tail)
               (type fixnum i tail-start))
      (with tail = (access-tail vector))
      (with bitmask = (access-tail-mask vector))
      (with tail-start = (- (the fixnum (access-index-bound vector))
                            cl-ds.common.rrb:+maximum-children-count+))
      (for i from 0 below cl-ds.common.rrb:+maximum-children-count+)
      (when (ldb-test (byte 1 i) bitmask)
        (funcall function (list* (+ tail-start i)
                                 (aref tail i)))))
    vector))


(defmethod cl-ds:traverse ((vector fundamental-sparse-rrb-vector) function)
  (cl-ds:across vector function))


(defmethod cl-ds:make-from-traversable
    ((class (eql 'mutable-sparse-rrb-vector))
     traversable
     &rest arguments)
  (lret ((i 0)
         (result (apply #'make-mutable-sparse-rrb-vector arguments)))
    (cl-ds:traverse traversable
                    (lambda (x)
                      (setf (cl-ds:at result i) x)
                      (incf i)))))


(defmethod cl-ds:make-from-traversable
    ((class (eql 'functional-sparse-rrb-vector))
     traversable
     &rest arguments)
  (~> (apply #'cl-ds:make-from-traversable traversable
             'mutable-sparse-rrb-vector arguments)
      cl-ds:become-functional))


(defmethod cl-ds:make-from-traversable
    ((class (eql 'transactional-sparse-rrb-vector))
     traversable
     &rest arguments)
  (~> (apply #'cl-ds:make-from-traversable traversable
             'mutable-sparse-rrb-vector arguments)
      cl-ds:become-transactional))
