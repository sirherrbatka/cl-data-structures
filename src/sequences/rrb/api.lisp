(cl:in-package #:cl-data-structures.sequences.rrb-vector)


(defclass rrb-vector (cl-ds.common.rrb:rrb-container)
  ())


(defclass functional-rrb-vector (rrb-vector
                                 cl-ds.seqs:functional-sequence)
  ())


(defclass mutable-rrb-vector (rrb-vector
                              cl-ds.seqs:mutable-sequence)
  ())


(defclass transactional-rrb-vector (rrb-vector
                                    cl-ds.seqs:transactional-sequence)
  ())


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:take-out!-function)
                                             (structure transactional-rrb-vector)
                                             container
                                             location &rest rest &key &allow-other-keys)
  (declare (optimize (speed 3) (space 0)
                     (debug 0) (safety 1)))
  (bind ((tail-size (cl-ds.common.rrb:access-tail-size structure))
         (result-status nil)
         (tail-change 0)
         ((:dflet shrink-bucket (bucket))
          (multiple-value-bind (bucket status)
              (apply #'cl-ds.meta:alter-bucket
                     operation
                     container
                     location
                     bucket
                     rest)
            (setf result-status status)
            (unless (cl-ds:changed status)
              (return-from cl-ds.meta:position-modification (values structure status)))
            (when (cl-ds.meta:null-bucket-p bucket)
              (decf tail-change))
            bucket)))
    (if (zerop tail-size)
        (if (zerop (cl-ds.common.rrb:access-size structure))
            (error 'cl-ds:empty-container :format-control "Can't take-out from empty container.")
            (bind (((:values new-root tail shift-decreased)
                    (cl-ds.common.rrb:remove-tail structure))
                   (new-shift (if shift-decreased
                                  (1- (cl-ds.common.rrb:access-shift structure))
                                  (cl-ds.common.rrb:access-shift structure)))
                   (new-size (max 0 (- (cl-ds.common.rrb:access-size structure)
                                       cl-ds.common.rrb:+maximum-children-count+)))
                   (new-tail tail)
                   (tail-size (if (null new-root)
                                  (cl-ds.common.rrb:access-size structure)
                                  cl-ds.common.rrb:+maximum-children-count+))
                   (last-index (1- tail-size))
                   (new-bucket (~> new-tail
                                   (aref (1- cl-ds.common.rrb:+maximum-children-count+))
                                   shrink-bucket)))
              (cond ((and (cl-ds.meta:null-bucket-p new-bucket)
                          (zerop (+ tail-change tail-size)))
                     (setf new-tail nil))
                    ((~> new-bucket cl-ds.meta:null-bucket-p not)
                     (setf new-tail (copy-array new-tail)
                           (aref new-tail last-index) new-bucket))
                    (t (setf new-tail (copy-array new-tail)
                             (aref new-tail last-index) nil)))
              (assert (or new-root new-tail (zerop new-size)))
              (assert (<= 0 tail-size +maximum-children-count+))
              (setf (cl-ds.common.rrb:access-root structure) new-root
                    (cl-ds.common.rrb:access-tail structure) new-tail
                    (cl-ds.common.rrb:access-tail-size structure) (+ tail-size tail-change)
                    (cl-ds.common.rrb:access-size structure) new-size
                    (cl-ds.common.rrb:access-shift structure) new-shift)))
        (let* ((tail (cl-ds.common.rrb:access-tail structure))
               (last-index (1- tail-size))
               (new-bucket (shrink-bucket (aref tail last-index))))
          (setf (aref tail (+ tail-change tail-size))
                (unless (cl-ds.meta:null-bucket-p new-bucket)
                  new-bucket))
          (incf (cl-ds.common.rrb:access-tail-size structure)
                tail-change)))
    (values structure result-status)))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:take-out!-function)
                                             (structure mutable-rrb-vector)
                                             container
                                             location &rest rest &key &allow-other-keys)
  (declare (optimize (speed 3) (space 0)
                     (debug 0) (safety 1)))
  (bind ((tail-size (cl-ds.common.rrb:access-tail-size structure))
         (result-status nil)
         (tail-change 0)
         ((:dflet shrink-bucket (bucket))
          (multiple-value-bind (bucket status)
              (apply #'cl-ds.meta:alter-bucket!
                     operation
                     container
                     location
                     bucket
                     rest)
            (setf result-status status)
            (unless (cl-ds:changed status)
              (return-from cl-ds.meta:position-modification (values structure status)))
            (when (cl-ds.meta:null-bucket-p bucket)
              (decf tail-change))
            bucket)))
    (if (zerop tail-size)
        (if (zerop (cl-ds.common.rrb:access-size structure))
            (error 'cl-ds:empty-container :format-control "Can't take-out from empty container.")
            (bind (((:values new-root tail shift-decreased)
                    (cl-ds.common.rrb:remove-tail structure))
                   (new-shift (if shift-decreased
                                  (1- (cl-ds.common.rrb:access-shift structure))
                                  (cl-ds.common.rrb:access-shift structure)))
                   (new-size (max 0 (- (cl-ds.common.rrb:access-size structure)
                                       cl-ds.common.rrb:+maximum-children-count+)))
                   (new-tail tail)
                   (tail-size (if (null new-root)
                                  (cl-ds.common.rrb:access-size structure)
                                  cl-ds.common.rrb:+maximum-children-count+))
                   (last-index (1- (+ tail-change tail-size)))
                   (new-bucket (~> new-tail
                                   (aref (1- cl-ds.common.rrb:+maximum-children-count+))
                                   shrink-bucket)))
              (cond ((and (cl-ds.meta:null-bucket-p new-bucket) (zerop (+ tail-change tail-size)))
                     (setf new-tail nil))
                    ((~> new-bucket cl-ds.meta:null-bucket-p not)
                     (setf (aref new-tail last-index) new-bucket))
                    (t (setf (aref new-tail last-index) nil)))
              (assert (or new-root new-tail (zerop new-size)))
              (assert (<= 0 tail-size +maximum-children-count+))
              (setf (cl-ds.common.rrb:access-size structure) new-size
                    (cl-ds.common.rrb:access-tail-size structure) (+ tail-size
                                                                     tail-change)
                    (cl-ds.common.rrb:access-tail structure) new-tail
                    (cl-ds.common.rrb:access-root structure) new-root
                    (cl-ds.common.rrb:access-shift structure) new-shift)))
        (bind ((tail (cl-ds.common.rrb:access-tail structure))
               (last-index (1- tail-size))
               (new-bucket (shrink-bucket (aref tail last-index))))
          (incf (cl-ds.common.rrb:access-tail-size structure) tail-change)
          (if (cl-ds.meta:null-bucket-p new-bucket)
              (setf (aref tail last-index) nil)
              (setf (aref tail last-index) new-bucket))
          (when (zerop (+ tail-size tail-change))
            (setf (cl-ds.common.rrb:access-tail structure) nil))))
    (values structure result-status)))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:grow-function)
                                             (structure mutable-rrb-vector)
                                             container
                                             index &rest rest &key value &allow-other-keys)
  (declare (optimize (speed 3) (safety 1)
                     (space 0) (debug 0)))
  (bind ((size (cl-ds.common.rrb:access-size structure))
         (result-status nil)
         (last-index (ldb (byte cl-ds.common.rrb:+bit-count+ 0) index))
         ((:dflet change-bucket (bucket))
          (multiple-value-bind (node status)
              (apply #'cl-ds.meta:alter-bucket!
                     operation
                     container
                     value
                     bucket
                     rest)
            (unless (cl-ds:changed status)
              (return-from cl-ds.meta:position-modification
                (values structure status)))
            (setf result-status status)
            node)))
    (unless (> (cl-ds:size structure) index)
      (error 'cl-ds:argument-value-out-of-bounds
             :argument 'index
             :bounds (list 0 (cl-ds:size structure))
             :value index
             :format-control "Index out of range."))
    (if (< index size)
        (let* ((node (iterate
                       (with shift = (cl-ds.common.rrb:access-shift structure))
                       (repeat shift)
                       (for position
                            from (* cl-ds.common.rrb:+bit-count+ shift)
                            downto 0
                            by cl-ds.common.rrb:+bit-count+)
                       (for i = (ldb (byte cl-ds.common.rrb:+bit-count+ position) index))
                       (for node
                            initially (cl-ds.common.rrb:access-root structure)
                            then (cl-ds.common.rrb:nref node i))
                       (finally (return node))))
               (last-array (rrb-node-content node))
               (bucket (change-bucket (aref last-array last-index))))
          (setf (aref last-array last-index) bucket))
        (let* ((offset (- index size))
               (tail (cl-ds.common.rrb:access-tail structure))
               (bucket (change-bucket (aref tail offset))))
          (setf (aref tail offset) bucket)))
    (values structure result-status)))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:put!-function)
                                             (structure mutable-rrb-vector)
                                             container
                                             location &rest rest &key value &allow-other-keys)
  (declare (optimize (speed 3) (safety 1)
                     (debug 0) (space 0)))
  (bind ((tail-size (cl-ds.common.rrb:access-tail-size structure))
         (tail (cl-ds.common.rrb:access-tail structure))
         ((:values new-bucket status) (apply #'cl-ds.meta:make-bucket
                                             operation
                                             container
                                             value
                                             rest)))
    (unless (cl-ds:changed status)
      (return-from cl-ds.meta:position-modification (values structure status)))
    (if (eql tail-size +maximum-children-count+)
        (bind ((new-tail (~> structure
                             cl-ds.common.rrb:read-element-type
                             cl-ds.common.rrb:make-node-content))
               ((:values new-root shift-increased)
                (cl-ds.common.rrb:insert-tail structure
                                              (cl-ds.common.abstract:read-ownership-tag structure)
                                              #'cl-ds.common.rrb:destructive-write
                                              tail)))
          (setf (aref new-tail 0) new-bucket
                (cl-ds.common.rrb:access-tail structure) new-tail
                (cl-ds.common.rrb:access-root structure) new-root
                (cl-ds.common.rrb:access-tail-size structure) 1)
          (when shift-increased
            (incf (cl-ds.common.rrb:access-shift structure)))
          (incf (cl-ds.common.rrb:access-size structure) +maximum-children-count+)
          (values structure status))
        (progn
          (setf tail (or tail (cl-ds.common.rrb:make-node-content))
                (aref tail tail-size) new-bucket
                (cl-ds.common.rrb:access-tail structure) tail)
          (incf (cl-ds.common.rrb:access-tail-size structure))
          (values structure status)))))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:functional-put-function)
                                             (structure functional-rrb-vector)
                                             container
                                             location &rest rest &key value &allow-other-keys)
  (declare (optimize (speed 3) (space 0)
                     (debug 0) (safety 1)))
  (bind ((tail-size (cl-ds.common.rrb:access-tail-size structure))
         (tag nil)
         (tail-change 1)
         ((:values new-bucket status) (apply #'cl-ds.meta:make-bucket
                                             operation
                                             container
                                             value
                                             rest)))
    (unless (cl-ds:changed status)
      (return-from cl-ds.meta:position-modification (values structure status)))
    (if (eql tail-size +maximum-children-count+)
        (bind ((new-tail (~> structure
                             cl-ds.common.rrb:read-element-type
                             cl-ds.common.rrb:make-node-content))
               ((:values new-root shift-increased)
                (cl-ds.common.rrb:insert-tail structure
                                              tag
                                              #'cl-ds.common.rrb:copy-on-write
                                              (cl-ds.common.rrb:access-tail structure))))
          (setf (aref new-tail 0) new-bucket)
          (make 'functional-rrb-vector
                :root new-root
                :tail new-tail
                :ownership-tag tag
                :tail-size tail-change
                :size (+ +maximum-children-count+
                         (cl-ds.common.rrb:access-size structure))
                :tail new-tail
                :shift (if shift-increased
                           (1+ (cl-ds.common.rrb:access-shift structure))
                           (cl-ds.common.rrb:access-shift structure))))
        (values
         (make 'functional-rrb-vector
               :root (cl-ds.common.rrb:access-root structure)
               :tail (let* ((tail (cl-ds.common.rrb:access-tail structure))
                            (new-tail (if (null tail)
                                          (~> structure
                                              cl-ds.common.rrb:read-element-type
                                              cl-ds.common.rrb:make-node-content)
                                          (copy-array tail))))
                       (setf (aref new-tail tail-size) new-bucket)
                       new-tail)
               :ownership-tag tag
               :tail-size (+ tail-size tail-change)
               :ownership-tag tag
               :size (cl-ds.common.rrb:access-size structure)
               :shift (cl-ds.common.rrb:access-shift structure))
         status))))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:put!-function)
                                             (structure transactional-rrb-vector)
                                             container
                                             location &rest rest &key value &allow-other-keys)
  (declare (optimize (speed 3) (space 0)
                     (debug 0) (safety 1)))
  (bind ((tail-size (cl-ds.common.rrb:access-tail-size structure))
         (tag (cl-ds.common.abstract:read-ownership-tag structure))
         (tail-change 1)
         ((:values new-bucket status) (apply #'cl-ds.meta:make-bucket
                                             operation
                                             container
                                             value
                                             rest)))
    (unless (cl-ds:changed status)
      (return-from cl-ds.meta:position-modification (values structure status)))
    (if (eql tail-size +maximum-children-count+)
        (bind ((new-tail (~> structure
                             cl-ds.common.rrb:read-element-type
                             cl-ds.common.rrb:make-node-content))
               ((:values new-root shift-increased)
                (cl-ds.common.rrb:insert-tail structure
                                              tag
                                              #'cl-ds.common.rrb:transactional-copy-on-write
                                              (cl-ds.common.rrb:access-tail structure))))
          (setf (aref new-tail 0) new-bucket)
          (setf (cl-ds.common.rrb:access-root structure) new-root
                (cl-ds.common.rrb:access-tail structure) new-tail
                (cl-ds.common.rrb:access-tail-size structure) tail-change
                (cl-ds.common.rrb:access-size structure) (+ +maximum-children-count+
                                                            (cl-ds.common.rrb:access-size structure))
                (cl-ds.common.rrb:access-shift structure) (if shift-increased
                                                              (1+ (cl-ds.common.rrb:access-shift structure))
                                                              (cl-ds.common.rrb:access-shift structure))))
        (setf (cl-ds.common.rrb:access-tail structure)
              (let* ((tail (cl-ds.common.rrb:access-tail structure))
                     (new-tail (or tail
                                   (~> structure
                                       cl-ds.common.rrb:read-element-type
                                       cl-ds.common.rrb:make-node-content))))
                (setf (aref new-tail tail-size) new-bucket)
                new-tail)

              (cl-ds.common.rrb:access-tail-size structure) (+ tail-size tail-change)))
    (values structure status)))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:take-out-function)
                                             (structure functional-rrb-vector)
                                             container
                                             location &rest rest &key &allow-other-keys)
  (declare (optimize (speed 3) (space 0)
                     (safety 1) (debug 0)))
  (bind ((tail-size (cl-ds.common.rrb:access-tail-size structure))
         (tag nil)
         (result-status nil)
         (tail-change 0)
         ((:dflet shrink-bucket (bucket))
          (multiple-value-bind (bucket status)
              (apply #'cl-ds.meta:alter-bucket
                     operation
                     container
                     location
                     bucket
                     rest)
            (setf result-status status)
            (unless (cl-ds:changed status)
              (return-from cl-ds.meta:position-modification (values structure status)))
            (when (cl-ds.meta:null-bucket-p bucket)
              (decf tail-change))
            bucket)))
    (values
     (if (zerop tail-size)
         (if (zerop (cl-ds.common.rrb:access-size structure))
             (error 'cl-ds:empty-container :format-control "Can't take-out from empty container.")
             (bind (((:values new-root tail shift-decreased)
                     (cl-ds.common.rrb:remove-tail structure))
                    (new-shift (if shift-decreased
                                   (1- (cl-ds.common.rrb:access-shift structure))
                                   (cl-ds.common.rrb:access-shift structure)))
                    (new-size (max 0 (- (cl-ds.common.rrb:access-size structure)
                                        cl-ds.common.rrb:+maximum-children-count+)))
                    (new-tail tail)
                    (tail-size (if (null new-root)
                                   (cl-ds.common.rrb:access-size structure)
                                   cl-ds.common.rrb:+maximum-children-count+))
                    (new-bucket (~> new-tail
                                    (aref (1- cl-ds.common.rrb:+maximum-children-count+))
                                    shrink-bucket)))
               (cond ((and (cl-ds.meta:null-bucket-p new-bucket)
                           (zerop tail-size))
                      (setf new-tail nil))
                     ((~> new-bucket cl-ds.meta:null-bucket-p not)
                      (setf new-tail (copy-array new-tail)
                            (aref new-tail (1- tail-size)) new-bucket))
                     (t (setf new-tail (copy-array new-tail)
                              (aref new-tail (1- tail-size)) nil)))
               (assert (or new-root new-tail (zerop new-size)))
               (assert (<= 0 tail-size +maximum-children-count+))
               (make 'functional-rrb-vector
                     :root new-root
                     :tail new-tail
                     :ownership-tag tag
                     :tail-size (+ tail-size tail-change)
                     :size new-size
                     :shift new-shift)))
         (make 'functional-rrb-vector
               :root (cl-ds.common.rrb:access-root structure)
               :tail (let* ((tail (cl-ds.common.rrb:access-tail structure))
                            (new-bucket (shrink-bucket (aref tail (1- tail-size))))
                            (new-size (+ tail-size tail-change)))
                       (if (zerop new-size)
                           (setf tail nil)
                           (setf tail (copy-array tail)
                                 (aref tail (1- tail-size)) (and (not (cl-ds.meta:null-bucket-p new-bucket))
                                                                 new-bucket)))
                       tail)
               :ownership-tag tag
               :tail-size (let ((s (+ tail-size tail-change)))
                            (assert (<= 0 s +maximum-children-count+))
                            s)
               :ownership-tag tag
               :size (cl-ds.common.rrb:access-size structure)
               :shift (cl-ds.common.rrb:access-shift structure)))
     result-status)))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:grow-function)
                                             (structure transactional-rrb-vector)
                                             container
                                             index &rest rest &key value &allow-other-keys)
  (declare (optimize (speed 3) (space 0)
                     (safety 1) (debug 0)))
  (bind ((tag (cl-ds.common.abstract:read-ownership-tag structure))
         (shift (cl-ds.common.rrb:access-shift structure))
         (size (cl-ds.common.rrb:access-size structure))
         (root (cl-ds.common.rrb:access-root structure))
         (result-status nil)
         ((:dflet change-bucket (bucket))
          (multiple-value-bind (node status)
              (apply #'cl-ds.meta:alter-bucket
                     operation
                     container
                     value
                     bucket
                     index
                     rest)
            (unless (cl-ds:changed status)
              (return-from cl-ds.meta:position-modification
                (values structure status)))
            (setf result-status status)
            node)))
    (unless (> (cl-ds:size structure) index)
      (error 'cl-ds:argument-value-out-of-bounds
             :argument 'index
             :bounds (list 0 (cl-ds:size structure))
             :value index
             :format-control "Index out of range."))
    (if (< index size)
        (bind (((:dflet cont (path indexes shift &aux (shift (1- shift))))
                (iterate
                  (with owned-depth = (iterate
                                        (for i from 0 below shift)
                                        (for node = (aref path i))
                                        (while (cl-ds.common.abstract:acquire-ownership node
                                                                                        tag))
                                        (finally (return i))))
                  (for i from shift downto 0)
                  (for position = (aref indexes i))
                  (for old-node = (aref path i))
                  (for node
                       initially (let* ((bucket (cl-ds.common.rrb:nref (aref path shift)
                                                                       (aref index shift)))
                                        (next-value (change-bucket bucket))
                                        (content (if (eql shift owned-depth)
                                                     (aref path shift)
                                                     (copy-array (rrb-node-content (aref path shift))))))
                                   (setf (aref content (aref index shift)) next-value)
                                   (if (eql shift owned-depth)
                                       (aref path shift)
                                       (cl-ds.common.rrb:make-rrb-node :ownership-tag tag
                                                                       :content content)))
                       then (if (< i owned-depth)
                                (cl-ds.common.rrb:rrb-node-push! old-node
                                                                 position
                                                                 node)
                                (cl-ds.common.rrb:rrb-node-push-into-copy old-node
                                                                          position
                                                                          node
                                                                          tag)))
                  (finally (return node))))
               (new-root (if (zerop shift)
                             (cl-ds.common.rrb:rrb-node-push-into-copy
                              root
                              index
                              (change-bucket (cl-ds.common.rrb:nref root index))
                              tag)
                             (cl-ds.common.rrb:descend-into-tree structure index #'cont))))
          (setf (cl-ds.common.rrb:access-root structure) new-root))
        (let* ((tail (~> structure
                         cl-ds.common.rrb:access-tail))
               (offset (- index size)))
          (setf (aref tail offset)
                (change-bucket (aref tail offset)))))
    (values structure result-status)))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:grow-function)
                                             (structure functional-rrb-vector)
                                             container
                                             index &rest rest &key value &allow-other-keys)
  (declare (optimize (speed 3) (space 0)
                     (safety 1) (debug 0)))
  (bind ((tail-size (cl-ds.common.rrb:access-tail-size structure))
         (tag nil)
         (shift (cl-ds.common.rrb:access-shift structure))
         (size (cl-ds.common.rrb:access-size structure))
         (root (cl-ds.common.rrb:access-root structure))
         (result-status nil)
         ((:dflet change-bucket (bucket))
          (multiple-value-bind (node status)
              (apply #'cl-ds.meta:alter-bucket
                     operation
                     container
                     value
                     bucket
                     rest)
            (unless (cl-ds:changed status)
              (return-from cl-ds.meta:position-modification
                (values structure status)))
            (setf result-status status)
            node)))
    (unless (> (cl-ds:size structure) index)
      (error 'cl-ds:argument-value-out-of-bounds
             :argument 'index
             :bounds (list 0 (cl-ds:size structure))
             :value index
             :format-control "Index out of range."))
    (values
     (if (< index size)
         (bind (((:dflet cont (path indexes shift &aux (shift (1- shift))))
                 (iterate
                   (for i from shift downto 0)
                   (for position = (aref indexes i))
                   (for old-node = (aref path i))
                   (for node
                        initially (let* ((bucket (cl-ds.common.rrb:nref (aref path shift)
                                                                        (aref index shift)))
                                         (next-value (change-bucket bucket))
                                         (content (copy-array (rrb-node-content (aref path shift)))))
                                    (setf (aref content (aref index shift))
                                          next-value)
                                    (cl-ds.common.rrb:make-rrb-node :ownership-tag tag
                                                                    :content content))
                        then (cl-ds.common.rrb:rrb-node-push-into-copy old-node
                                                                       position
                                                                       node
                                                                       tag))
                   (finally (return node))))
                (new-root (if (zerop shift)
                              (cl-ds.common.rrb:rrb-node-push-into-copy
                               root
                               index
                               (change-bucket (cl-ds.common.rrb:nref root index))
                               tag)
                              (cl-ds.common.rrb:descend-into-tree structure index #'cont)))
                ((:accessors (tail cl-ds.common.rrb:access-tail)
                             (shift cl-ds.common.rrb:access-shift))
                 structure))
           (make 'functional-rrb-vector
                 :root new-root
                 :tail tail
                 :shift shift
                 :ownership-tag tag
                 :tail-size tail-size
                 :size size))
         (make 'functional-rrb-vector
               :root (cl-ds.common.rrb:access-root structure)
               :tail (let* ((new-tail (~> structure
                                          cl-ds.common.rrb:access-tail
                                          copy-array))
                            (offset (- index size)))
                       (setf (aref new-tail offset)
                             (change-bucket (aref new-tail offset)))
                       new-tail)
               :ownership-tag tag
               :tail-size tail-size
               :ownership-tag tag
               :size size
               :shift (cl-ds.common.rrb:access-shift structure)))
     result-status)))


(defmethod cl-ds:empty-clone ((container rrb-vector))
  (make-instance (class-of container)
                 :root nil
                 :shift 0
                 :size 0
                 :tail-size 0
                 :tail nil))


(defmethod cl-ds:reset! ((obj mutable-rrb-vector))
  (setf (cl-ds.common.rrb:access-root obj) nil
        (cl-ds.common.rrb:access-shift obj) 0
        (cl-ds.common.rrb:access-size obj) 0
        (cl-ds.common.rrb:access-tail-size obj) 0
        (cl-ds.common.rrb:access-tail obj) nil)
  obj)


(defmethod cl-ds:reset! ((obj transactional-rrb-vector))
  (setf (cl-ds.common.rrb:access-root obj) nil
        (cl-ds.common.rrb:access-shift obj) 0
        (cl-ds.common.rrb:access-size obj) 0
        (cl-ds.common.rrb:access-tail-size obj) 0
        (cl-ds.common.rrb:access-tail obj) nil))


(defmethod cl-ds:become-mutable ((container functional-rrb-vector))
  (make 'mutable-rrb-vector
        :root (cl-ds.common.rrb:access-root container)
        :shift (cl-ds.common.rrb:access-shift container)
        :size (cl-ds.common.rrb:access-size container)
        :tail-size (cl-ds.common.rrb:access-tail-size container)
        :tail (and #1=(cl-ds.common.rrb:access-tail container)
                   (copy-array #1#))))


(defmethod cl-ds:become-mutable ((container transactional-rrb-vector))
  (make 'mutable-rrb-vector
        :root (cl-ds.common.rrb:access-root container)
        :shift (cl-ds.common.rrb:access-shift container)
        :size (cl-ds.common.rrb:access-size container)
        :tail-size (cl-ds.common.rrb:access-tail-size container)
        :tail (and #1=(cl-ds.common.rrb:access-tail container)
                   (copy-array #1#))))


(defmethod cl-ds:become-functional ((container mutable-rrb-vector))
  (make 'functional-rrb-vector
        :root (cl-ds.common.rrb:access-root container)
        :shift (cl-ds.common.rrb:access-shift container)
        :size (cl-ds.common.rrb:access-size container)
        :tail-size (cl-ds.common.rrb:access-tail-size container)
        :tail (and #1=(cl-ds.common.rrb:access-tail container)
                   (copy-array #1#))))


(defmethod cl-ds:become-functional ((container transactional-rrb-vector))
  (make 'functional-rrb-vector
        :root (cl-ds.common.rrb:access-root container)
        :shift (cl-ds.common.rrb:access-shift container)
        :size (cl-ds.common.rrb:access-size container)
        :tail-size (cl-ds.common.rrb:access-tail-size container)
        :tail (and #1=(cl-ds.common.rrb:access-tail container)
                   (copy-array #1#))))


(defmethod cl-ds:replica ((container transactional-rrb-vector)
                          &optional isolate)
  (cl-ds.common.abstract:replica container isolate))


(defmethod cl-ds:become-transactional ((container rrb-vector))
  (make 'transactional-rrb-vector
        :root (cl-ds.common.rrb:access-root container)
        :shift (cl-ds.common.rrb:access-shift container)
        :size (cl-ds.common.rrb:access-size container)
        :tail-size (cl-ds.common.rrb:access-tail-size container)
        :ownership-tag (cl-ds.common.abstract:make-ownership-tag)
        :tail (and #1=(cl-ds.common.rrb:access-tail container)
                   (copy-array #1#))))


(defun fold-content (tag content)
  (iterate
    (for i
         from 0
         by cl-ds.common.rrb:+maximum-children-count+)
    (for k from 0)
    (setf (aref content k)
          (iterate
            (with result = (cl-ds.common.rrb:make-node-content))
            (for j from i below (length content))
            (for k from 0)
            (repeat cl-ds.common.rrb:+maximum-children-count+)
            (setf (aref result k) (aref content j))
            (finally (return (cl-ds.common.rrb:make-rrb-node
                              :content result
                              :ownership-tag tag)))))
    (while (< i (length content)))))


(defun fold-rrb-content (content tag)
  (when (emptyp content)
    (return-from fold-rrb-content
      (values nil
              0)))
  (iterate
    (with shift = 0)
    (for count
         initially (length content)
         then (truncate count cl-ds.common.rrb:+maximum-children-count+))
    (until (zerop count))
    (fold-content tag content)
    (incf shift)
    (setf (fill-pointer content) count)
    (finally
     (return (values (if (emptyp content)
                         nil
                         (first-elt content))
                     shift)))))


(defun mutable-from-traversable (traversable tag arguments)
  (declare (optimize (speed 3)))
  (bind ((content (vect))
         (size 0)
         (element-type (getf arguments :element-type t))
         ((:dflet index ())
          (rem size cl-ds.common.rrb:+maximum-children-count+)))
    (cl-ds:across traversable
                  (lambda (x &aux (index (index)))
                    (when (zerop index)
                      (vector-push-extend (cl-ds.common.rrb:make-node-content element-type)
                                          content))
                    (setf (aref (last-elt content) index)
                          x)
                    (incf size)))
    (map-into content
              (lambda (x)
                (cl-ds.common.rrb:make-rrb-node :content x
                                                :ownership-tag tag))
              content)
    (bind ((tail-size (let ((s (rem size cl-ds.common.rrb:+maximum-children-count+)))
                        (if (zerop s)
                            32
                            s)))
           (tree-size (- size tail-size))
           (tail (if (zerop tail-size) nil (~> content
                                               cl-ds.utils:pop-last
                                               rrb-node-content)))
           ((:values root shift) (fold-rrb-content content tag)))
      (make 'mutable-rrb-vector
            :root root
            :ownership-tag tag
            :shift shift
            :tail (if (zerop tail-size)
                      nil
                      tail)
            :tail-size tail-size
            :size tree-size))))


(defmethod cl-ds:make-from-traversable (traversable
                                        (class (eql 'mutable-rrb-vector))
                                        &rest arguments)
  (declare (optimize (speed 3)))
  (mutable-from-traversable traversable nil arguments))



(defmethod cl-ds:make-from-traversable (traversable
                                        (class (eql 'functional-rrb-vector))
                                        &rest arguments)
  (bind ((result (mutable-from-traversable traversable nil arguments)))
    (cl-ds:become-functional result)))


(defmethod cl-ds:make-from-traversable (traversable
                                        (class (eql 'transactional-rrb-vector))
                                        &rest arguments)
  (bind ((tag (cl-ds.common.abstract:make-ownership-tag))
         (result (mutable-from-traversable traversable tag arguments))
         (transactional (cl-ds:become-transactional result)))
    (cl-ds.common.abstract:write-ownership-tag tag transactional)
    transactional))


(defun make-functional-rrb-vector (&rest all &key element-type)
  (declare (ignore element-type))
  (apply #'make 'functional-rrb-vector all))


(defun make-mutable-rrb-vector (&rest all &key element-type)
  (declare (ignore element-type))
  (apply #'make 'mutable-rrb-vector all))


(defun make-transactional-rrb-vector (&rest all &key element-type)
  (declare (ignore element-type))
  (apply #'make 'transactional-rrb-vector
         :ownership-tag (cl-ds.common.abstract:make-ownership-tag)
         all))


(defun make-mutable-of-size (size tag arguments)
  (declare (optimize (speed 3)))
  (let* ((number-of-leafs (~> size
                              (/ cl-ds.common.rrb:+maximum-children-count+)
                              truncate))
         (tree-size (* cl-ds.common.rrb:+maximum-children-count+
                       number-of-leafs))
         (tail-size (- size tree-size))
         (element-type (getf arguments :element-type t))
         (leafs (make-array number-of-leafs :fill-pointer number-of-leafs)))
    (iterate
      (for i from 0 below number-of-leafs)
      (setf (aref leafs i)
            (cl-ds.common.rrb:make-rrb-node
             :content (cl-ds.common.rrb:make-node-content element-type)
             :ownership-tag tag)))
    (bind (((:values root shift) (fold-rrb-content leafs tag)))
      (make 'mutable-rrb-vector
            :root root
            :ownership-tag tag
            :shift shift
            :tail (if (zerop tail-size)
                      nil
                      (cl-ds.common.rrb:make-node-content element-type))
            :tail-size tail-size
            :size tree-size))))


(defmethod cl-ds:make-of-size ((class (eql 'mutable-rrb-vector))
                               size &rest arguments)
  (make-mutable-of-size size nil arguments))


(defmethod cl-ds:make-of-size ((class (eql 'functional-rrb-vector))
                               size &rest arguments)
  (declare (optimize (speed 3)))
  (~> (make-mutable-of-size size nil arguments)
      cl-ds:become-functional))


(defmethod cl-ds:make-of-size ((class (eql 'transactional-rrb-vector))
                               size &rest arguments)
  (declare (optimize (speed 3)))
  (bind ((tag (cl-ds.common.abstract:make-ownership-tag))
         (mutable (make-mutable-of-size size tag arguments))
         (transactional (cl-ds:become-transactional mutable)))
    (cl-ds.common.abstract:write-ownership-tag tag transactional)
    transactional))
