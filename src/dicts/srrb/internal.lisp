(cl:in-package #:cl-data-structures.dicts.srrb)


(-> insert-tail-handle-root-overflow
    (fixnum
     cl-ds.common.rrb:sparse-rrb-node
     cl-ds.common.rrb:sparse-rrb-node
     t)
    cl-ds.common.rrb:sparse-rrb-node)
(defun insert-tail-handle-root-overflow (shift tree new-node ownership-tag)
  (iterate
    (repeat shift)
    (for node
         initially new-node
         then (cl-ds.common.rrb:make-sparse-rrb-node
               :content (make-array 1 :initial-element node)
               :bitmask 1
               :ownership-tag ownership-tag))
    (finally
     (bind ((content (make-array 2))
            (root (cl-ds.common.rrb:make-sparse-rrb-node
                   :content content
                   :bitmask #b11
                   :ownership-tag ownership-tag)))
       (setf (aref content 0) tree
             (aref content 1) node)
       (return root)))))


(-> make-node-from-tail
    (fundamental-sparse-rrb-vector
     t)
    cl-ds.common.rrb:sparse-rrb-node)
(defun make-node-from-tail (rrb-container ownership-tag)
  (declare (optimize (speed 3) (debug 0) (space 0) (safety 0)))
  (bind (((:slots %tree-size %shift %tree %tail %tail-mask
                  %element-type %tree-index-bound)
          rrb-container)
         (tail-mask (the cl-ds.common.rrb:sparse-rrb-mask %tail-mask))
         (tail-size (logcount tail-mask))
         (tail (the simple-vector %tail))
         (element-type (read-element-type rrb-container))
         (new-content
          (iterate
            (declare (type fixnum j i))
            (with result = (make-array tail-size :element-type element-type))
            (with j = 0)
            (for i from 0 below cl-ds.common.rrb:+maximum-children-count+)
            (for present = (ldb-test (byte 1 i) tail-mask))
            (when present
              (setf (aref result j) (aref tail i))
              (incf j))
            (finally (return result))))
         (new-node (cl-ds.common.rrb:make-sparse-rrb-node
                    :content new-content
                    :bitmask tail-mask
                    :ownership-tag ownership-tag)))
    new-node))


(defun has-single-child-p (node)
  (~> node cl-ds.common.rrb:sparse-rrb-node-size (eql 1)))


(defun insert-into-node! (into new-element index)
  (assert (not (cl-ds.common.rrb:sparse-rrb-node-contains into index)))
  (let* ((content (cl-ds.common.rrb:sparse-rrb-node-content into))
         (bitmask (cl-ds.common.rrb:sparse-rrb-node-bitmask into))
         (new-bitmask (dpb 1 (byte 1 index) bitmask))
         (length (length content))
         (position (1- (logcount (ldb (byte (1+ index) 0) new-bitmask))))
         (new-content
           (if (eql length (logcount bitmask))
               (lret ((r (make-array
                          (1+ length)
                          :element-type (array-element-type content))))
                 (setf (cl-ds.common.rrb:sparse-rrb-node-content into) r))
               content)))
    (declare (type fixnum length position)
             (type cl-ds.common.rrb:sparse-rrb-mask new-bitmask bitmask)
             (type (simple-array * (*)) new-content content))
    (setf (cl-ds.common.rrb:sparse-rrb-node-bitmask into)
          new-bitmask)
    (iterate
      (declare (type fixnum i))
      (for i from 0 below position)
      (setf (aref new-content i) (aref content i)))
    (iterate
      (declare (type fixnum i))
      (for i from position below (logcount bitmask))
      (setf (aref new-content (1+ i)) (aref content i)))
    (setf (aref new-content position) new-element)
    new-element))


(-> insert-tail! (mutable-sparse-rrb-vector)
    mutable-sparse-rrb-vector)
(defun insert-tail! (structure)
  (let ((tail-mask (access-tail-mask structure))
        (ownership-tag nil))
    (declare (type (type cl-ds.common.rrb:sparse-rrb-mask new-bitmask bitmask) tail-mask))
    (when (zerop tail-mask)
      (return-from insert-tail! structure))
    (bind ((new-node (make-node-from-tail structure ownership-tag))
           ((:accessors (tree access-tree)
                        (tree-size access-tree-size)
                        (%shift access-shift)
                        (index-bound access-index-bound)
                        (tree-index-bound access-tree-index-bound))
            structure)
           (root tree)
           (shift %shift))
      (declare (type non-negative-fixnum shift))
      ;; corner case
      (when (and (cl-ds.meta:null-bucket-p root)
                 (> index-bound cl-ds.common.rrb:+maximum-children-count+))
        (setf root (cl-ds.common.rrb:make-sparse-rrb-node
                    :ownership-tag ownership-tag)))
      (cond ((cl-ds.meta:null-bucket-p root)
             (setf tree new-node))
            ((>= (ash tree-index-bound (- cl-ds.common.rrb:+bit-count+))
                 (ash 1 (* cl-ds.common.rrb:+bit-count+ shift))) ; overflow
             (let ((new-root (insert-tail-handle-root-overflow
                              shift root new-node ownership-tag)))
               (incf %shift)
               (setf tree new-root)))
            (t (iterate
                 (declare (type fixnum size position index))
                 (with size =
                       (~> structure
                           access-index-bound
                           (- cl-ds.common.rrb:+maximum-children-count+)))
                 (with node = root)
                 (with position = (* cl-ds.common.rrb:+bit-count+ shift))
                 (with p-node = nil)
                 (for index = (ldb (byte cl-ds.common.rrb:+bit-count+
                                         position)
                                   size))
                 (unless (> (decf shift) 0)
                   (finish))
                 (let ((present (cl-ds.common.rrb:sparse-rrb-node-contains
                                 node index)))
                   (if present
                       (shiftf p-node
                               node
                               (cl-ds.common.rrb:sparse-nref node index))
                       (let* ((new-element
                                (cl-ds.common.rrb:make-sparse-rrb-node
                                 :ownership-tag ownership-tag
                                 :content (make-array 1))))
                         (insert-into-node! node new-element index)
                         (assert (eq (cl-ds.common.rrb:sparse-nref node
                                                                   index)
                                     new-element))
                         (setf node new-element
                               p-node node))))
                 (decf position cl-ds.common.rrb:+bit-count+)
                 (finally
                  (insert-into-node! node new-node
                                     (ldb (byte cl-ds.common.rrb:+bit-count+
                                                cl-ds.common.rrb:+bit-count+)
                                          size))))))
      (setf (access-tail-mask structure) 0
            (access-tree-index-bound structure) (tree-index-bound tree %shift)))
    (incf (access-tree-size structure) (logcount tail-mask)))
  structure)


(defun insert-tail (structure)
  (declare (optimize (speed 3) (debug 0) (space 0) (safety 0)))
  (let ((tail-mask (access-tail-mask structure)))
    (declare (type (type cl-ds.common.rrb:sparse-rrb-mask new-bitmask bitmask) tail-mask))
    (if (zerop tail-mask)
        (make (type-of structure)
              :tree (access-tree structure)
              :tail nil
              :tail-mask 0
              :shift (access-shift structure)
              :tree-size (access-tree-size structure)
              :tree-index-bound (access-index-bound structure)
              :index-bound (+ cl-ds.common.rrb:+maximum-children-count+
                              (access-index-bound structure))
              :element-type (read-element-type structure))
        (bind ((new-node (make-node-from-tail structure nil))
               ((:accessors (tree access-tree)
                            (tree-size access-tree-size)
                            (index-bound access-index-bound)
                            (%shift access-shift)
                            (tree-index-bound access-tree-index-bound))
                structure)
               (root tree)
               (shift %shift))
          (declare (type non-negative-fixnum shift))
          (when (and (cl-ds.meta:null-bucket-p root)
                     (> index-bound cl-ds.common.rrb:+maximum-children-count+))
            (setf root (cl-ds.common.rrb:make-sparse-rrb-node)))
          (cond ((cl-ds.meta:null-bucket-p root)
                 (setf root new-node))
                ((>= (ash tree-index-bound (- cl-ds.common.rrb:+bit-count+))
                     (ash 1 (* cl-ds.common.rrb:+bit-count+ shift))) ; overflow
                 (let ((new-root (insert-tail-handle-root-overflow
                                  shift root new-node nil)))
                   (incf shift)
                   (setf root new-root)))
                (t (bind ((size (~>
                                 structure
                                 access-index-bound
                                 (- cl-ds.common.rrb:+maximum-children-count+)))
                          ((:labels impl (node byte-position depth))
                           (let ((current-node
                                   (if (cl-ds.meta:null-bucket-p node)
                                       (cl-ds.common.rrb:make-rrb-node
                                        :content (make-array 1))
                                       (cl-ds.common.rrb:deep-copy-sparse-rrb-node
                                        node 1))))
                             (if (zerop depth)
                                 (insert-into-node!
                                  current-node new-node
                                  (ldb (byte cl-ds.common.rrb:+bit-count+
                                             cl-ds.common.rrb:+bit-count+)
                                       size))
                                 (let* ((index
                                          (ldb (byte cl-ds.common.rrb:+bit-count+
                                                     byte-position)
                                               size))
                                        (present
                                          (and (not (cl-ds.meta:null-bucket-p node))
                                               (cl-ds.common.rrb:sparse-rrb-node-contains
                                                node index)))
                                        (next-node
                                          (if present
                                              (cl-ds.common.rrb:sparse-nref
                                               node index)
                                              cl-ds.meta:null-bucket))
                                        (new-node
                                          (impl next-node
                                                (- byte-position
                                                   cl-ds.common.rrb:+bit-count+)
                                                (1- depth))))
                                   (setf (cl-ds.common.rrb:sparse-nref
                                          current-node index)
                                         new-node)))
                             current-node)))
                     (setf root (impl root
                                      (* cl-ds.common.rrb:+bit-count+ shift)
                                      (max 0 (1- shift)))))))
          (make (type-of structure)
                :tree root
                :tail nil
                :tail-mask 0
                :shift shift
                :tree-size (+ tree-size
                              (logcount (access-tail-mask structure)))
                :tree-index-bound (tree-index-bound root shift)
                :index-bound (+ cl-ds.common.rrb:+maximum-children-count+
                                (access-index-bound structure))
                :element-type (read-element-type structure))))))


(-> transactional-insert-tail! (transactional-sparse-rrb-vector t)
    transactional-sparse-rrb-vector)
(defun transactional-insert-tail! (structure ownership-tag)
  (let ((tail-mask (access-tail-mask structure)))
    (declare (type (type cl-ds.common.rrb:sparse-rrb-mask new-bitmask bitmask) tail-mask))
    (when (zerop tail-mask)
      (return-from transactional-insert-tail! structure))
    (bind ((new-node (make-node-from-tail structure ownership-tag))
           (size (~> structure
                     access-index-bound
                     (- cl-ds.common.rrb:+maximum-children-count+)))
           ((:accessors (tree access-tree)
                        (tree-size access-tree-size)
                        (%shift access-shift)
                        (index-bound access-index-bound)
                        (tree-index-bound access-tree-index-bound))
            structure)
           ((:labels impl (node byte-position depth))
            (declare (type fixnum byte-position depth))
            (let* ((current-node (if (cl-ds.meta:null-bucket-p node)
                                     (cl-ds.common.rrb:make-sparse-rrb-node
                                      :content (make-array 1)
                                      :ownership-tag ownership-tag)
                                     node))
                   (owned (cl-ds.common.abstract:acquire-ownership
                           current-node
                           ownership-tag)))
              (if (zerop depth)
                  (let ((final-index (ldb (byte cl-ds.common.rrb:+bit-count+
                                                cl-ds.common.rrb:+bit-count+)
                                          size)))
                    (if owned
                        (progn
                          (insert-into-node! current-node new-node final-index)
                          current-node)
                        (lret ((copy (cl-ds.common.rrb:deep-copy-sparse-rrb-node
                                      current-node 1
                                      ownership-tag)))
                          (insert-into-node! copy new-node final-index))))
                  (let* ((index (ldb (byte cl-ds.common.rrb:+bit-count+
                                           byte-position)
                                     size))
                         (present (and (not (cl-ds.meta:null-bucket-p node))
                                       (cl-ds.common.rrb:sparse-rrb-node-contains
                                        node index)))
                         (next-node (if present
                                        (cl-ds.common.rrb:sparse-nref
                                         node index)
                                        cl-ds.meta:null-bucket))
                         (new-node (impl next-node
                                         (- byte-position
                                            cl-ds.common.rrb:+bit-count+)
                                         (1- depth))))
                    (cl-ds.utils:cond+ (owned present)
                      ((t t)
                       (setf (cl-ds.common.rrb:sparse-nref current-node index)
                             new-node)
                       current-node)
                      ((t nil)
                       (insert-into-node! current-node new-node index)
                       current-node)
                      ((nil t)
                       (lret ((copy (cl-ds.common.rrb:deep-copy-sparse-rrb-node
                                     current-node 0
                                     ownership-tag)))
                         (setf (cl-ds.common.rrb:sparse-nref copy index)
                               new-node)))
                      ((nil nil)
                       (lret ((copy (cl-ds.common.rrb:deep-copy-sparse-rrb-node
                                     current-node 1
                                     ownership-tag)))
                         (insert-into-node! copy new-node index))))))))
           (root tree)
           (shift %shift))
      (declare (type non-negative-fixnum shift))
      ;; corner case
      (when (and (cl-ds.meta:null-bucket-p root)
                 (> index-bound cl-ds.common.rrb:+maximum-children-count+))
        (setf root (cl-ds.common.rrb:make-sparse-rrb-node
                    :ownership-tag ownership-tag)))
      (cond ((cl-ds.meta:null-bucket-p root)
             (setf tree new-node))
            ((>= (ash tree-index-bound (- cl-ds.common.rrb:+bit-count+))
                 (ash 1 (* cl-ds.common.rrb:+bit-count+ shift))) ; overflow
             (let ((new-root (insert-tail-handle-root-overflow
                              shift root new-node ownership-tag)))
               (incf %shift)
               (setf tree new-root)))
            (t (let ((new-tree (impl root
                                     (* cl-ds.common.rrb:+bit-count+ shift)
                                     (max 0 (1- shift)))))
                 (unless (eq new-tree root)
                   (setf tree new-tree)))))
      (setf (access-tail-mask structure) 0
            (access-tree-index-bound structure) (tree-index-bound tree %shift)))
    (incf (access-tree-size structure) (logcount tail-mask)))
  structure)


(-> make-adjusted-tree (fundamental-sparse-rrb-vector fixnum fixnum t)
    cl-ds.common.rrb:sparse-rrb-node)
(defun make-adjusted-tree (structure position new-shift ownership-tag)
  (declare (optimize (speed 3) (space 0) (debug 0) (safety 0))
           (ignore position))
  (bind (((:accessors (root access-tree)
                      (tree-size access-tree-size)
                      (shift access-shift)
                      (tree-index-bound access-tree-index-bound))
          structure)
         (old-shift shift)
         (old-tree-index-bound tree-index-bound)
         (shift-difference (- new-shift old-shift))
         (larger? (> shift-difference 0)))
    (declare (type non-negative-fixnum
                   old-tree-index-bound
                   old-shift)
             (type boolean larger?)
             (type fixnum shift-difference))
    (assert (not (zerop shift-difference)))
    (if larger?
        (if (cl-ds.meta:null-bucket-p root)
            (cl-ds.common.rrb:make-sparse-rrb-node
             :ownership-tag ownership-tag)
            (iterate
              (declare (type fixnum i highest-current byte-position))
              (with highest-current = (1- old-tree-index-bound))
              (with new-root = (cl-ds.common.rrb:make-sparse-rrb-node
                                :content (make-array 1)
                                :ownership-tag ownership-tag))
              (with node = new-root)
              (with byte-position = (* cl-ds.common.rrb:+bit-count+
                                       new-shift))
              (repeat (1- shift-difference))
              (for i = (ldb (byte cl-ds.common.rrb:+bit-count+ byte-position)
                            highest-current))
              (setf node (insert-new-node! node i ownership-tag))
              (decf byte-position cl-ds.common.rrb:+bit-count+)
              (finally
               (let ((i (ldb (byte cl-ds.common.rrb:+bit-count+
                                   (* (1+ old-shift)
                                      cl-ds.common.rrb:+bit-count+))
                             highest-current)))
                 (cl-ds.common.rrb:with-sparse-rrb-node node
                   (setf (cl-ds.common.rrb:sparse-nref node i) root))
                 (assert (eq (cl-ds.common.rrb:sparse-nref node i)
                             root))
                 (return new-root)))))
        (if (cl-ds.meta:null-bucket-p root)
            (cl-ds.common.rrb:make-sparse-rrb-node
             :ownership-tag ownership-tag)
            (iterate
              (with node = root)
              (until (~> node cl-ds.common.rrb:sparse-rrb-node-bitmask zerop))
              (repeat (- shift-difference))
              (setf node (~> node
                             cl-ds.common.rrb:sparse-rrb-node-content
                             first-elt))
              (finally (return node)))))))


(deftype shift () `(integer 0 ,cl-ds.common.rrb:+maximal-shift+))
(-> shift-for-position (fixnum) shift)
(defun shift-for-position (position)
  (~> position
      integer-length
      (ceiling cl-ds.common.rrb:+bit-count+)
      1-
      (max 0)))


(-> adjust-tree-to-new-size! (fundamental-sparse-rrb-vector fixnum t)
    fundamental-sparse-rrb-vector)
(defun adjust-tree-to-new-size! (structure position ownership-tag)
  (let ((new-shift (shift-for-position (1- position))))
    (unless (eql new-shift (access-shift structure))
      (let ((new-root (make-adjusted-tree structure position new-shift
                                          ownership-tag)))
        (setf (access-shift structure) new-shift
              (access-tree structure) new-root)))
    (setf (access-tree-index-bound structure)
          (* (floor position cl-ds.common.rrb:+maximum-children-count+)
             cl-ds.common.rrb:+maximum-children-count+))
    structure))


(-> set-in-tail! (mutable-sparse-rrb-vector
                  cl-ds.meta:grow-function t
                  cl-ds.common.rrb:rrb-node-position
                  t
                  list)
    (values mutable-sparse-rrb-vector t))
(defun set-in-tail! (structure operation container offset value all)
  (bind (((:accessors (element-type read-element-type)
                      (%tail-mask access-tail-mask)
                      (%tail access-tail))
          structure)
         (tail %tail)
         (tail-mask %tail-mask)
         (present (ldb-test (byte 1 offset) tail-mask)))
    (declare (type (or null simple-vector) tail)
             (type cl-ds.common.rrb:sparse-rrb-mask tail-mask)
             (type boolean present))
    (if present
        (bind ((old-bucket (aref (the simple-vector tail) offset))
               ((:values bucket status)
                (apply #'cl-ds.meta:alter-bucket!
                       container operation value old-bucket all)))
          (when (cl-ds:changed status)
            (setf (aref tail offset) bucket))
          (values structure status))
        (bind (((:values bucket status)
                (apply #'cl-ds.meta:make-bucket
                       container operation value
                       all)))
          (when (cl-ds:changed status)
            (let ((tail-array
                    (or tail
                        (make-array
                         cl-ds.common.rrb:+maximum-children-count+
                         :element-type t))))
              (setf (aref tail-array offset) bucket
                    %tail-mask (dpb 1 (byte 1 offset) tail-mask))
              (unless (eq tail tail-array)
                (setf %tail tail-array))))
          (values structure status)))))


(defun tail-copy (tail element-type)
  (if (null tail)
      (make-array
       cl-ds.common.rrb:+maximum-children-count+
       :element-type element-type)
      (copy-array tail)))


(defun deep-copy-of-tail (vector)
  (tail-copy (access-tail vector) (read-element-type vector)))


(defun set-in-tail (structure operation container offset value all)
  (bind (((:accessors (element-type read-element-type)
                      (%tail-mask access-tail-mask)
                      (%tail access-tail))
          structure)
         (tail %tail)
         (new-tail nil)
         (tail-mask %tail-mask)
         (final-status nil)
         (present (ldb-test (byte 1 offset) tail-mask)))
    (declare (type (or null cl-ds.common.rrb:node-content) tail)
             (type cl-ds.common.rrb:sparse-rrb-mask tail-mask)
             (type boolean present))
    (if present
        (bind ((old-bucket (aref tail offset))
               ((:values bucket status)
                (apply #'cl-ds.meta:alter-bucket
                       container operation value old-bucket all)))
          (when (cl-ds:changed status)
            (setf final-status status
                  new-tail (tail-copy tail t)
                  (aref new-tail offset) bucket)))
        (bind (((:values bucket status)
                (apply #'cl-ds.meta:make-bucket
                       container operation value
                       all)))
          (setf final-status status)
          (when (cl-ds:changed status)
            (setf new-tail (tail-copy tail t)
                  (aref new-tail offset) bucket
                  tail-mask (dpb 1 (byte 1 offset) tail-mask)))))
    (values (if (null new-tail)
                structure
                (make (type-of structure)
                      :tree (access-tree structure)
                      :tail new-tail
                      :tail-mask tail-mask
                      :shift (access-shift structure)
                      :tree-size (access-tree-size structure)
                      :tree-index-bound (access-tree-index-bound structure)
                      :element-type (read-element-type structure)
                      :index-bound (access-index-bound structure)))
            final-status)))


(-> insert-new-node! (cl-ds.common.rrb:sparse-rrb-node
                      cl-ds.common.rrb:rrb-node-position
                      &optional t)
    cl-ds.common.rrb:sparse-rrb-node)
(defun insert-new-node! (node i &optional ownership-tag)
  (let* ((old-content (cl-ds.common.rrb:sparse-rrb-node-content node))
         (old-content-size (array-dimension old-content 0))
         (old-bitmask (cl-ds.common.rrb:sparse-rrb-node-bitmask node))
         (new-bitmask (dpb 1 (byte 1 i) old-bitmask))
         (splice-index (1- (logcount (ldb (byte (1+ i) 0) new-bitmask))))
         (old-count (logcount old-bitmask))
         (new-count (1+ old-count))
         (new-content (if (<= new-count old-content-size)
                          old-content
                          (make-array
                           new-count
                           :element-type (array-element-type old-content))))
         (new-node (cl-ds.common.rrb:make-sparse-rrb-node
                    :ownership-tag ownership-tag
                    :content (make-array 1
                                         :element-type (array-element-type
                                                        old-content))
                    :bitmask 0)))
    (unless (eq old-content new-content)
      (setf (cl-ds.common.rrb:sparse-rrb-node-content node) new-content)
      (iterate
        (for i from 0 below splice-index)
        (setf (aref new-content i) (aref old-content i))))
    (iterate
      (for i from splice-index below old-count)
      (setf (aref new-content (1+ i)) (aref old-content i)))
    (setf (aref new-content splice-index)
          new-node
          (cl-ds.common.rrb:sparse-rrb-node-bitmask node) new-bitmask)
    new-node))


(-> transactional-grow-tree! (cl-ds.meta:grow-function
                              transactional-sparse-rrb-vector
                              t
                              fixnum
                              list
                              t)
    (values transactional-sparse-rrb-vector t))
(defun transactional-grow-tree! (operation structure container position all value)
  (declare (optimize (debug 0)))
  (bind ((final-status nil)
         (ownership-tag (cl-ds.common.abstract:read-ownership-tag structure))
         (operation-type (type-of operation))
         (size-increased 0)
         (update? (member operation-type
                          '(cl-ds.meta:update!-function
                            cl-ds.meta:update-if!-function)))
         ((:labels impl (node byte-position depth))
          (let* ((i (ldb (byte cl-ds.common.rrb:+bit-count+ byte-position)
                         position))
                 (present (and (not (cl-ds.meta:null-bucket-p node))
                               (cl-ds.common.rrb:sparse-rrb-node-contains node i))))
            (when (and (not present) update?)
              (return-from transactional-grow-tree!
                (values structure
                        cl-ds.common:empty-eager-modification-operation-status)))
            (if (zerop depth)
                (if present
                    (bind ((current (cl-ds.common.rrb:sparse-nref node i))
                           ((:values new-bucket status)
                            (apply #'cl-ds.meta:alter-bucket
                                   container operation value current all)))
                      (if (cl-ds:changed status)
                          (progn
                            (if (cl-ds.common.abstract:acquire-ownership
                                 node ownership-tag)
                                (setf (cl-ds.common.rrb:sparse-nref node i) new-bucket)
                                (setf node (cl-ds.common.rrb:deep-copy-sparse-rrb-node
                                            node 0 ownership-tag)
                                      final-status status
                                      (cl-ds.common.rrb:sparse-nref node i) new-bucket))
                            node)
                          (return-from transactional-grow-tree!
                            (values structure status))))
                    (bind (((:values new-bucket status)
                            (apply #'cl-ds.meta:make-bucket
                                   container operation
                                   value all))
                           (node (if (cl-ds.meta:null-bucket-p node)
                                     (cl-ds.common.rrb:make-sparse-rrb-node
                                           :content (make-array
                                                     1
                                                     :element-type (read-element-type structure))
                                           :ownership-tag ownership-tag)
                                     node))
                           (owned (cl-ds.common.abstract:acquire-ownership node ownership-tag)))
                      (if (cl-ds:changed status)
                          (if owned
                              (progn
                                (setf (cl-ds.common.rrb:sparse-nref node i) new-bucket
                                      size-increased 1
                                      final-status status)
                                node)
                              (progn
                                (setf node (cl-ds.common.rrb:deep-copy-sparse-rrb-node
                                            node 1 ownership-tag)
                                      final-status status
                                      (cl-ds.common.rrb:sparse-nref node i) new-bucket)
                                node))
                          (return-from transactional-grow-tree!
                            (values structure status)))))
                (if present
                    (let* ((next-node (cl-ds.common.rrb:sparse-nref node i))
                           (new-node (impl next-node
                                           (- byte-position cl-ds.common.rrb:+bit-count+)
                                           (1- depth))))
                      (if (eq new-node next-node)
                          node
                          (progn
                            (unless (cl-ds.common.abstract:acquire-ownership
                                     node ownership-tag)
                              (setf node (cl-ds.common.rrb:deep-copy-sparse-rrb-node
                                          node 0 ownership-tag)))
                            (setf (cl-ds.common.rrb:sparse-nref node i) new-node)
                            node)))
                    (let ((new-node (impl cl-ds.meta:null-bucket
                                          (- byte-position cl-ds.common.rrb:+bit-count+)
                                          (1- depth)))
                          (current-node (if (cl-ds.meta:null-bucket-p node)
                                            (cl-ds.common.rrb:make-sparse-rrb-node
                                             :content (make-array 1)
                                             :ownership-tag ownership-tag)
                                            node)))
                      (setf (cl-ds.common.rrb:sparse-nref current-node i) new-node)
                      current-node)))))
         (shift (access-shift structure))
         (tree (access-tree structure))
         (new-tree (impl tree
                         (* cl-ds.common.rrb:+bit-count+ shift)
                         shift)))
    (incf (access-tree-size structure) size-increased)
    (unless (eq tree new-tree)
      (setf (access-tree structure) new-tree))
    (values structure final-status)))


(defun grow-tree (operation structure container position all value)
  (bind ((final-status nil)
         (ownership-tag nil)
         (operation-type (type-of operation))
         (update? (member operation-type
                          '(cl-ds.meta:update-function
                            cl-ds.meta:update-if-function)))
         (size-increased 0)
         ((:labels impl (node byte-position depth))
          (let* ((i (ldb (byte cl-ds.common.rrb:+bit-count+ byte-position)
                         position))
                 (present (and (not (cl-ds.meta:null-bucket-p node))
                               (cl-ds.common.rrb:sparse-rrb-node-contains node i))))
            (when (and (not present) update?)
              (return-from grow-tree
                (values structure
                        cl-ds.common:empty-eager-modification-operation-status)))
            (if (zerop depth)
                (if present
                    (bind ((current (cl-ds.common.rrb:sparse-nref node i))
                           ((:values new-bucket status)
                            (apply #'cl-ds.meta:alter-bucket
                                   container operation value current all)))
                      (if (cl-ds:changed status)
                          (progn
                            (setf node (cl-ds.common.rrb:deep-copy-sparse-rrb-node
                                        node 0 ownership-tag)
                                  final-status status
                                  (cl-ds.common.rrb:sparse-nref node i) new-bucket)
                            node)
                          (return-from grow-tree
                            (values structure status))))
                    (bind (((:values new-bucket status)
                            (apply #'cl-ds.meta:make-bucket
                                   container operation value
                                   all)))
                      (if (cl-ds:changed status)
                          (progn
                            (setf node (if (cl-ds.meta:null-bucket-p node)
                                           (cl-ds.common.rrb:make-sparse-rrb-node
                                            :content (make-array
                                                      1
                                                      :element-type (read-element-type structure))
                                            :ownership-tag ownership-tag)
                                           (cl-ds.common.rrb:deep-copy-sparse-rrb-node
                                            node 1))
                                  (cl-ds.common.rrb:sparse-nref node i) new-bucket
                                  size-increased 1
                                  final-status status)
                            node)
                          (return-from grow-tree
                            (values structure status)))))
                (if present
                    (let* ((next-node (cl-ds.common.rrb:sparse-nref node i))
                           (new-node (impl next-node
                                           (- byte-position cl-ds.common.rrb:+bit-count+)
                                           (1- depth))))
                      (setf node (cl-ds.common.rrb:deep-copy-sparse-rrb-node
                                  node 0 ownership-tag)
                            (cl-ds.common.rrb:sparse-nref node i) new-node)
                      node)
                    (let ((new-node (impl cl-ds.meta:null-bucket
                                          (- byte-position cl-ds.common.rrb:+bit-count+)
                                          (1- depth)))
                          (current-node (if (cl-ds.meta:null-bucket-p node)
                                            (cl-ds.common.rrb:make-sparse-rrb-node
                                             :content (make-array 1))
                                            (cl-ds.common.rrb:deep-copy-sparse-rrb-node
                                             node 1))))
                      (assert current-node)
                      (setf (cl-ds.common.rrb:sparse-nref current-node i) new-node)
                      current-node)))))
         (shift (access-shift structure))
         (tree (access-tree structure))
         (new-tree (impl tree
                         (* cl-ds.common.rrb:+bit-count+ shift)
                         shift))
         (old-tail (access-tail structure))
         (result (make (type-of structure)
                       :tree new-tree
                       :tail old-tail
                       :tail-mask (access-tail-mask structure)
                       :shift (access-shift structure)
                       :tree-size (+ size-increased (access-tree-size structure))
                       :tree-index-bound (access-tree-index-bound structure)
                       :element-type (read-element-type structure)
                       :index-bound (access-index-bound structure))))
    (values result
            final-status)))


(-> destructive-grow-tree! (cl-ds.meta:grow-function
                            mutable-sparse-rrb-vector
                            t
                            fixnum
                            list
                            t)
    (values mutable-sparse-rrb-vector t))
(defun destructive-grow-tree! (operation structure container position all value)
  (declare (optimize (speed 3) (debug 0) (space 0) (safety 0)))
  (bind ((final-status nil)
         (operation-type (type-of operation))
         (update? (member operation-type
                          '(cl-ds.meta:update!-function
                            cl-ds.meta:update-if!-function)))
         ((:accessors (element-type read-element-type)) structure)
         (size-increased 0)
         ((:labels impl (node byte-position depth))
          (declare (type fixnum depth byte-position))
          (let* ((i (ldb (byte cl-ds.common.rrb:+bit-count+ byte-position)
                         position))
                 (present (and (not (cl-ds.meta:null-bucket-p node))
                               (cl-ds.common.rrb:sparse-rrb-node-contains node i))))
            (when (and (not present) update?)
              (return-from destructive-grow-tree!
                (values structure
                        cl-ds.common:empty-eager-modification-operation-status)))
            (if (zerop depth)
                (if present
                    (bind ((current (cl-ds.common.rrb:sparse-nref node i))
                           ((:values new-bucket status)
                            (apply #'cl-ds.meta:alter-bucket!
                                   container operation value current all)))
                      (if (cl-ds:changed status)
                          (progn
                            (setf (cl-ds.common.rrb:sparse-nref node i)
                                  new-bucket
                                  final-status status)
                            node)
                          (return-from destructive-grow-tree!
                            (values structure status))))
                    (bind (((:values new-bucket status)
                            (apply #'cl-ds.meta:make-bucket
                                   container operation
                                   value
                                   all))
                           (node (if (cl-ds.meta:null-bucket-p node)
                                     (cl-ds.common.rrb:make-sparse-rrb-node
                                      :content (make-array
                                                1 :element-type element-type))
                                     node)))
                      (if (cl-ds:changed status)
                          (progn
                            (setf (cl-ds.common.rrb:sparse-nref node i) new-bucket
                                  size-increased 1
                                  final-status status)
                            (incf (the fixnum (access-tree-size structure)))
                            node)
                          (return-from destructive-grow-tree!
                            (values structure status)))))
                (if present
                    (let* ((next-node (cl-ds.common.rrb:sparse-nref node i))
                           (new-node (impl next-node
                                           (- byte-position cl-ds.common.rrb:+bit-count+)
                                           (1- depth))))
                      (unless (eq new-node next-node)
                        (setf (cl-ds.common.rrb:sparse-nref node i) new-node))
                      node)
                    (let ((new-node (impl cl-ds.meta:null-bucket
                                          (- byte-position cl-ds.common.rrb:+bit-count+)
                                          (1- depth)))
                          (current-node (if (cl-ds.meta:null-bucket-p node)
                                            (cl-ds.common.rrb:make-sparse-rrb-node
                                             :content (make-array 1))
                                            node)))
                      (setf (cl-ds.common.rrb:sparse-nref current-node i) new-node)
                      current-node)))))
         (shift (access-shift structure)))
    (declare (type fixnum shift size-increased))
    (incf (the fixnum (access-tree-size structure)) size-increased)
    (let* ((old-root (access-tree structure))
           (new-root (impl old-root
                           (the fixnum (* cl-ds.common.rrb:+bit-count+ shift))
                           shift)))
      (unless (eq old-root new-root)
        (setf (access-tree structure) new-root)))
    (values structure final-status)))


(defun transactional-shrink-tree! (operation structure container position all)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (bind ((shift (access-shift structure))
         (tag (cl-ds.common.abstract:read-ownership-tag structure))
         (tree (access-tree structure)))
    (cl-ds.common.rrb:with-sparse-rrb-node-path
        (tree position shift path indexes length all-present)
      (unless all-present
        (return-from transactional-shrink-tree!
          (values structure
                  cl-ds.common:empty-eager-modification-operation-status)))
      (bind ((current-bucket (svref path (1- length)))
             (last-node (svref path (- length 2)))
             (last-node-mask (cl-ds.common.rrb:sparse-rrb-node-bitmask
                              last-node))
             ((:values new-bucket status)
              (apply #'cl-ds.meta:alter-bucket
                     container operation nil current-bucket all))
             (last-node-size (logcount last-node-mask)))
        (declare (type fixnum last-node-size )
                 (type cl-ds.common.rrb:sparse-rrb-mask last-node-mask))
        (unless (cl-ds:changed status)
          (return-from transactional-shrink-tree!
            (values structure status)))
        (when (cl-ds.meta:null-bucket-p new-bucket)
          (decf last-node-size)
          (decf (the fixnum (access-tree-size structure))))
        (setf (svref path (1- length)) new-bucket)
        (block end
          (let ((result
                  (cl-ds.common.rrb:reduce-path
                   (prev index node)
                   (if (cl-ds.meta:null-bucket-p prev)
                       (if (has-single-child-p node)
                           cl-ds.meta:null-bucket
                           (if (cl-ds.common.abstract:acquire-ownership
                                node tag)
                               (cl-ds.common.rrb:sparse-rrb-node-erase!
                                node index)
                               (cl-ds.common.rrb:sparse-rrb-node-erase
                                node index tag)))
                       (let ((current (cl-ds.common.rrb:sparse-nref node index)))
                         (assert (cl-ds.common.rrb:sparse-rrb-node-contains node index))
                         (cond
                           ((eq current prev)
                            (return-from end))
                           ((cl-ds.common.abstract:acquire-ownership
                             node tag)
                            (setf (cl-ds.common.rrb:sparse-nref node index) prev)
                            node)
                           (t
                            (setf node (cl-ds.common.rrb:deep-copy-sparse-rrb-node
                                        node 0 tag)
                                  (cl-ds.common.rrb:sparse-nref node index) prev)
                            node)))))))
            (setf (access-tree structure)
                  result)))
        (transactional-shrink-handle-tail! structure position status
                                           last-node-size last-node-mask last-node)))))


(defun unset-in-tail (operation structure container offset all)
  (let* ((tail-mask (access-tail-mask structure))
         (present (ldb-test (byte 1 offset) tail-mask)))
    (if present
        (bind ((tail (access-tail structure))
               (current-bucket (aref tail offset))
               ((:accessors (tree-index-bound access-tree-index-bound)
                            (index-bound access-index-bound)
                            (tree-size access-tree-size)
                            (element-type read-element-type)
                            (shift access-shift)
                            (tree access-tree))
                structure)
               ((:values new-bucket status)
                (apply #'cl-ds.meta:alter-bucket
                       container operation nil current-bucket all)))
          (if (cl-ds:changed status)
              (let ((tail-mask (dpb (if (cl-ds.meta:null-bucket-p new-bucket)
                                        0 1)
                                    (byte 1 offset) tail-mask))
                    (tail (tail-copy tail t)))
                (unless (cl-ds.meta:null-bucket-p new-bucket)
                  (setf (aref tail offset) new-bucket))
                (values (make (type-of structure)
                              :tree tree
                              :tail tail
                              :tail-mask tail-mask
                              :shift shift
                              :tree-size tree-size
                              :tree-index-bound tree-index-bound
                              :element-type element-type
                              :index-bound index-bound)
                        status))
              #1=(values
                  structure
                  cl-ds.common:empty-eager-modification-operation-status)))
        #1#)))


(defun unset-in-tail! (operation structure container offset all)
  (let* ((tail-mask (access-tail-mask structure))
         (present (ldb-test (byte 1 offset) tail-mask)))
    (if present
        (bind ((tail (access-tail structure))
               (current-bucket (aref tail offset))
               ((:values new-bucket status)
                (apply #'cl-ds.meta:alter-bucket!
                       container operation nil current-bucket all)))
          (if (cl-ds:changed status)
              (progn
                (if (cl-ds.meta:null-bucket-p new-bucket)
                    (let ((new-tail-mask (dpb 0 (byte 1 offset) tail-mask)))
                      (setf (access-tail-mask structure) new-tail-mask)
                      (when (zerop new-tail-mask)
                        (let ((tree (access-tree structure)))
                          (when (cl-ds.meta:null-bucket-p tree)
                            (setf (access-tree-index-bound structure) 0)))))
                    (setf (aref tail offset) new-bucket))
                (values structure status))
              (values structure
                      cl-ds.common:empty-eager-modification-operation-status)))
        (values structure
                cl-ds.common:empty-eager-modification-operation-status))))


(defun tree-index-bound (tree shift)
  (if (cl-ds.meta:null-bucket-p tree)
      0
      (iterate
        (with result = 0)
        (for i from 0 to shift)
        (for byte-position from (* cl-ds.common.rrb:+bit-count+ shift)
             downto 0
             by cl-ds.common.rrb:+bit-count+)
        (for bitmask = (cl-ds.common.rrb:sparse-rrb-node-bitmask node))
        (for index = (~> bitmask integer-length 1-))
        (for last = (~> bitmask logcount 1-))
        (for node
             initially tree
             then (~> node cl-ds.common.rrb:sparse-rrb-node-content (aref last)))
        (setf (ldb (byte cl-ds.common.rrb:+bit-count+ byte-position) result)
              index)
        (finally (return (1+ result))))))


(-> scan-index-bound (fundamental-sparse-rrb-vector) fixnum)
(defun scan-index-bound (structure)
  "Returns index-bound from the tree."
  (tree-index-bound (access-tree structure) (access-shift structure)))


(defun drop-unneded-nodes (tree shift-difference)
  (unless (null tree)
    (iterate
      (repeat shift-difference)
      (for node
           initially tree
           then (~> node
                    cl-ds.common.rrb:sparse-rrb-node-content
                    first-elt))
      (finally (return node)))))


(defun shrink-handle-tail! (structure position final-status
                            last-node-size last-node-mask new-last-node)
  (declare (optimize (debug 0))
           (ignore last-node-mask new-last-node)
           (type fixnum position last-node-size))
  (when (and (zerop last-node-size)
             (eql (access-tree-index-bound structure)
                  (1- position)))
    (let* ((tail-mask (access-tail-mask structure))
           (tail-empty (zerop tail-mask)))
      (declare (type cl-ds.common.rrb:sparse-rrb-mask tail-mask))
      (if tail-empty
          (adjust-tree-to-new-size! structure
                                    (scan-index-bound structure)
                                    nil)
          (progn
            (adjust-tree-to-new-size! structure
                                      (access-index-bound structure)
                                      nil)
            (insert-tail! structure)))
      (setf (access-index-bound structure)
            (the fixnum (+ cl-ds.common.rrb:+maximum-children-count+
                           (the fixnum (access-tree-index-bound
                                        structure)))))))
  (values structure final-status))


(defun transactional-shrink-handle-tail! (structure position final-status
                                          last-node-size last-node-mask
                                          new-last-node)
  (declare (optimize (speed 3))
           (ignore last-node-mask new-last-node)
           (type fixnum position last-node-size))
  (when (and (zerop last-node-size)
             (eql (the fixnum (access-tree-index-bound structure))
                  (the fixnum (1- position))))
    (let* ((tail-mask (access-tail-mask structure))
           (tail-empty (zerop tail-mask))
           (tag (cl-ds.common.abstract:read-ownership-tag structure)))
      (declare (type cl-ds.common.rrb:sparse-rrb-mask tail-mask))
      (if tail-empty
          (adjust-tree-to-new-size! structure
                                    (scan-index-bound structure)
                                    tag)
          (progn
            (adjust-tree-to-new-size! structure
                                      (access-index-bound structure)
                                      tag)
            (transactional-insert-tail! structure tag)))
      (setf (access-index-bound structure)
            (the fixnum (+ (the fixnum (access-tree-index-bound structure))
                           cl-ds.common.rrb:+maximum-children-count+)))))
  (values structure final-status))


(defun shrink-tree! (operation structure container position all)
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (let ((shift (access-shift structure))
        (tree (access-tree structure)))
    (declare (type fixnum shift))
    (cl-ds.common.rrb:with-sparse-rrb-node-path
        (tree position shift path indexes length all-present)
      (unless all-present
        (return-from shrink-tree!
          (values structure
                  cl-ds.common:empty-eager-modification-operation-status)))
      (bind ((current-bucket (svref path (1- length)))
             (last-node (svref path (- length 2)))
             (last-node-mask (cl-ds.common.rrb:sparse-rrb-node-bitmask last-node))
             (last-node-size (logcount last-node-mask))
             ((:values new-bucket status)
              (apply #'cl-ds.meta:alter-bucket!
                     container operation nil current-bucket all)))
        (unless (cl-ds:changed status)
          (return-from shrink-tree!
            (values structure status)))
        (when (cl-ds.meta:null-bucket-p new-bucket)
          (decf last-node-size)
          (decf (the fixnum (access-tree-size structure))))
        (setf (svref path (1- length)) new-bucket)
        (block end
          (let ((result
                  (cl-ds.common.rrb:reduce-path
                   (prev index node)
                   (if (cl-ds.meta:null-bucket-p prev)
                       (if (has-single-child-p node)
                           cl-ds.meta:null-bucket
                           (cl-ds.common.rrb:sparse-rrb-node-erase!
                            node index))
                       (let ((current (cl-ds.common.rrb:sparse-nref node index)))
                         (cond ((eql current prev)
                                (return-from end))
                               (t (setf (cl-ds.common.rrb:sparse-nref node index) prev)
                                  (assert (cl-ds.common.rrb:sparse-rrb-node-contains node index))
                                  node)))))))
            (setf (access-tree structure) result)))
        (shrink-handle-tail! structure position status
                             last-node-size last-node-mask last-node)))))


(defun shrink-tree (operation structure container position all)
  (declare (optimize (speed 3) (space 0) (debug 0) (safety 1)))
  (bind ((shift (access-shift structure))
         (tree (access-tree structure)))
    (cl-ds.common.rrb:with-sparse-rrb-node-path
        (tree position shift path indexes length all-present)
      (unless all-present
        (return-from shrink-tree
          (values structure
                  cl-ds.common:empty-eager-modification-operation-status)))
      (bind ((current-bucket (svref path (1- length)))
             (last-node (svref path (- length 2)))
             (last-node-mask (cl-ds.common.rrb:sparse-rrb-node-bitmask last-node))
             (last-node-size (logcount last-node-mask))
             (tail (access-tail structure))
             ((:values new-bucket status)
              (apply #'cl-ds.meta:alter-bucket
                     container operation nil current-bucket all)))
        (unless (cl-ds:changed status)
          (return-from shrink-tree
            (values structure status)))
        (when (cl-ds:changed status)
          (decf last-node-size)
          (decf (the fixnum (access-tree-size structure))))
        (setf (svref path (1- length)) new-bucket)
        (let* ((root
                (cl-ds.common.rrb:reduce-path
                 (prev index node)
                 (if (cl-ds.meta:null-bucket-p prev)
                     (if (has-single-child-p node)
                         cl-ds.meta:null-bucket
                         (cl-ds.common.rrb:sparse-rrb-node-erase
                          node index))
                     (let ((node (cl-ds.common.rrb:deep-copy-sparse-rrb-node node 0)))
                       (setf (cl-ds.common.rrb:sparse-nref node index) prev)
                       node))))
               (result (make (type-of structure)
                             :tree root
                             :tail tail
                             :tail-mask (access-tail-mask structure)
                             :shift (access-shift structure)
                             :tree-size (access-tree-size structure)
                             :tree-index-bound (access-tree-index-bound structure)
                             :index-bound (access-index-bound structure)
                             :element-type (read-element-type structure))))
        (shrink-handle-tail! result position status
                             last-node-size last-node-mask last-node))))))


(defun transactional-sparse-rrb-vector-grow (operation structure
                                             container position
                                             all value)
  (declare (type integer position))
  (check-type position integer)
  (let ((tree-bound (access-tree-index-bound structure)))
    (declare (type fixnum tree-bound))
    (cond ((negative-integer-p position)
           (error 'cl-ds:argument-value-out-of-bounds
                  :argument 'position
                  :value position
                  :bounds "Must be non-negative"
                  :format-control "Sparse vector index can not be negative."))
          ((< position tree-bound)
           (transactional-grow-tree! operation structure
                                     container position
                                     all value))
          ((< position (access-index-bound structure))
           (set-in-tail! structure operation container
                         (logandc2 (the fixnum position)
                                   cl-ds.common.rrb:+tail-mask+)
                         value all))
          (t (bind (((:values bucket status)
                     (apply #'cl-ds.meta:make-bucket
                            container operation value
                            all)))
               (check-type position cl-ds.common.rrb:rrb-index)
               (when (cl-ds:changed status)
                 (let ((ownership-tag (cl-ds.common.abstract:read-ownership-tag
                                       structure)))
                   (transactional-insert-tail! structure ownership-tag)
                   (adjust-tree-to-new-size! structure
                                             position
                                             ownership-tag))
                 (let* ((offset (- (the fixnum position)
                                   (the fixnum
                                        (access-tree-index-bound structure))))
                        (tail-mask (ash 1 offset))
                        (tail (cl-ds.common.rrb:make-node-content t)))
                   (declare (type cl-ds.common.rrb:rrb-node-position offset)
                            (type cl-ds.common.rrb:sparse-rrb-mask tail-mask)
                            (type simple-vector tail))
                   (setf (aref tail offset) bucket
                         (access-tail structure) tail
                         (access-tail-mask structure) tail-mask

                         (access-index-bound structure)
                         (* (ceiling (1+ position)
                                     cl-ds.common.rrb:+maximum-children-count+)
                            cl-ds.common.rrb:+maximum-children-count+))))
               (values structure status))))))


(defun mutable-sparse-rrb-vector-grow (operation structure
                                       container position
                                       all value)
  (declare (optimize (speed 3) (space 0) (debug 0))
           (type integer position))
  (check-type position integer)
  (let ((tree-bound (access-tree-index-bound structure)))
    (declare (type fixnum tree-bound))
    (cond ((negative-integer-p position)
           (error 'cl-ds:argument-value-out-of-bounds
                  :argument 'position
                  :value position
                  :bounds "Must be non-negative"
                  :format-control "Sparse vector index can not be negative."))
          ((< position tree-bound)
           (destructive-grow-tree! operation structure
                                   container position
                                   all value))
          ((< position (the fixnum (access-index-bound structure)))
           (set-in-tail! structure operation container
                         (logandc2 (the fixnum position)
                                   cl-ds.common.rrb:+tail-mask+)
                         value all))
          (t (bind (((:values bucket status)
                     (apply #'cl-ds.meta:make-bucket
                            container operation value
                            all)))
               (check-type position cl-ds.common.rrb:rrb-index)
               (when (cl-ds:changed status)
                 (insert-tail! structure)
                 (adjust-tree-to-new-size! structure
                                           position
                                           nil)
                 (let* ((offset (- position
                                   (the fixnum
                                        (access-tree-index-bound structure))))
                        (tail-mask (ash 1 offset))
                        (tail (cl-ds.common.rrb:make-node-content t)))
                   (declare (type cl-ds.common.rrb:rrb-node-position offset)
                            (type cl-ds.common.rrb:sparse-rrb-mask tail-mask)
                            (type simple-vector tail))
                   (setf (aref tail offset) bucket
                         (access-tail structure) tail
                         (access-tail-mask structure) tail-mask

                         (access-index-bound structure)
                         (* (ceiling (1+ position)
                                     cl-ds.common.rrb:+maximum-children-count+)
                            cl-ds.common.rrb:+maximum-children-count+))))
               (values structure status))))))


(defun functional-sparse-rrb-vector-grow (operation structure
                                          container position
                                          all value)
  (declare (optimize (speed 3) (space 0) (debug 0))
           (type integer position))
  (check-type position integer)
  (let ((tree-bound (access-tree-index-bound structure)))
    (declare (type fixnum tree-bound))
    (cond ((negative-integer-p position)
           (error 'cl-ds:argument-value-out-of-bounds
                  :argument 'position
                  :value position
                  :bounds "Must be non-negative"
                  :format-control "Sparse vector index can not be negative."))
          ((< position tree-bound)
           (grow-tree operation structure
                      container position
                      all value))
          ((< position (access-index-bound structure))
           (set-in-tail structure operation container
                        (logandc2 (the fixnum position)
                                  cl-ds.common.rrb:+tail-mask+)
                        value all))
          (t (bind (((:values bucket status)
                     (apply #'cl-ds.meta:make-bucket
                            container operation value
                            all)))
               (check-type position cl-ds.common.rrb:rrb-index)
               (if (cl-ds:changed status)
                   (let ((new-structure (insert-tail structure)))
                     (adjust-tree-to-new-size! new-structure
                                               position
                                               nil)
                     (let* ((offset (- (the fixnum position)
                                       (the fixnum (access-tree-index-bound
                                                    new-structure))))
                            (tail-mask (ash 1 offset))
                            (tail (cl-ds.common.rrb:make-node-content t))
                            (index-bound
                              (~>>
                               cl-ds.common.rrb:+maximum-children-count+
                               (ceiling (1+ position))
                               (* cl-ds.common.rrb:+maximum-children-count+))))
                       (declare (type cl-ds.common.rrb:rrb-node-position offset)
                                (type cl-ds.common.rrb:sparse-rrb-mask tail-mask)
                                (type simple-vector tail))
                       (setf (aref tail offset) bucket
                             (access-tail new-structure) tail
                             (access-tail-mask new-structure) tail-mask
                             (access-index-bound new-structure) index-bound))
                     (values new-structure status))
                   (values structure status)))))))
