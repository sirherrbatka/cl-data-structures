;; link to java implementation https://github.com/clojure/clojure/blob/0b73494c3c855e54b1da591eeb687f24f608f346/src/jvm/clojure/lang/PersistentVector.java
(cl:in-package #:cl-data-structures.common.rrb)


(eval-always
  (define-constant +bit-count+ 5)
  (define-constant +maximal-shift+ (iterate
                                     (for c
                                          initially (ash 1 +bit-count+)
                                          then (ash c +bit-count+))
                                     (while (non-negative-fixnum-p c))
                                     (counting t)))
  (define-constant +maximum-children-count+ (ash 1 +bit-count+))
  (define-constant +tail-mask+ (dpb 0 (byte +bit-count+ 0)
                                    most-positive-fixnum)))


(deftype node-content ()
  "Vector with content of the node"
  `(simple-array * (,+maximum-children-count+)))
(deftype rrb-node ()
  "Basicly either node-content or pair of node-content and ownership-tag"
  `(or node-content list))
(deftype node-size ()
  "Effectivly index in the node-content"
  `(integer 0 ,+maximum-children-count+))
(deftype shift ()
  `(integer 0 ,+maximal-shift+))
(deftype rrb-index ()
  `(integer 0 ,(ash 1 (* +bit-count+ +maximal-shift+))))
(deftype rrb-indexes ()
  `(simple-array node-size (,+maximal-shift+)))
(deftype rrb-path ()
  `(simple-array * (,+maximal-shift+)))
(deftype sparse-rrb-mask ()
  `(unsigned-byte ,+maximum-children-count+))
(deftype rrb-node-position ()
  `(integer 0 ,+maximum-children-count+))


(declaim (inline sparse-rrb-node-content))
(declaim (inline sparse-rrb-node-bitmask))
(declaim (inline sparse-rrb-node-untagged-content))
(declaim (inline sparse-rrb-node-untagged-bitmask))
(declaim (inline sparse-rrb-node-tagged-content))
(declaim (inline sparse-rrb-node-tagged-bitmask))
(cl-ds.common.abstract:define-tagged-untagged-node sparse-rrb-node
  (content #() :type (simple-array * (*)))
  (bitmask 0 :type sparse-rrb-mask))


(defun make-node-content (&optional (element-type t))
  (make-array +maximum-children-count+ :initial-element nil
                                       :element-type element-type))


(defun make-rrb-node (&key (content (make-node-content)) ownership-tag)
  (if (null ownership-tag)
      content
      (cons content ownership-tag)))


(defmacro with-sparse-rrb-node (node &body body)
  (once-only (node)
    `(locally (declare (type sparse-rrb-node ,node))
       (cl-ds.utils:cases ((typep ,node 'sparse-rrb-node-tagged)
                           (typep ,node 'sparse-rrb-node-untagged))
         (macrolet ((sindex (index)
                      `(the rrb-node-position
                            (1- (logcount (ldb (byte (1+ ,index) 0)
                                               (sparse-rrb-node-bitmask ,',node)))))))
           ,@body)))))


(defmacro with-sparse-rrb-node-path ((tree index shift path indexes length &optional all-present)
                                     &body body)
  (let ((all-present (or all-present (gensym))))
    (once-only (index shift tree)
      `(let ((,path (make-array ,(1+ +maximal-shift+)))
             (,indexes (make-array ,(1+ +maximal-shift+)
                                   :element-type 'node-size))
             (,all-present t)
             (,length 0))
         (declare (type fixnum ,length)
                  (type boolean ,all-present)
                  (dynamic-extent ,path ,indexes ,length))
         (when (not (null ,tree))
           (setf (aref ,path 0) ,tree
                 ,all-present t
                 ,length 1)
           (iterate
             (declare (type fixnum i byte-position))
             (with inner-node = ,tree)
             (for byte-position
                  from (the fixnum (* cl-ds.common.rrb:+bit-count+
                                      (the fixnum ,shift)))
                  downto 0
                  by cl-ds.common.rrb:+bit-count+)
             (for i = (ldb (byte cl-ds.common.rrb:+bit-count+ byte-position)
                           ,index))
             (let ((present (cl-ds.common.rrb:sparse-rrb-node-contains inner-node
                                                                       i)))
               (declare (type boolean present))
               (unless present
                 (setf ,all-present nil)
                 (leave))
               (setf inner-node (cl-ds.common.rrb:sparse-nref inner-node i)
                     (aref ,path (the fixnum ,length)) inner-node
                     (aref ,indexes (the fixnum (1- ,length))) i))
             (the fixnum (incf ,length))))
         (macrolet ((reduce-path ((prev-node index node)
                                  &body body)
                      (with-gensyms (!i !prev-node)
                        `(let ((,!prev-node (aref ,',path (the fixnum (1- ,',length)))))
                           (iterate
                             (declare (type fixnum ,!i))
                             (for ,!i from (the fixnum (- ,',length 2)) downto 0)
                             (let ((,index (aref ,',indexes ,!i))
                                   (,prev-node ,!prev-node)
                                   (,node (aref ,',path ,!i)))
                               (declare (type fixnum ,index))
                               (setf ,!prev-node (progn ,@body)))
                             (finally (return ,!prev-node)))))))
           ,@body)))))


(declaim (inline sparse-rrb-node-contains))
(-> sparse-rrb-node-contains (sparse-rrb-node node-size) boolean)
(defun sparse-rrb-node-contains (node index)
  (declare (optimize (speed 3)))
  (with-sparse-rrb-node node
    (ldb-test (byte 1 index) (sparse-rrb-node-bitmask node))))


(declaim (inline sparse-nref))
(-> sparse-nref (sparse-rrb-node node-size) t)
(defun sparse-nref (node index)
  (declare (optimize (speed 3) (space 0) (debug 0)))
  (with-sparse-rrb-node node
    (aref (sparse-rrb-node-content node) (sindex index))))


(declaim (inline (setf sparse-nref)))
(-> (setf sparse-nref) (t sparse-rrb-node node-size) t)
(defun (setf sparse-nref) (new-value node index)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (with-sparse-rrb-node node
    (let ((content (sparse-rrb-node-content node)))
      (if (sparse-rrb-node-contains node index)
          (setf (aref content (sindex index)) new-value)
          (let* ((length (length content))
                 (bitmask (sparse-rrb-node-bitmask node))
                 (new-bitmask (dpb 1 (byte 1 index) bitmask))
                 (content (sparse-rrb-node-content node))
                 (new-length (logcount new-bitmask))
                 (sindex 0)
                 (new-content
                   (if (>= length new-length)
                       content
                       (make-array
                        (clamp (* 2 length) 1 +maximum-children-count+)
                        :element-type (array-element-type content)))))
            (declare (type rrb-index new-length)
                     (type node-size sindex)
                     (type (simple-array * (*)) new-content))
            (assert (>= length (logcount bitmask)))
            (assert (>= (length new-content) (logcount new-bitmask)))
            (setf (sparse-rrb-node-bitmask node) new-bitmask
                  sindex (sindex index))
            (iterate
              (declare (type node-size i))
              (with i = (1- new-length))
              (while (> i sindex))
              (setf (aref new-content i) (aref content (1- i)))
              (decf i))
            (unless (eq new-content content)
              (iterate
                (declare (type node-size i))
                (with i = 0)
                (while (< i sindex))
                (setf (aref new-content i) (aref content i))
                (incf i)))
            (setf (aref new-content sindex) new-value
                  (sparse-rrb-node-content node) new-content))))))


(declaim (inline sparse-rrb-node-erase))
(-> sparse-rrb-node-erase (sparse-rrb-node node-size &optional t) sparse-rrb-node)
(defun sparse-rrb-node-erase (node i &optional ownership-tag)
  (with-sparse-rrb-node node
    (let* ((index (sindex i))
           (old-content (sparse-rrb-node-content node))
           (old-bitmask (sparse-rrb-node-bitmask node))
           (new-bitmask (dpb 0 (byte 1 i) old-bitmask))
           (old-size (logcount old-bitmask))
           (new-size (1- old-size))
           (new-content (make-array new-size
                                    :element-type (array-element-type old-content))))
      (assert (not (zerop new-bitmask)))
      (assert (not (zerop new-size)))
      (iterate
        (declare (type node-size i))
        (with i = 0)
        (while (< i index))
        (setf (aref new-content i) (aref old-content i))
        (incf i))
      (iterate
        (declare (type node-size i))
        (with i = index)
        (while (< i new-size))
        (setf (aref new-content i) (aref old-content (1+ i)))
        (incf i))
      (make-sparse-rrb-node :ownership-tag ownership-tag
                            :content new-content
                            :bitmask new-bitmask))))


(declaim (inline sparse-rrb-node-erase!))
(-> sparse-rrb-node-erase! (sparse-rrb-node node-size) sparse-rrb-node)
(defun sparse-rrb-node-erase! (node i)
  (declare (optimize (speed 3)))
  (with-sparse-rrb-node node
    (let* ((index (sindex i))
           (old-content (sparse-rrb-node-content node))
           (old-bitmask (sparse-rrb-node-bitmask node))
           (new-bitmask (dpb 0 (byte 1 i) old-bitmask))
           (new-size (logcount new-bitmask))
           (element-type (array-element-type old-content))
           (new-content (make-array new-size :element-type element-type)))
      (iterate
        (declare (type node-size i))
        (with i = 0)
        (while (< i index))
        (setf (aref new-content i) (aref old-content i))
        (incf i))
      (iterate
        (declare (type node-size i))
        (with i = index)
        (while (< i new-size))
        (setf (aref new-content i) (aref old-content (1+ i)))
        (incf i))
      (setf (sparse-rrb-node-bitmask node) new-bitmask
            (sparse-rrb-node-content node) new-content)))
  node)


(declaim (inline sparse-rrb-node-size))
(-> sparse-rrb-node-size (sparse-rrb-node) node-size)
(defun sparse-rrb-node-size (node)
  (with-sparse-rrb-node node
    (logcount (sparse-rrb-node-bitmask node))))


(declaim (inline deep-copy-sparse-rrb-node))
(-> deep-copy-sparse-rrb-node (sparse-rrb-node &optional (or null fixnum) t)
    sparse-rrb-node)
(defun deep-copy-sparse-rrb-node (node &optional size-change tag)
  (declare (optimize (speed 3) (debug 0) (space 0) (safety 0)))
  (with-sparse-rrb-node node
    (let* ((content (sparse-rrb-node-content node))
           (current-size (sparse-rrb-node-size node))
           (desired-size (clamp (if (null size-change)
                                    (the fixnum (* 2 current-size))
                                    (the fixnum (+ size-change current-size)))
                                0
                                +maximum-children-count+)))
      (declare (type fixnum current-size desired-size))
      (make-sparse-rrb-node
       :ownership-tag tag
       :bitmask (sparse-rrb-node-bitmask node)
       :content (cond ((eql 0 size-change)
                       (copy-array content))
                      (t (lret ((result (make-array
                                         desired-size
                                         :element-type (array-element-type content))))
                           (iterate
                             (for i from 0 below current-size)
                             (setf (aref result i) (aref content i))))))))))


(defun fill-sparse-rrb-node-without (new-node old-node skipped-index)
  (nest
   (with-sparse-rrb-node old-node)
   (with-sparse-rrb-node new-node)
   (let* ((old-mask (sparse-rrb-node-bitmask old-node))
          (new-mask (dpb 0 (byte 1 skipped-index) old-mask))
          (new-content (sparse-rrb-node-content new-node))
          (old-content (sparse-rrb-node-content old-node))
          (length (array-dimension old-content 0)))
     (setf (sparse-rrb-node-bitmask new-node) new-mask)
     (iterate
       (for i from 0 below skipped-index)
       (setf (aref new-content i) (aref old-content i)))
     (iterate
       (for i from skipped-index below length)
       (setf (aref new-content i) (aref old-content (1- i))))
     new-node)))


(defun fill-sparse-rrb-node-with-new (new-node old-node new-index new-element)
  (nest
   (with-sparse-rrb-node old-node)
   (with-sparse-rrb-node new-node)
   (let* ((old-mask (sparse-rrb-node-bitmask old-node))
          (new-mask (dpb 1 (byte 1 new-index) old-mask))
          (new-content (sparse-rrb-node-content new-node))
          (old-content (sparse-rrb-node-content old-node))
          (length (array-dimension old-content 0))
          (new-index (logcount (ldb (byte new-index 0) new-mask))))
     (setf (sparse-rrb-node-bitmask new-node) new-mask)
     (iterate
       (for i from 0 below new-index)
       (setf (aref new-content i) (aref old-content i)))
     (iterate
       (for i from new-index below length)
       (setf (aref new-content (1+ i)) (aref old-content i)))
     (setf (aref new-content new-index) new-element)
     new-node)))


(declaim (inline rrb-node-content))
(defun rrb-node-content (node)
  (if (listp node)
      (car node)
      node))


(defun rrb-node-deep-copy (node ownership-tag)
  (lret ((result (copy-array node)))
    (make-rrb-node result ownership-tag)))


(defun nref (node position)
  (if (listp node)
      (aref (car node) position)
      (aref node position)))


(defun (setf nref) (new-value node position)
  (if (listp node)
      (setf (aref (car node) position)
            new-value)
      (setf (aref node position) new-value)))


(defun rrb-node-push! (node position element)
  (setf (nref node position) element)
  node)


(defun rrb-node-push-into-copy (node position element ownership-tag)
  (let* ((source-content (rrb-node-content node))
         (result-content (make-node-content (array-element-type source-content))))
    (setf (aref result-content position) element)
    (iterate
      (for i from 0 below position)
      (setf (aref result-content i) (aref source-content i)))
    (make-rrb-node :ownership-tag ownership-tag
                   :content result-content)))


(defun rrb-node-pop-in-the-copy (node position ownership-tag)
  (unless (zerop position)
    (let* ((source-content (rrb-node-content node))
           (result-content (copy-array source-content)))
      (setf (aref result-content position) nil)
      (make-rrb-node :ownership-tag ownership-tag
                     :content result-content))))


(defun rrb-node-pop! (node position)
  (setf (nref node position) nil)
  node)


(defclass rrb-container (fundamental-ownership-tagged-object
                         cl-ds:traversable)
  ((%root :accessor access-root
          :initarg :root
          :initform nil
          :type (or rrb-node nil)
          :documentation "root of the tree")
   (%shift :initarg :shift
           :accessor access-shift
           :type shift
           :initform 0)
   (%size :initarg :size
          :initform 0
          :type non-negative-fixnum
          :accessor access-size)
   (%element-type :initarg :element-type
                  :initform t
                  :reader cl-ds:type-specialization
                  :reader read-element-type)
   (%tail-size :initform 0
               :initarg :tail-size
               :type node-size
               :accessor access-tail-size)
   (%tail :initform nil
          :type (or null simple-array)
          :initarg :tail
          :accessor access-tail)))


(defmethod initialize-instance :after ((container rrb-container) &key &allow-other-keys)
  (bind (((:slots %tail %tail-size %size %shift %root) container)
         (max-size (ash +maximum-children-count+ (* +bit-count+ %shift))))
    (assert (<= 0 %shift +maximal-shift+))
    (assert (<= 0 %tail-size +maximum-children-count+))
    (assert (<= 0 %size))
    (assert (<= %size max-size))
    (assert (if (null %tail) (zerop %tail-size) t))))


(declaim (inline tail-offset))
(-> tail-offset (non-negative-fixnum) non-negative-fixnum)
(defun tail-offset (size)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (if (< size 32)
      0
      (~> size 1- (logand +tail-mask+))))


(defun push-into-copy-of-tail (rrb-container ownership-tag element)
  (bind (((:slots %tail-size %tail) rrb-container))
    (unless (eql %tail-size +maximum-children-count+)
      (bind ((content %tail)
             ((:vectors copy-of-content) (copy-array content)))
        (setf (copy-of-content %tail-size) element)
        (make-rrb-node :ownership-tag ownership-tag
                       :content copy-of-content)))))


(defun push-into-tail! (rrb-container element)
  (bind (((:slots %tail-size %tail) rrb-container))
    (unless (eql %tail-size +maximum-children-count+)
      (bind (((:vectors content) %tail))
        (setf (content %tail-size) element)
        t))))


(declaim (inline insert-tail))
(-> insert-tail (rrb-container
                 t
                 (-> (rrb-path rrb-indexes shift t node-content)
                     cl-ds.common.rrb:rrb-node)
                 node-content)
    rrb-node)
(defun insert-tail (rrb-container ownership-tag continue tail)
  (declare (optimize (speed 3) (safety 1)
                     (space 0) (debug 0)))
  (bind (((:slots %size %shift %root) rrb-container)
         (root-overflow (>= (the fixnum (ash (the fixnum %size) (- +bit-count+)))
                            (ash 1 (* +bit-count+ (the shift %shift))))))
    (if root-overflow
        (iterate
          (repeat %shift)
          (for node
               initially (make-rrb-node :content tail
                                        :ownership-tag ownership-tag)
               then (let ((next (make-rrb-node :ownership-tag ownership-tag)))
                      (setf (nref next 0)
                            node)
                      next))
          (finally
           (bind ((root (make-rrb-node :ownership-tag ownership-tag))
                  ((:vectors content) (rrb-node-content root)))
             (setf (content 0) %root
                   (content 1) node)
             (return (values root t)))))
        (let ((path (make-array +maximal-shift+
                                :initial-element nil))
              (indexes (make-array +maximal-shift+
                                   :element-type `(integer 0 ,+maximum-children-count+))))
          (declare (dynamic-extent path)
                   (dynamic-extent indexes))
          (iterate
            (with size = (the non-negative-fixnum %size))
            (repeat %shift)
            (for i from 0)
            (for position from (* +bit-count+ %shift) downto 0 by +bit-count+)
            (for index = (ldb (byte +bit-count+ position) size))
            (for node
                 initially %root
                 then (and node (nref node index)))
            (setf (aref path i) node
                  (aref indexes i) index))
          (values (funcall continue
                           path
                           indexes
                           %shift
                           ownership-tag
                           tail)
                  nil)))))


(-> descend-into-tree
    (rrb-container
     rrb-index
     (-> (rrb-path rrb-indexes shift) t))
    t)
(defun descend-into-tree (rrb-container location continue)
  (declare (optimize (speed 3) (debug 0)
                     (safety 1) (space 0)))
  (let ((path (make-array +maximal-shift+
                          :initial-element nil))
        (shift (access-shift rrb-container))
        (indexes (make-array +maximal-shift+
                             :element-type `(integer 0 ,+maximum-children-count+))))
    (declare (dynamic-extent path)
             (dynamic-extent indexes))
    (iterate
      (for i from 0 to shift)
      (for position from (* +bit-count+ shift) downto 0 by +bit-count+)
      (for index = (ldb (byte +bit-count+ position) location))
      (for node
           initially (access-root rrb-container)
           then (and node (nref node index)))
      (setf (aref path i) node
            (aref indexes i) index))
    (funcall continue
             path
             indexes
             shift)))


(-> rrb-at (rrb-container rrb-index) t)
(defun rrb-at (container index)
  (declare (optimize (speed 3) (debug 0)
                     (safety 1) (space 0)))
  (if (< index (access-size container))
      (iterate
        (with shift = (slot-value container '%shift))
        (for position from (* +bit-count+ shift) downto 0 by +bit-count+)
        (for i = (ldb (byte +bit-count+ position) index))
        (for node
             initially (slot-value container '%root)
             then (nref node i))
        (finally (return node)))
      (let ((offset (- index (access-size container))))
        (~> container access-tail (aref offset)))))


(defmethod cl-ds:at ((container rrb-container) index &rest more)
  (cl-ds:assert-one-dimension more)
  (unless (< -1 index (cl-ds:size container))
    (error 'cl-ds:argument-value-out-of-bounds
           :argument 'index
           :bounds (list 0 (cl-ds:size container))
           :value index
           :format-control "Index out of bounds."))
  (check-type index rrb-index)
  (rrb-at container index))


(declaim (inline copy-on-write))
(-> copy-on-write
    (rrb-path rrb-indexes shift t node-content)
    cl-ds.common.rrb:rrb-node)
(defun copy-on-write (path indexes shift ownership-tag tail)
  (declare (optimize (speed 3) (debug 0)
                     (space 0) (safety 1)))
  (iterate
    (for i from (1- shift) downto 0)
    (for position = (aref indexes i))
    (for old-node = (aref path i))
    (for node
         initially (make-rrb-node :content tail
                                  :ownership-tag ownership-tag)
         then (if (null old-node)
                  (lret ((n (make-rrb-node
                             :content (~> tail
                                          array-element-type
                                          make-node-content)
                             :ownership-tag ownership-tag)))
                    (setf (nref n position) node))
                  (rrb-node-push-into-copy old-node
                                           position
                                           node
                                           ownership-tag)))
    (finally (return node))))


(defun acquire-path (path shift ownership-tag)
  (or
   (iterate
     (for i from 0 below shift)
     (for node = (aref path i))
     (finding i such-that (~> node
                              (acquire-ownership ownership-tag)
                              null)))
   shift))


(-> transactional-copy-on-write
    (rrb-path rrb-indexes shift t cl-ds.common.rrb:node-content)
    cl-ds.common.rrb:rrb-node)
(defun transactional-copy-on-write (path indexes shift ownership-tag tail)
  (iterate
    (with acquired = (acquire-path path shift ownership-tag))
    (for i from (1- shift) downto 0)
    (for position = (aref indexes i))
    (for old-node = (aref path i))
    (for node
         initially (make-rrb-node :content tail
                                  :ownership-tag ownership-tag)
         then (if (null old-node)
                  (lret ((n (make-rrb-node :content (make-node-content (array-element-type tail))
                                           :ownership-tag ownership-tag)))
                    (setf (nref n position) node))
                  (if (< i acquired)
                      (rrb-node-push! old-node
                                      position
                                      node)
                      (rrb-node-push-into-copy old-node
                                               position
                                               node
                                               ownership-tag))))
    (finally (return node))))


(declaim (inline destructive-write))
(-> destructive-write
    (rrb-path rrb-indexes shift t cl-ds.common.rrb:node-content)
    cl-ds.common.rrb:rrb-node)
(defun destructive-write (path indexes shift ownership-tag tail)
  (declare (optimize (speed 3) (debug 0)
                     (safety 1) (space 0)))
  (iterate
    (for i from (1- shift) downto 0)
    (for position = (aref indexes i))
    (for old-node = (aref path i))
    (for node
         initially (make-rrb-node :content tail
                                  :ownership-tag ownership-tag)
         then (if (null old-node)
                  (lret ((n (make-rrb-node :content (make-node-content (array-element-type tail))
                                           :ownership-tag ownership-tag)))
                    (setf (nref n position) node))
                  (rrb-node-push! old-node position node)))
    (finally (return node))))


(-> remove-tail (rrb-container) t)
(defun remove-tail (rrb-container)
  (declare (optimize (speed 3) (debug 0)
                     (safety 1) (space 0)))
  (bind (((:slots %size %shift %root) rrb-container)
         (root-underflow (eql (ash (the fixnum (- %size +maximum-children-count+))
                                   (the fixnum (- (* %shift +bit-count+))))
                              1)))
    (if (zerop %shift)
        (values nil (rrb-node-content %root) nil)
        (iterate
          (with last = (1- (the non-negative-fixnum %size)))
          (repeat %shift)
          (for position
               from (* +bit-count+ %shift)
               downto 0
               by +bit-count+)
          (for index = (ldb (byte +bit-count+ position) last))
          (for node
               initially %root
               then (nref node index))
          (finally
           (check-type node rrb-node)
           (return
             (values
              (if root-underflow
                  (nref %root 0)
                  %root)
              (rrb-node-content node)
              root-underflow)))))))


(defmethod cl-ds:size ((container rrb-container))
  (+ (access-size container)
     (access-tail-size container)))


(labels ((impl (function node depth)
           (if (zerop depth)
               (map nil function (rrb-node-content node))
               (map nil
                    (lambda (x) (impl function x (1- depth)))
                    (rrb-node-content node)))))
  (defmethod cl-ds:traverse ((object rrb-container) function)
    (let ((root (access-root object))
          (shift (access-shift object))
          (size (access-size object)))
      (if (zerop shift)
          (map nil function root)
          (iterate
            (with content = (rrb-node-content root))
            (for i from 0 to (ldb (byte +bit-count+ (* +bit-count+ shift))
                                  size))
            (impl function (aref content i) (1- shift)))))
    (iterate
      (with tail = (access-tail object))
      (for i from 0 below (access-tail-size object))
      (funcall function (aref tail i)))
    object))


(defclass rrb-range (cl-ds:fundamental-random-access-range)
  ((%start :type fixnum
           :accessor access-start)
   (%mutex :type bt:lock
           :reader read-mutex
           :initarg :mutex)
   (%last-size :type fixnum
               :accessor access-last-size)
   (%lower-bound :type fixnum
                 :accessor access-lower-bound)
   (%upper-bound :initarg :upper-bound
                 :type fixnum
                 :accessor access-upper-bound)
   (%initial-lower-bound :initarg :initial-lower-bound
                         :type fixnum
                         :reader read-initial-lower-bound)
   (%initial-upper-bound :initarg :initial-upper-bound
                         :type fixnum
                         :reader read-initial-upper-bound)
   (%content :reader read-content)
   (%container :initarg :container
               :accessor access-container))
  (:default-initargs :mutex (bt:make-lock)))


(defclass chunked-rrb-range (cl-ds:fundamental-forward-range)
  ((%vectors-in-chunk :initarg :vectors-in-chunk
                      :reader read-vectors-in-chunk)
   (%rrb-range :initarg :rrb-range
               :reader read-rrb-range)))


(defmethod cl-ds:clone ((range chunked-rrb-range))
  (make 'chunked-rrb-range
        :vectors-in-chunk (read-vectors-in-chunk range )
        :rrb-range (~> range read-rrb-range cl-ds:clone)))


(defmethod cl-ds:chunked ((range rrb-range) &optional chunk-size-hint)
  (make 'chunked-rrb-range
        :rrb-range (cl-ds:clone range)
        :vectors-in-chunk (if chunk-size-hint
                              (max 1 (truncate chunk-size-hint
                                               +maximum-children-count+))
                              1)))


(defmethod cl-ds:consume-front ((range chunked-rrb-range))
  (bind (((:slots %start %last-size %lower-bound %upper-bound %container
                  %initial-lower-bound %mutex %initial-upper-bound %content)
          (read-rrb-range range))
         ((:slots %vectors-in-chunk) range))
    (if (or (eql %lower-bound %upper-bound) (null %content))
        (values nil nil)
        (let* ((content %content)
               (start %start)
               (count 0)
               (lower-bound %lower-bound)
               (end +maximum-children-count+)
               (result-content (make-instance 'flexichain:standard-flexichain)))
          (incf count (- +maximum-children-count+ start))
          (incf %lower-bound count)
          (iterate
            (repeat %vectors-in-chunk)
            (flexichain:push-end result-content (flexichain:pop-start content))
            (unless (first-iteration-p)
              (incf %lower-bound +maximum-children-count+)
              (incf count +maximum-children-count+))
            (until (zerop (flexichain:nb-elements content))))
          (when (zerop (flexichain:nb-elements content))
            (setf %content nil
                  end %last-size))
          (setf %start 0)
          (values (make 'rrb-range
                        :start start
                        :last-size end
                        :content result-content
                        :initial-lower-bound lower-bound
                        :lower-bound lower-bound
                        :upper-bound (+ lower-bound count)
                        :initial-upper-bound (+ lower-bound count)
                        :container %container)
                  t)))))


(defmethod cl-ds:peek-front ((range chunked-rrb-range))
  (bind (((:slots %start %last-size %lower-bound %upper-bound %container
                  %initial-lower-bound %initial-upper-bound %content)
          (read-rrb-range range))
         ((:slots %vectors-in-chunk) range))
    (if (or (eql %lower-bound %upper-bound) (null %content))
        (values nil nil)
        (let* ((content %content)
               (start %start)
               (count 0)
               (lower-bound %lower-bound)
               (end +maximum-children-count+)
               (result-content (make-instance 'flexichain:standard-flexichain)))
          (incf count (- +maximum-children-count+ start))
          (incf %lower-bound count)
          (iterate
            (repeat %vectors-in-chunk)
            (for i from 0 below (flexichain:nb-elements content))
            (flexichain:push-end result-content (flexichain:element* content i))
            (unless (first-iteration-p)
              (incf count +maximum-children-count+)))
          (when (zerop (flexichain:nb-elements content))
            (setf end %last-size))
          (values (make 'rrb-range
                        :start start
                        :last-size end
                        :content result-content
                        :initial-lower-bound lower-bound
                        :lower-bound lower-bound
                        :upper-bound (+ lower-bound count)
                        :initial-upper-bound (+ lower-bound count)
                        :container %container)
                  t)))))


(defclass mutable-rrb-range (rrb-range)
  ())


(defmethod cl-ds:whole-range ((container rrb-container))
  (make 'rrb-range :container container
                   :initial-lower-bound 0
                   :initial-upper-bound (cl-ds:size container)))


;; TODO very innefficient!
(defun init-rrb (instance container &key (from 0) (to (cl-ds:size container)))
  (bind (((:slots %start %last-size %content %lower-bound
                  %upper-bound %size)
          instance)
         ((:slots %root %shift %size %tail-size %tail)
          container)
         (size (- to from)))
    (setf %content (make-instance 'flexichain:standard-flexichain
                                  :min-size +maximum-children-count+)
          %start (rem from +maximum-children-count+)
          %lower-bound from
          %last-size (let ((s (rem to +maximum-children-count+)))
                       (if (zerop s)
                           +maximum-children-count+
                           s))
          %upper-bound to)
    (labels ((collect-bottom (node depth)
               (if (zerop depth)
                   (progn
                     (flexichain:push-end %content
                                          (the node-content (rrb-node-content node)))
                     (decf size +maximum-children-count+))
                   (iterate
                     (for i from 0 below +maximum-children-count+)
                     (collect-bottom (nref node i)
                                     (1- depth))
                     (unless (> size +maximum-children-count+)
                       (leave))))))
      (if (zerop %shift)
          (unless (null %root)
            (flexichain:push-end %content
                                 (the node-content (rrb-node-content %root))))
          (iterate
            (with content = (rrb-node-content %root))
            (for i from 0 to (ldb (byte +bit-count+ (* +bit-count+ %shift))
                                  %size))
            (collect-bottom (aref content i) (1- %shift))
            (unless (> size +maximum-children-count+)
              (leave)))))
    (let ((skip-front-count (truncate from +maximum-children-count+)))
      (iterate
        (repeat skip-front-count)
        (flexichain:pop-start %content))
      (when (> size 0)
        (flexichain:push-end %content (the node-content %tail))))))


(defmethod initialize-instance :after ((instance rrb-range)
                                       &key container
                                       &allow-other-keys)
  (init-rrb instance container
            :from (read-initial-lower-bound instance)
            :to (read-initial-upper-bound instance)))


(defmethod reinitialize-instance (instance &key &allow-other-keys)
  (init-rrb instance (access-container instance)
            :from (read-initial-lower-bound instance)
            :to (read-initial-upper-bound instance)))


(defmethod cl-ds:peek-front ((range rrb-range))
  (bind (((:slots %start %content) range))
    (if (null %content)
        (values nil nil)
        (~> (flexichain:element* %content 0)
            (aref %start)
            (values t)))))


(defmethod cl-ds:peek-back ((range rrb-range))
  (bind (((:slots %tail-size %content) range))
    (if (null %content)
        (values nil nil)
        (~> %content
            (flexichain:element* (~> %content flexichain:nb-elements 1-))
            (aref (1- %tail-size))
            (values t)))))


(defmethod cl-ds:at ((range rrb-range) index &rest more)
  (cl-ds:assert-one-dimension more)
  (bind (((:slots %upper-bound %lower-bound %content) range))
    (unless (and (>= index %lower-bound) (< index %upper-bound))
      (error 'cl-ds:argument-value-out-of-bounds
             :argument 'index
             :bounds (list %lower-bound %upper-bound)
             :value index
             :format-control "Index out of bounds."))
    (let* ((index (- index %lower-bound))
           (which-array (ash index (- +bit-count+)))
           (array-index (logand index (lognot +tail-mask+))))
      (~> %content
          (flexichain:element* which-array)
          (aref array-index)))))


(defmethod cl-ds:consume-front ((range rrb-range))
  (bind (((:slots %start %content %last-size %lower-bound %upper-bound)
          range))
    (if (or (eql %lower-bound %upper-bound) (null %content))
        (values nil nil)
        (let* ((new-start (rem (1+ %start) +maximum-children-count+))
               (old-start %start)
               (reached-tail (eql 1 (flexichain:nb-elements %content)))
               (first-array (flexichain:element* %content 0)))
          (when (and reached-tail (eql old-start %last-size))
            (setf %content nil)
            (return-from cl-ds:consume-front (values nil nil)))
          (when (zerop new-start)
            (flexichain:pop-start %content))
          (setf %start new-start)
          (incf %lower-bound)
          (~> first-array
              (aref old-start)
              (values t))))))


(defmethod cl-ds:consume-back ((range rrb-range))
  (bind (((:slots %start %content %last-size %upper-bound) range))
    (if (null %content)
        (values nil nil)
        (let* ((new-end (if (eql %last-size 1)
                            +maximum-children-count+
                            (1- %last-size)))
               (old-end %last-size)
               (position (1- %last-size))
               (reached-tail (eql 1 (flexichain:nb-elements %content)))
               (last-array (~>> %content
                                flexichain:nb-elements
                                1-
                                (flexichain:element* %content))))
          (when (and reached-tail (eql %start position))
            (setf %content nil)
            (return-from cl-ds:consume-back (values nil nil)))
          (when (eql old-end 1)
            (flexichain:pop-end %content))
          (setf %last-size new-end)
          (decf %upper-bound)
          (~> last-array
              (aref position)
              (values t))))))


(defmethod cl-ds:across ((range rrb-range) function)
  (bind (((:slots %start %lower-bound %upper-bound %content %last-size) range)
         (index %start)
         (last-position (~> %content flexichain:nb-elements 1-)))
    (iterate
      (for i from 0 below (flexichain:nb-elements %content))
      (for array = (flexichain:element* %content i))
      (for end = (eql i last-position))
      (iterate
        (for a from index below (if end %last-size +maximum-children-count+))
        (funcall function (aref array a)))
      (setf index 0))
    range))


(defmethod cl-ds:traverse ((range rrb-range) function)
  (bind (((:slots %start %lower-bound %upper-bound %content %last-size) range)
         (index %start)
         (last-position (~> %content flexichain:nb-elements 1-)))
    (iterate
      (for i from 0)
      (until (eql %lower-bound %upper-bound))
      (until (zerop (flexichain:nb-elements %content)))
      (for array = (flexichain:element* %content 0))
      (iterate
        (with end = (eql i last-position))
        (with last-index = (if end %last-size +maximum-children-count+))
        (for a from index below last-index)
        (setf %start (1+ a))
        (incf %lower-bound)
        (when (eql %start last-index)
          (flexichain:pop-start %content))
        (funcall function (aref array a))
        (until (eql %lower-bound %upper-bound)))
      (setf index 0))
    range))


(defmethod cl-ds:reset! ((obj rrb-range))
  (reinitialize-instance obj)
  obj)


(defmethod cl-ds:size ((obj rrb-range))
  (bind (((:slots %upper-bound %lower-bound) obj))
    (- %upper-bound %lower-bound)))


(defmethod cl-ds:clone ((obj rrb-range))
  (bind (((:slots %lower-bound %upper-bound %container) obj))
    (make (type-of obj)
          :container %container
          :initial-lower-bound %lower-bound
          :initial-upper-bound %upper-bound)))


(defmethod (setf cl-ds:peek-back) (new-value (range mutable-rrb-range))
  (bind (((:slots %tail-size %content) range))
    (if (null %content)
        (error 'cl-ds:operation-not-allowed
               :format-control "Can't assign into empty range!")
        (setf (aref (~> %content
                        (flexichain:element* (~> %content
                                                 flexichain:nb-elements
                                                 1-)))
                    (1- %tail-size))
              new-value))))


(defmethod (setf cl-ds:peek-front) (new-value (range mutable-rrb-range))
  (bind (((:slots %start %content) range))
    (if (null %content)
        (error 'cl-ds:operation-not-allowed
               :format-control "Can't assign into empty range!")
        (setf (aref (flexichain:element* %content 0)
                    %start)
              new-value))))


(defmethod (setf cl-ds:at) (new-value (range mutable-rrb-range)
                            index &rest more)
  (cl-ds:assert-one-dimension more)
  (bind (((:slots %upper-bound %lower-bound %content) range))
    (unless (and (>= index %lower-bound) (< index %upper-bound))
      (error 'cl-ds:argument-value-out-of-bounds
             :argument 'index
             :bounds (list %lower-bound %upper-bound)
             :value index
             :format-control "Index out of bounds."))
    (let* ((index (- index %lower-bound))
           (which-array (ash index (- +bit-count+)))
           (array-index (logand index (lognot +tail-mask+))))
      (setf (aref (~> %content
                      (flexichain:element* which-array))
                  array-index)
            new-value))))


(defun sparse-rrb-tree-size (tree depth)
  (declare (optimize (speed 3)))
  (check-type depth fixnum)
  (if (zerop depth)
      (if (cl-ds.meta:null-bucket-p tree)
          0
          (sparse-rrb-node-size tree))
      (iterate
        (declare (type fixnum i))
        (for i from 0 below +maximum-children-count+)
        (when (sparse-rrb-node-contains tree i)
          (sum (sparse-rrb-tree-size (sparse-nref tree i)
                                     (1- depth)))))))
