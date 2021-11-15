(cl:in-package #:cl-data-structures.common.hamt)

#|

Basic types

|#

(eval-always
  (define-constant +hash-level+ 5)
  (define-constant +maximum-children-count+ (ash 1 +hash-level+))
  (define-constant +depth+ (floor (/ 64 +hash-level+))))


(deftype hash-node-index ()
  `(integer 0 ,+maximum-children-count+))


(deftype hash-node-size ()
  `(integer 0 ,(1+ +maximum-children-count+)))


(deftype hash-mask ()
  `(unsigned-byte ,+maximum-children-count+))


(deftype node-path ()
  `(simple-vector ,+depth+))


(deftype index-path ()
  `(simple-array fixnum (,+depth+)))

#|

Macros

|#

(defmacro hash-do ((node index &optional (count (gensym)))
                   (root hash)
                   &key on-leaf on-nil on-every)
  "Macro used for writing code going down into hash tree."
  (with-gensyms (!pos !block !leaf)
    (once-only (hash root)
      `(block ,!block
         (do ((,!pos ,+hash-level+ (the fixnum (+ (the fixnum ,!pos)
                                                  ,+hash-level+)))
              (,index (ldb (byte ,+hash-level+ 0) ,hash)
                      (ldb (byte ,+hash-level+ ,!pos) ,hash))
              (,count 0 (1+ ,count))
              (,node ,root (and (hash-node-contains ,node ,index) (hash-node-access ,node ,index))))
             (nil nil)
           (declare (type fixnum ,hash ,!pos ,index ,count)
                    (dynamic-extent ,!pos ,index ,count))
           (let ((,!leaf (and ,node (not (hash-node-p ,node)))))
             ,(when on-nil
                `(when (or (cl-ds.meta:null-bucket-p ,node) (null ,node))
                   (return-from ,!block
                     ,on-nil)))
             ,(when on-leaf
                `(when ,!leaf
                   (return-from ,!block
                     ,on-leaf)))
             (when (or ,!leaf (cl-ds.meta:null-bucket-p ,node) (null ,node))
               (return-from ,!block ,node))
             ,on-every
             (when (eql ,count ,+depth+)
               (values ,node ,count))))))))


(defmacro with-hamt-path (node hash root &key on-leaf on-nil operation)
  (with-gensyms (!count !path !indexes !root !index)
    `(let* ((,!path (make-array ,+depth+))
            (,!indexes (make-array ,+depth+ :element-type 'fixnum))
            (,!root ,root))
       (declare (type index-path ,!indexes)
                (type node-path ,!path)
                (dynamic-extent ,!path ,!indexes))
       (hash-do
           (,node ,!index ,!count)
           (,!root ,hash)
           :on-every (setf (aref ,!path ,!count) ,node
                           (aref ,!indexes ,!count) ,!index)
           :on-nil (let ((next ,on-nil))
                     (,operation ,!indexes ,!path ,!count next))
           :on-leaf (let ((next ,on-leaf))
                      (,operation ,!indexes ,!path ,!count next))))))


(defmacro with-destructive-erase-hamt (node container hash &key on-leaf on-nil)
  (with-gensyms (!path !depth !indexes !rewrite)
    (once-only (container)
      `(flet ((,!rewrite (,!indexes ,!path ,!depth conflict) ;path and indexes have constant size BUT only part of it is used, that's why length is passed here
                (declare (type index-path ,!indexes)
                         (type node-path ,!path)
                         (type fixnum ,!depth))
                (cl-ds.utils:with-vectors (,!path ,!indexes)
                  (iterate
                    (for i from (1- ,!depth) downto 0) ;reverse order (starting from deepest node)
                    (for ,node = (,!path i))
                    (for index = (,!indexes i))
                    (for ac
                         initially conflict
                         then (if (or (cl-ds.meta:null-bucket-p ac) (null ac))
                                  (if (eql 1 (hash-node-size ,node))
                                      ac
                                      (hash-node-remove! ,node index))
                                  (progn (hash-node-replace! ,node ac index)
                                         (leave (aref ,!path 0)))))
                    (finally (return ac))))))
         (declare (dynamic-extent (function ,!rewrite))
                  (inline ,!rewrite))
         (with-hamt-path ,node
           ,hash
           (access-root ,container)
           :on-leaf ,on-leaf
           :on-nil ,on-nil
           :operation ,!rewrite)))))


(declaim (inline hash-node-node-mask))
(declaim (inline hash-node-content))
(declaim (inline (setf hash-node-node-mask)))
(declaim (inline (setf hash-node-content)))
(declaim (inline hash-node-access))
(declaim (inline hash-node-size))
(declaim (inline hash-node-whole-mask))
(declaim (inline hash-node-to-masked-index))
(declaim (inline hash-node-contains))
(declaim (inline hash-node-contains-leaf))
(declaim (inline hash-node-contains-node))
(declaim (inline copy-node))
(declaim (inline hash-node-replace-in-the-copy))
(declaim (inline hash-node-insert-into-copy))
(declaim (inline hash-node-deep-copy))
(declaim (inline round-size))
(defun round-size (size)
  (ash (ceiling (/ size 8.0)) 3))

#|

Tree structure of HAMT

|#

(cl-ds.common.abstract:define-tagged-untagged-node hash-node
  (node-mask 0 :type hash-mask)
  (content #() :type simple-array))


(defun hash-node-p (node)
  (typep node 'hash-node))

#|

Interface class.

|#

(defclass hamt-container (fundamental-ownership-tagged-object)
  ((%root :accessor access-root
          :initarg :root
          :documentation "Hash node pointing to root of the whole hash tree.")
   (%size :initarg :size
          :initform 0
          :type non-negative-fixnum
          :accessor access-size
          :documentation "How many elements are in there?")
   (%bucket-size
    :type positive-fixnum
    :initarg :bucket-size
    :reader read-bucket-size
    :initform 3)))

#|

Functions with basic bit logic.

|#

(-> set-in-node-mask (hash-node hash-node-index (integer 0 1)) hash-node)
(defun set-in-node-mask (node position bit)
  (declare (optimize (speed 3) (space 0) (safety 1)))
  (setf (ldb (byte 1 position) (hash-node-node-mask node)) bit)
  node)



(-> hash-node-whole-mask (hash-node) hash-mask)
(defun hash-node-whole-mask (node)
  (hash-node-node-mask node))


(-> hash-node-to-masked-index (hash-node (hash-node-index)) hash-node-index)
(defun hash-node-to-masked-index (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 1) (space 0)))
  (~>> hash-node
       hash-node-whole-mask
       (ldb (byte index 0))
       logcount))


(-> hash-node-contains (hash-node hash-node-index) boolean)
(defun hash-node-contains (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 1) (space 0)))
  (~>> (hash-node-whole-mask hash-node)
       (ldb-test (byte 1 index))))


(-> hash-node-contains-node (hash-node hash-node-index) boolean)
(defun hash-node-contains-node (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 1) (space 0)))
  (~>> (hash-node-node-mask hash-node)
       (ldb-test (byte 1 index))))


(-> hash-node-access (hash-node hash-node-index) t)
(defun hash-node-access (hash-node index)
  (declare (optimize (speed 3) (debug 0) (safety 1) (space 0)))
  (~>> (hash-node-to-masked-index hash-node index)
       (aref (hash-node-content hash-node))))


(-> hash-node-size (hash-node) hash-node-size)
(defun hash-node-size (node)
  (declare (optimize (speed 3) (debug 0) (safety 1) (space 0)))
  (logcount (hash-node-whole-mask node)))

#|

Copy nodes and stuff.

|#


(-> go-down-on-path (hamt-container fixnum function function function)
    (values t cl-ds.common:eager-modification-operation-status))
(defun go-down-on-path
    (container hash on-leaf on-nil after)
  (declare (optimize (speed 3)
                     (debug 0)
                     (safety 1)
                     (space 0))
           (type fixnum hash))
  (bind ((status nil)
         ((:dflet after (indexes path depth next))
          (funcall after indexes path depth next)))
    (values (block loop-block
              (with-hamt-path node hash
                (access-root container)
                :operation after
                :on-leaf (multiple-value-bind (b s c)
                             (funcall on-leaf node)
                           (setf status s)
                           (unless c
                             (return-from loop-block nil))
                           b)
                :on-nil (multiple-value-bind (b s c)
                            (funcall on-nil)
                          (setf status s)
                          (unless c
                            (return-from loop-block nil))
                          b)))
            status)))


(-> copy-node (hash-node &key
                         (:node-mask hash-mask)
                         (:ownership-tag t)
                         (:content simple-vector))
    hash-node)
(defun copy-node (node &key node-mask ownership-tag content)
  (declare (optimize (speed 3) (debug 0) (safety 1) (space 0)))
  (make-hash-node
   :ownership-tag (or ownership-tag (read-ownership-tag node))
   :node-mask (or node-mask (hash-node-node-mask node))
   :content (or content (hash-node-content node))))


(-> hash-node-replace-in-the-copy (hash-node t hash-node-index t) hash-node)
(defun hash-node-replace-in-the-copy (hash-node item index ownership-tag)
  (declare (optimize (speed 3)
                     (debug 0)
                     (safety 1)
                     (space 0)))
  (let* ((content (copy-array (hash-node-content hash-node)))
         (node-mask (hash-node-node-mask hash-node)))
    (declare (type hash-mask node-mask))
    (setf (aref content
                (logcount (ldb (byte index 0) node-mask)))
          item)
    (make-hash-node :node-mask node-mask
                    :ownership-tag ownership-tag
                    :content content)))


(-> hash-node-insert-into-copy (hash-node t hash-node-index t) hash-node)
(defun hash-node-insert-into-copy (hash-node content index ownership-tag)
  (declare (optimize (speed 3)
                     (debug 0)
                     (safety 1)
                     (space 0)))
  (bind ((position (hash-node-to-masked-index hash-node index))
         ((:vectors current-array new-array)
          (hash-node-content hash-node)
          (make-array (~> hash-node
                          hash-node-whole-mask
                          logcount
                          1+))))
    (assert (~> (array-dimension new-array 0)
                (<= +maximum-children-count+)))
    ;;before new element
    (iterate
      (for i from 0 below position)
      (setf (new-array i) (current-array i)))

    ;;new element
    (setf (new-array position)
          content)

    ;;after new element
    (iterate
      (for i from position below (array-dimension current-array 0))
      (for j from (1+ position) below (array-dimension new-array 0))
      (setf (new-array j) (current-array i)))

    ;;just make new hash-node
    (let ((node-mask (hash-node-node-mask hash-node)))
      (setf (ldb (byte 1 index) node-mask) 1)
      (make-hash-node :node-mask node-mask
                      :ownership-tag ownership-tag
                      :content new-array))))


(-> rebuild-rehashed-node (hamt-container fixnum t t) t)
(-> build-rehashed-node (hamt-container fixnum simple-vector t) t)
(defun build-rehashed-node (container depth content ownership-tag)
  (let ((mask 0)
        (node-mask 0)
        (size 0))
    (iterate
      (for elt in-vector content)
      (for index from 0)
      (when elt
        (incf size)
        (setf (ldb (byte 1 index) mask) 1)))
    (cl-ds.utils:with-vectors ((array (make-array (round-size size)
                                                  :initial-element nil)))
      (iterate
        (for conflict in-vector content)
        (for index from 0)
        (when conflict
          (for i = (logcount (ldb (byte index 0) mask)))
          (setf (array i)
                (rebuild-rehashed-node container
                                       depth
                                       conflict
                                       ownership-tag)
                (ldb (byte 1 index) node-mask) 1)))
      (make-hash-node :node-mask node-mask
                      :ownership-tag ownership-tag
                      :content array))))


(defun full-bucket-p (container bucket)
  (not
   (iterate
     (declare (ignorable elt))
     (with bucket-size = (read-bucket-size container))
     (for i from 1)
     (for elt in bucket)
     (always (< i bucket-size)))))


(defun rebuild-rehashed-node (container depth conflict ownership-tag)
  (declare (optimize (speed 0) (safety 1) (debug 3) (space 0))
           (type fixnum depth))
  (flet ((cont (array)
           (build-rehashed-node container (1+ depth) array ownership-tag)))
    (declare (dynamic-extent #'cont))
    (bind (((:accessors (lock hash-node-lock)
                        (tag hash-node-ownership-tag))
            conflict))
      (if (or (>= depth +depth+) (not (full-bucket-p container conflict)))
          conflict
          (rehash conflict
                  depth
                  #'cont)))))


(-> build-node (hash-node-index t t) hash-node)
(defun build-node (index content ownership-tag)
  (make-hash-node :node-mask (ash 1 index)
                  :ownership-tag ownership-tag
                  :content (make-array 1 :initial-element content)))


(-> hash-node-insert! (hash-node t hash-node-index) hash-node)
(defun hash-node-insert! (node content index)
  (assert (not (ldb-test (byte 1 index) (hash-node-whole-mask node))))
  (bind ((next-mask (~>> node
                         hash-node-whole-mask
                         (dpb 1 (byte 1 index))))
         (next-size (~> next-mask
                        logcount))
         (masked-index (~>> next-mask
                            (ldb (byte index 0))
                            logcount))
         ((:vectors s n)
          (hash-node-content node)
          (if (< (array-dimension s 0) next-size)
              (make-array (round-size next-size) :initial-element nil)
              s)))
    (unless (eq s n)
      (iterate
        (for i from 0 below masked-index)
        (setf (n i) (s i))))
    (iterate
      (for i from (1- next-size) downto (max 1 masked-index))
      (setf (n i) (s (1- i))))
    (setf (hash-node-content node) n
          (n masked-index) content)
    (set-in-node-mask node index 1)
    node))


(defun hash-node-replace! (node content index)
  (assert (not (zerop (ldb (byte 1 index) (hash-node-whole-mask node)))))
  (cl-ds.utils:with-vectors ((a (hash-node-content node)))
    (setf (a (hash-node-to-masked-index node index))
          content))
  node)


(-> hash-node-remove-from-the-copy (hash-node fixnum t) hash-node)
(-> hash-node-remove! (hash-node fixnum) hash-node)
(flet ((new-array (node index)
         (cl-ds.utils:copy-without (hash-node-content node)
                                   (1- (logcount (ldb (byte (1+ index) 0)
                                                      (hash-node-whole-mask node)))))))

  (defun hash-node-remove-from-the-copy (node index ownership-tag)
    "Returns copy of node, but without element under index. Not safe, does not check if element is actually present."
    (make-hash-node :node-mask (dpb 0 (byte 1 index) (hash-node-node-mask node))
                    :ownership-tag ownership-tag
                    :content (new-array node index)))


  (defun hash-node-remove! (node index)
    (declare (optimize (debug 0)
                       (speed 3)
                       (space 0)))
    (bind ((next-size (1- (logcount (hash-node-node-mask node))))
           (masked-index (1- (logcount (the fixnum
                                            (ldb (byte (1+ index) 0)
                                                 (hash-node-whole-mask node))))))
           ((:vectors s n)
            (hash-node-content node)
            (if (> (array-dimension s 0)
                   (ash next-size 1))
                (make-array (round-size next-size))
                s)))
      (set-in-node-mask node index 0)
      (unless (eq s n)
        (iterate
          (for i from 0 below masked-index)
          (setf (n i) (s i))))
      (iterate
        (for i from masked-index below next-size)
        (setf (n i) (s (1+ i))))
      (setf (hash-node-content node) n)
      node)))


(defun rehash (conflict level cont)
  (declare (type list conflict)
           (optimize (speed 3)
                     (safety 1)
                     (debug 0)
                     (space 0)))
  (let ((result (make-array +maximum-children-count+ :initial-element nil))
        (byte (byte +hash-level+ (the fixnum
                                      (* (the fixnum +hash-level+)
                                         (the fixnum level))))))
    (declare (dynamic-extent byte)
             (dynamic-extent result))
    (iterate
      (for item in conflict)
      (for hash = (cl-ds.common:hash-content-hash item))
      (for index = (ldb byte hash))
      (push
       item
       (aref result index)))
    (funcall cont result)))


(-> copy-on-write (hamt-container t (vector fixnum) vector fixnum t) t)
(defun copy-on-write (container ownership-tag indexes path depth conflict)
  (declare (type index-path indexes)
           (type node-path path)
           (type fixnum depth)
           (optimize (speed 3)
                     (safety 1)
                     (debug 0)
                     (space 0)))
  (cl-ds.utils:with-vectors (path indexes)
    (when (and (not (zerop depth)) (eq conflict (path (- depth 1))))
      (return-from copy-on-write (path 0)))
    (iterate
      (for i from (- depth 1) downto 0) ;reverse order (starting from deepest node)
      (for node = (path i))
      (for index = (indexes i))
      (for ac initially (if (or (cl-ds.meta:null-bucket-p conflict) (null conflict))
                            ;;if we didn't find element or element was found but depth was already maximal,
                            ;;we will just return element, otherwise attempt to divide (rehash) conflicting node into few more
                            conflict
                            (rebuild-rehashed-node container
                                                   depth
                                                   conflict
                                                   ownership-tag))
           then (if ac
                    (if (hash-node-contains node index)
                        (hash-node-replace-in-the-copy node ac index ownership-tag)
                        (hash-node-insert-into-copy node ac index ownership-tag))
                    (if (eql 1 (hash-node-size node))
                        ac
                        (hash-node-remove-from-the-copy node index ownership-tag))))
      (finally (return ac)))))


(-> transactional-copy-on-write
    (hamt-container t (vector fixnum) vector fixnum (or list symbol))
    t)
(defun transactional-copy-on-write (container
                                    ownership-tag
                                    indexes
                                    path
                                    depth
                                    conflict)
  (declare (type (simple-array fixnum) indexes)
           (type simple-array path)
           (type fixnum depth)
           (optimize (speed 3)
                     (safety 1)
                     (debug 0)
                     (space 0)))
  (cl-ds.utils:with-vectors (path indexes)
    (iterate
      (with owned-depth = (iterate
                            (for i from 0 below depth)
                            (for node = (path i))
                            (while (acquire-ownership (the hash-node node)
                                                      ownership-tag))
                            (finally (return i))))
      (for i from (- depth 1) downto 0) ;reverse order (starting from deepest node)
      (for node = (path i))
      (for index = (indexes i))
      (for ac
           initially (if (or (cl-ds.meta:null-bucket-p conflict) (null conflict))
           ;;if we didn't find element or element was found but depth was already maximal,
           ;;we will just return element, otherwise attempt to divide (rehash) conflicting node into few more
                         conflict
                         (rebuild-rehashed-node container
                                                depth
                                                conflict
                                                ownership-tag))
           then (if ac
                    (cl-ds.utils:cond+ ((hash-node-contains node index)
                                        (< i owned-depth))
                      ((t t) (hash-node-replace! node ac index))
                      ((nil t) (hash-node-insert! node ac index))
                      ((t nil) (hash-node-replace-in-the-copy node
                                                              ac
                                                              index
                                                              ownership-tag))
                      ((nil nil) (hash-node-insert-into-copy node
                                                             ac
                                                             index
                                                             ownership-tag)))
                    (cl-ds.utils:cond+ ((eql 1 (hash-node-size node))
                                        (< i owned-depth))
                      ((t t) ac)
                      ((t nil) ac)
                      ((nil t) (hash-node-remove! node index))
                      ((nil nil) (hash-node-remove-from-the-copy node
                                                                 index
                                                                 ownership-tag)))))
      (when (eq node ac)
        (leave (path 0)))
      (finally (return ac)))))


(-> hash-node-deep-copy (hash-node t) hash-node)
(defun hash-node-deep-copy (node ownership-tag)
  (make-hash-node :node-mask (hash-node-node-mask node)
                  :content (copy-array (hash-node-content node))
                  :ownership-tag ownership-tag))


(defstruct hamt-range-stack-cell
  (start 0 :type non-negative-fixnum)
  (end 0 :type non-negative-fixnum)
  node)


(defmethod cl-ds:clone ((cell hamt-range-stack-cell))
  (make-hamt-range-stack-cell
   :start (hamt-range-stack-cell-start cell)
   :end (hamt-range-stack-cell-end cell)
   :node (hamt-range-stack-cell-node cell)))


(defun new-cell (node)
  (cond
    ((consp node) node)
    ((hash-node-p node) (make-hamt-range-stack-cell
                         :start 0
                         :end (1- (hash-node-size node))
                         :node node))
    (t (error "Logic Error!"))))


(defun forward-cell (cell)
  (cond
    ((hamt-range-stack-cell-p cell)
     (values
      (new-cell (~> cell hamt-range-stack-cell-node hash-node-content
                    (aref (hamt-range-stack-cell-start cell))))
      (unless (eql (hamt-range-stack-cell-start cell) (hamt-range-stack-cell-end cell))
        (make-hamt-range-stack-cell
         :start (1+ (hamt-range-stack-cell-start cell))
         :end (hamt-range-stack-cell-end cell)
         :node (hamt-range-stack-cell-node cell)))))
    ((listp cell)
     (values
      (first cell)
      (rest cell)))
    (t (error "Logic Error!"))))


(defun obtain-value (pull push)
  (iterate
    (for old-cell = (funcall pull))
    (for (values new-cell modified-cell) = (forward-cell old-cell))
    (unless (null modified-cell) ;push if anything
      (funcall push modified-cell))
    (if (listp old-cell)
        (return-from obtain-value (values new-cell t))
        (unless (null new-cell)
          (funcall push new-cell)))))


(defgeneric get-range-key-function (container))


(defmethod cl-ds:whole-range ((container hamt-container))
  (make 'cl-ds.common:forward-tree-range
        :obtain-value #'obtain-value
        :key (get-range-key-function container)
        :forward-stack (list (new-cell (access-root container)))
        :container container))
