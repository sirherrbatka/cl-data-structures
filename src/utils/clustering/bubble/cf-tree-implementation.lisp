(cl:in-package #:cl-data-structures.utils.clustering.bubble)


(defun distance (tree first-item second-item)
  (funcall (read-distance-function tree) first-item second-item))


(defun grab-sample (tree family-size vector result)
  (let* ((node-size (length vector))
         (sample-size (read-sample-size tree))
         (contribution-size (max 1 (/ (* node-size sample-size)
                                      family-size))))
    (iterate
      (with shuffled = (cl-ds.utils:lazy-shuffle 0 node-size))
      (repeat contribution-size)
      (~> (funcall shuffled)
          (aref vector _)
          (vector-push-extend result)))
    result))


(defun vector-average-distance (distance-function vector item)
  (/ (reduce #'+ vector
             :key (lambda (x) (funcall distance-function x item)))
     (length vector)))


(defun split* (matrix vector)
  (bind ((length (length vector))
         ((first . second)
          (iterate outer
            (declare (type fixnum i))
            (for i from 0 below length)
            (iterate
              (declare (type fixnum j))
              (for j from i below length)
              (iterate
                (for k from 0 below length)
                (for i-distance = (if (= i k)
                                      0
                                      (cl-ds.utils:mref matrix i k)))
                (for j-distance = (if (= j k)
                                      0
                                      (cl-ds.utils:mref matrix j k)))
                (sum (abs (- i-distance j-distance)) into total)
                (finally
                 (in outer (finding (cons i j) maximizing total)))))))
         (first-content (vect first))
         (second-content (vect second))
         ((:flet vector-average-distance (vector index))
          (/ (reduce #'+ vector
                     :key (lambda (i) (cl-ds.utils:mref matrix index i)))
             (length vector))))
    (iterate
      (for i from 0 below length)
      (when (or (eql i first)
                (eql i second))
        (next-iteration))
      (for first-distance = (vector-average-distance first-content
                                                     i))
      (for second-distance = (vector-average-distance second-content
                                                      i))
      (vector-push-extend i (if (< first-distance second-distance)
                                first-content
                                second-content)))
    (cl-ds.utils:transform (lambda (i) (aref vector i)) first-content)
    (cl-ds.utils:transform (lambda (i) (aref vector i)) second-content)
    (cons first-content second-content)))


(defun cf-leaf-update-row-sums (tree leaf)
  (declare (optimize (speed 3)))
  (let* ((distance-function (read-distance-function tree))
         (content (read-content leaf))
         (row-sums (read-row-sums leaf))
         (length (length content))
         (new-row-sum 0)
         (last-index (1- length))
         (new-element (aref content last-index)))
    (declare (type (cl-ds.utils:extendable-vector t) content row-sums)
             (type fixnum length last-index)
             (type function distance-function))
    (iterate
      (declare (type fixnum i))
      (for i from 0 below (1- length))
      (for distance = (funcall distance-function
                               (aref content i)
                               new-element))
      (incf new-row-sum distance)
      (incf (aref row-sums i) distance))
    (vector-push-extend new-row-sum row-sums)
    nil))


(defun cf-leaf-update-clusteroid (tree leaf)
  (declare (optimize (speed 3))
           (ignore tree))
  (let* ((content (read-content leaf))
         (row-sums (read-row-sums leaf))
         (length (length content))
         (new-clusteroid (iterate
                           (declare (type fixnum i))
                           (for i from 0 below length)
                           (for row-sum = (aref row-sums i))
                           (finding i minimizing row-sum))))
    (declare (type cl-ds.utils:extendable-vector content row-sums)
             (type fixnum length))
    (rotatef (aref content new-clusteroid)
             (aref content 0))
    (rotatef (aref row-sums new-clusteroid)
             (aref row-sums 0))
    nil))


(defun cf-subtree-update-clusteroid (tree subtree)
  (iterate
    (with sample = (access-sample subtree))
    (with length = (length sample))
    (with distance-function = (read-distance-function tree))
    (for i from 0 below length)
    (for first-elt = (aref sample i))
    (for row-sum = (+ (iterate
                        (for j from 0 below i)
                        (sum (funcall distance-function
                                      first-elt
                                      (aref sample j))))
                      (iterate
                        (for j from i below length)
                        (sum (funcall distance-function
                                      first-elt
                                      (aref sample j))))))
    (finding i minimizing row-sum into position)
    (finally (rotatef (aref sample position) (aref sample 0)))))


(defun cf-leaf-calculate-radius (tree leaf)
  (declare (ignore tree))
  (let* ((content (read-content leaf)))
    (declare (type (cl-ds.utils:extendable-vector t) content))
    (~> leaf
        read-row-sums
        (aref 0)
        (/ (length content))
        sqrt)))


(defun average-inter-cluster-distance* (distance-function
                                        first-leaf
                                        second-leaf)
  (declare (optimize (speed 3) (safety 0)))
  (ensure-functionf distance-function)
  (iterate
    (declare (type (cl-ds.utils:extendable-vector t)
                   first-content second-content)
             (type number result)
             (type fixnum i first-length second-length))
    (with first-content = (read-content first-leaf))
    (with second-content = (read-content second-leaf))
    (with first-length = (length first-content))
    (with second-length = (length second-content))
    (with result = 0)
    (for i from 0 below first-length)
    (for first-elt = (aref first-content i))
    (iterate
      (declare (type fixnum j))
      (for j from 0 below second-length)
      (for second-elt = (aref second-content j))
      (incf result (funcall distance-function first-elt second-elt)))
    (finally (return
               (~> result
                   (/ (* first-length second-length))
                   (/ 2))))))


(defmethod clusteroid ((tree cf-tree) (node cf-leaf))
  (aref (read-content node) 0))


(defmethod clusteroid ((tree cf-tree) (node cf-subtree))
  (aref (access-sample node) 0))


(defmethod clusteroid-distance ((tree cf-tree)
                                (first-node fundamental-cf-node)
                                (second-node fundamental-cf-node))
  (distance tree
            (clusteroid tree first-node)
            (clusteroid tree second-node)))


(defmethod clusteroid-distance ((tree cf-tree)
                                (first-node fundamental-cf-node)
                                (item t))
  (distance tree
            (clusteroid tree first-node)
            item))


(defmethod needs-resampling-p ((tree cf-tree) (leaf cf-leaf))
  nil)


(defmethod needs-resampling-p ((tree cf-tree) (subtree cf-subtree))
  (or (null (access-sample subtree))
      (>= (access-inserts subtree) (read-sampling-rate tree))))


(defmethod needs-split-p ((tree cf-tree) (subtree cf-subtree))
  (>= (~> subtree read-children length)
      (read-subtree-maximum-arity tree)))


(defmethod needs-split-p ((tree cf-tree) (leaf cf-leaf))
  (or (>= (~> leaf read-content length)
          (read-leaf-maximum-size tree))
      (>= (access-radius leaf)
          (read-leaf-maximum-radius tree))))


(defmethod average-distance ((tree cf-tree)
                             (first-leaf cf-leaf)
                             (second-leaf cf-leaf))
  (average-inter-cluster-distance* (read-distance-function tree)
                                   first-leaf second-leaf))


(defmethod average-distance ((tree cf-tree)
                             (leaf cf-leaf)
                             item)
  (vector-average-distance (read-distance-function tree)
                           (read-content leaf)
                           item))


(defmethod average-distance ((tree cf-tree)
                             first-item
                             second-item)
  (distance tree first-item second-item))


(defmethod average-distance ((tree cf-tree)
                             (node cf-subtree)
                             item)
  (vector-average-distance (read-distance-function tree)
                           (access-sample node)
                           item))


(defmethod split ((tree cf-tree)
                  (node cf-leaf))
  (bind ((content (read-content node))
         (result (vector (make-leaf tree) (make-leaf tree)))
         (distance-function (read-distance-function tree))
         ((first-content . second-content)
          (split* (cl-ds.utils:make-distance-matrix-from-vector
                   t distance-function
                   content)
                  content)))
    (absorb-nodes tree (aref result 0) first-content)
    (absorb-nodes tree (aref result 1) second-content)
    result))


(defmethod leaf-content ((tree cf-tree)
                         (node cf-leaf))
  (read-content node))


(defmethod visit-leafs ((tree cf-tree)
                        (node cf-subtree)
                        callback
                        &key (key #'identity))
  (iterate
    (for child in-vector (read-children node))
    (visit-leafs tree child callback :key key)))


(defmethod visit-leafs ((tree cf-tree)
                        (node cf-leaf)
                        callback
                        &key (key #'identity))
  (~>> (funcall key node)
       (funcall callback)))


(defmethod split ((tree cf-tree)
                  (node cf-subtree))
  (bind ((children (read-children node))
         (result (vector (make-subtree tree) (make-subtree tree)))
         ((first-content . second-content)
          (~> (lambda (a b) (average-distance tree a b))
              (cl-ds.utils:make-distance-matrix-from-vector t _ children)
              (split* children))))
    (absorb-nodes tree (aref result 0) first-content)
    (absorb-nodes tree (aref result 1) second-content)
    (when (needs-resampling-p tree (aref result 0))
      (resample tree (aref result 0)))
    (when (needs-resampling-p tree (aref result 1))
      (resample tree (aref result 1)))
    result))


(defmethod node-size ((tree cf-tree)
                      (node cf-subtree))
  (~> node access-sample length))


(defmethod node-size ((tree cf-tree)
                      (node cf-leaf))
  (~> node read-content length))


(defmethod contribute-sample ((tree cf-tree)
                              (node cf-leaf)
                              result
                              family-size)
  (grab-sample tree family-size (read-content node) result))


(defmethod contribute-sample ((tree cf-tree)
                              (node cf-subtree)
                              result
                              family-size)
  (grab-sample tree family-size (read-children node) result))


(defmethod resample ((tree cf-tree)
                     (node cf-subtree))
  (let ((sample (or (access-sample node) (vect))))
    (setf (fill-pointer sample) 0)
    (iterate
      (with children = (read-children node))
      (with family-size = (reduce #'+ children
                                  :key (curry #'node-size tree)))
      (for child in-vector children)
      (contribute-sample tree child sample family-size))
    (setf (access-sample node) sample
          (access-inserts node) 0)))


(defmethod cf-insert ((tree cf-tree)
                      (node cf-subtree)
                      item)
  (incf (access-inserts node))
  (iterate
    (with children = (read-children node))
    (with length = (length children))
    (for i from 0 below length)
    (for child = (aref children i))
    (for distance = (average-distance tree child item))
    (finding i minimizing distance into position)
    (finally (return (cons (aref children position) i)))))


(defmethod cf-insert ((tree cf-tree)
                      (node cf-leaf)
                      item)
  (vector-push-extend item (read-content node))
  (cf-leaf-update-row-sums tree node)
  (setf (access-radius node) (cf-leaf-calculate-radius tree node))
  nil)


(defmethod absorb-nodes ((tree cf-tree)
                         (parent cf-subtree)
                         children
                         &optional (position nil position-bound))
  (bind ((old-children (read-children parent))
         (length (fill-pointer old-children))
         (size (array-dimension old-children 0))
         (new-size (+ length (length children) -1)))
    (declare (type fixnum length size new-size))
    (when position-bound
      (cl-ds.utils:swapop old-children position))
    (when (> new-size size)
      (adjust-array old-children new-size))
    (iterate
      (for child in-vector children)
      (vector-push-extend child old-children))
    parent))


(defmethod absorb-nodes ((tree cf-tree)
                         (parent cf-leaf)
                         (content vector)
                         &optional (position nil position-bound))
  (let* ((old-content (read-content parent))
         (length (fill-pointer old-content))
         (size (array-dimension old-content 0))
         (new-size (+ length (length content))))
    (declare (type fixnum length size new-size))
    (when position-bound
      (cl-ds.utils:swapop old-content position)
      (cl-ds.utils:swapop (read-row-sums parent) position))
    (when (> new-size size)
      (adjust-array old-content new-size))
    (iterate
      (for child in-vector content)
      (vector-push-extend child old-content))
    (cf-leaf-update-row-sums tree parent)
    (cf-leaf-update-clusteroid tree parent)
    (setf (access-radius parent) (cf-leaf-calculate-radius tree parent))
    parent))


(defmethod make-leaf ((tree cf-tree))
  (let ((leaf-maximum-size (read-leaf-maximum-size tree)))
    (make 'cf-leaf
          :content #1=(make-array leaf-maximum-size
                                  :fill-pointer 0
                                  :adjustable t)
          :row-sums #1#)))


(defmethod make-subtree ((tree cf-tree))
  (make 'cf-subtree
        :children (make-array (read-subtree-maximum-arity tree)
                              :fill-pointer 0
                              :adjustable t)
        :sample (make-array (read-subtree-sample-size tree)
                            :fill-pointer 0
                            :adjustable t)))
