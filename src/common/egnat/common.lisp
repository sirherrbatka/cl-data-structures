(cl:in-package #:cl-data-structures.common.egnat)


(defun select-seeds (branching-factor end &optional (start 0))
  (iterate
    (with generator = (cl-ds.utils:lazy-shuffle start end))
    (with result = (make-array branching-factor
                               :element-type 'fixnum))
    ;; map index of seed to index of children
    (for k from 0 below branching-factor)
    (for i = (funcall generator))
    (while i)
    (setf (aref result k) i)
    (for j from 0)
    (finally (return (adjust-array result k)))))


(defun select-best-seeds (container data &optional (start 0) parallel)
  (declare (type vector data)
           (optimize (speed 3)))
  (let ((samples-count (read-samples-count container))
        (data-count (length data))
        (branching-factor (read-branching-factor container)))
    (~> (funcall (if parallel #'lparallel:pmap-into #'map-into)
                 (make-array samples-count)
                  (lambda ()
                    (iterate
                      (declare (type number final)
                               (type fixnum length i)
                               (type (simple-array fixnum (*)) seeds))
                      (with final = 0)
                      (with seeds = (select-seeds branching-factor
                                                  data-count
                                                  start))
                      (with length = (length seeds))
                      (for i from 0 below length)
                      (for first-seed = (~>> (aref seeds i) (aref data)))
                      (iterate
                        (declare (type fixnum j))
                        (for j from 0 below i)
                        (for second-seed = (~>> (aref seeds j) (aref data)))
                        (for distance = (distance container first-seed second-seed))
                        (incf final distance))
                      (finally (return (cons final seeds))))))
        (extremum #'> :key #'car)
        cdr)))


(defun make-partitions (container seeds-indexes data)
  (let ((result (make-array (cl-ds:size data) :element-type 'fixnum))
        (index 0))
    (cl-ds:across data
                  (lambda (value)
                    (setf (aref result (finc index))
                          (iterate
                            (for seed-index in-vector seeds-indexes)
                            (for seed = (cl-ds:at data seed-index))
                            (for distance = (distance container value seed))
                            (minimize distance into min)
                            (for result
                                 initially 0
                                 then (if (= distance min)
                                          seed-index
                                          result))
                            (finally (return result))))))
    result))


(defun make-ranges (container contents)
  (bind ((branching-factor (read-branching-factor container))
         (range-size `(,branching-factor ,branching-factor))
         (metric-type (read-metric-type container))
         (close-range (make-array range-size
                                  :element-type metric-type))
         (distant-range (make-array range-size
                                    :element-type metric-type)))
    (iterate
      (for child from 0 below (length contents))
      (for head = (~> (aref contents child) (aref 0)))
      (iterate
        (declare (type fixnum data))
        (for data from 0 below (length contents))
        (when (= child data)
          (next-iteration))
        (for init = (distance container head
                              (~> contents
                                  (aref data)
                                  (aref 0))))
        (for (values min max) =
             (cl-ds.utils:optimize-value ((mini < init) (maxi > init))
               (map nil (lambda (x)
                          (let ((distance (distance container x head)))
                            (mini distance)
                            (maxi distance)))
                    (aref contents data))))
        (setf (aref close-range child data) min
              (aref distant-range child data) max)))
    (values close-range distant-range)))


(defun average-pairwise-distance (container seeds content)
  (declare (type vector content)
           (type vector seeds)
           (type fundamental-egnat-container container))
  (iterate outer
    (with length = (length seeds))
    (for i from 0 below length)
    (iterate
      (for j from 0 below i)
      (in outer (sum (distance container
                               (~>> (aref seeds i)
                                    (aref content))
                               (~>> (aref seeds j)
                                    (aref content))))))))


(defgeneric become-subtree* (container node))


(defun become-subtree (container node)
  (become-subtree* container node))


(defmethod become-subtree* (container (node egnat-subtree))
  node)


(defmethod become-subtree* (container (node egnat-node))
  (bind ((content (read-content node))
         (new-content (aref content 0))
         (seeds (select-best-seeds container content 1))
         (split (assign-children container seeds content 1))
         ((:values close-range distant-range) (make-ranges container
                                                           split))
         (subtree (change-class
                   node 'egnat-subtree
                   :content new-content
                   :children (map-into (make-children-vector container)
                                       (lambda (new-content)
                                         (assert (vectorp new-content))
                                         (lret ((result (make 'egnat-node
                                                              :content new-content)))
                                           (when (node-full-p container result)
                                             (become-subtree container result))))
                                       split)
                   :close-range close-range
                   :distant-range distant-range)))
    subtree))


(defun make-egnat-tree (container operation extra-arguments data &optional (parallel nil))
  (if parallel
      (bind ((seeds (select-best-seeds container data 1 parallel))
             (subtree-contents (assign-children container seeds data 1 parallel))
             (this-content (first-elt data))
             ((:values close distant) (make-ranges container subtree-contents))
             (children (lparallel:pmap-into (make-children-vector container)
                                            (lambda (x)
                                              (make-egnat-tree container operation
                                                               extra-arguments
                                                               x nil))
                                            subtree-contents)))
        (make 'egnat-subtree :close-range close
                             :distant-range distant
                             :content this-content
                             :children children))
      (bind ((result (make 'egnat-node :content (make-content-vector container))))
        (map nil
             (lambda (x)
               (recursive-egnat-grow!* result container operation
                                       x extra-arguments))
             data)
        result)))


(defun prune-subtrees (container trees
                       close-range distant-range
                       value margin)
  (declare (type (or null simple-array) close-range distant-range)
           (type (array t (*)) trees))
  (iterate
    (declare (type fixnum i)
             (type simple-bit-vector result))
    (with length = (length trees))
    (with result = (make-array length
                               :element-type 'bit
                               :initial-element 1))
    (for i from 0 below length)
    (for tree = (aref trees i))
    (for head = (node-head tree))
    (for distance = (distance container head value))
    (iterate
      (declare (type fixnum j))
      (for j from 0 below length)
      (unless (or (eql i j) (zerop (aref result j)))
        (let ((in-range (<= (- (aref close-range i j)
                               margin)
                            distance
                            (+ (aref distant-range i j)
                               margin))))
          (setf (aref result j) (if in-range 1 0)))))
    (finally (return result))))


(defun node-full-p (container node)
  (assert (typep node 'egnat-node))
  (>= (~> node read-content length)
      (read-content-count-in-node container)))

(defun node-children-full-p (container node)
  (bind ((children (read-children node))
         (children-count (fill-pointer children))
         (branching-factor (read-branching-factor container)))
    (= children-count branching-factor)))


(defun closest-node (container nodes item)
  (iterate
    (for node in-vector nodes)
    (for content = (read-content node))
    (for head = (aref content 0))
    (for distance = (distance container head item))
    (minimize distance into min)
    (for result
         initially node
         then (if (= distance min)
                  node
                  result))
    (finally (return result))))


(defun visit-every-bucket (function parent &optional (stack (vect)))
  (setf (fill-pointer stack) 0)
  (vector-push-extend parent stack)
  (iterate
    (until (emptyp stack))
    (for node = (aref stack (~> stack fill-pointer 1-)))
    (decf (fill-pointer stack))
    (for content = (read-content node))
    (if (typep node 'egnat-node)
        (map nil function content)
        (progn
          (funcall function content)
          (map nil
               (rcurry #'vector-push-extend stack)
               (read-children node))))))


(-> calculate-distances (fundamental-egnat-container t egnat-node
                                                     &optional vector)
    t)
(defun calculate-distances (container item parent &optional (stack (vect)))
  (cl-ds.utils:optimize-value ((mini <) (maxi >))
    (visit-every-bucket (lambda (x &aux (distance (distance container x item)))
                          (mini distance)
                          (maxi distance))
                        parent
                        stack)))


(-> collect-buckets (egnat-node) vector)
(defun collect-buckets (parent)
  (lret ((result (vect)))
    (visit-every-bucket (rcurry #'vector-push-extend result)
                        parent)))


(defun node-head (node)
  (let ((content (read-content node)))
    (if (typep node 'egnat-node)
        (aref content 0)
        content)))


(-> update-ranges! (mutable-egnat-container egnat-node t fixnum)
    mutable-egnat-container)
(defun update-ranges! (container node item closest-index)
  (let ((children (read-children node))
        (close-range (read-close-range node))
        (distant-range (read-distant-range node)))
    (iterate
      (for i from 0 below (length children))
      (for child = (aref children i))
      (unless (eql i closest-index)
        (let ((distance (~> child
                            node-head
                            (distance container _ item))))
          (symbol-macrolet ((close (aref close-range i closest-index))
                            (distant (aref distant-range i closest-index)))
            (setf close (if (zerop close)
                            distance
                            (min close distance))
                  distant (max distant distance))))))
    container))


(defun make-content-vector (container &rest initial-content)
  (~> (make-array (read-content-count-in-node container)
                  :element-type t
                  :adjustable t
                  :fill-pointer 0)
      (map-into #'identity initial-content)))


(defun make-children-vector (container &rest initial-content)
(~> (make-array (read-branching-factor container)
                :element-type t
                :adjustable t
                :fill-pointer 0)
    (map-into #'identity initial-content)))


(defun assign-children (container seeds data &optional (start 0) parallel)
  (declare (type vector data)
           (type (simple-array fixnum (*)) seeds)
           (type fixnum start))
  (assert (array-has-fill-pointer-p data))
  (let ((result (map 'vector
                     (lambda (i)
                       (make-content-vector container (aref data i)))
                     seeds)))
    (iterate
      (declare (type fixnum length i))
      (with length = (length data))
      (for i from start below length)
      (when (find i seeds :test 'eql)
        (next-iteration))
      (for point = (aref data i))
      (for target = (if parallel
                        (lparallel:preduce
                         (lambda (&rest arg)
                           (declare (dynamic-extent arg))
                           (extremum arg #'<
                                     :key (lambda (x)
                                            (distance container
                                                      (aref x 0)
                                                      point))))
                         result)
                        (extremum result #'< :key
                                  (lambda (x)
                                    (distance container
                                              (aref x 0)
                                              point)))))
      (vector-push-extend point target))
    (assert (= (- (length data) start)
               (reduce #'+ result :key #'length)))
    result))


(defgeneric recursive-egnat-grow!* (node container operation
                                    item additional-arguments))


(defun recursive-egnat-grow! (node container operation item
                              additional-arguments)
  (recursive-egnat-grow!* node
                          container
                          operation
                          item
                          additional-arguments))


(defmethod recursive-egnat-grow!* ((node (eql nil))
                                   container operation
                                   item additional-arguments)
  (make 'egnat-node
        :content (vector (apply #'cl-ds.meta:make-bucket
                                operation
                                container
                                item
                                additional-arguments))))


(defmethod recursive-egnat-grow!* ((node egnat-subtree)
                                   container operation
                                   item additional-arguments)
  (declare (type list additional-arguments))
  (let* ((children (read-children node))
         (children-count (fill-pointer children))
         (distant-range (read-distant-range node))
         (close-range (read-close-range node)))
    (if (node-children-full-p container node)
        (let ((distances (make-array children-count)))
          (iterate
            (declare (type fixnum i))
            (for i from 0 below children-count)
            (for subnode = (aref children i))
            (for head = (node-head subnode))
            (for distance = (distance container head item))
            (setf (aref distances i) distance))
          (bind ((target-node (iterate
                                (for d in-vector distances)
                                (for i from 0)
                                (finding i minimizing d))))
            (recursive-egnat-grow!* (aref children target-node)
                                    container operation
                                    item additional-arguments)
            (iterate
              (declare (type fixnum i))
              (for i from 0 below children-count)
              (for distance = (aref distances i))
              (maxf (aref distant-range i target-node) distance)
              (minf (aref close-range i target-node) distance))))
        (iterate
          (declare (type fixnum i))
          (for i from 0 below children-count)
          (for subnode = (aref children i))
          (for head = (node-head subnode))
          (for distance = (distance container head item))
          (setf (aref distant-range i children-count) distance
                (aref close-range i children-count) distance)
          (for (values close distant) =
               (cl-ds.utils:optimize-value ((mini <) (maxi >))
                 (visit-every-bucket (lambda (x)
                                       (let ((distance (distance container x item)))
                                         (mini distance)
                                         (maxi distance)))
                                     subnode)))
          (setf (aref distant-range children-count i) distant
                (aref close-range children-count i) close)
          (finally
           (vector-push-extend (make 'egnat-node :content
                                     (vect (apply #'cl-ds.meta:make-bucket
                                                  operation
                                                  container
                                                  item
                                                  additional-arguments)))
                               (read-children node))))))
  node)


(defun push-content (node container operation item additional-arguments)
  (vector-push-extend (apply #'cl-ds.meta:make-bucket operation container
                             item additional-arguments)
                      (read-content node))
  t)


(defmethod recursive-egnat-grow!* ((node egnat-node)
                                   container operation
                                   item additional-arguments)
  (declare (type list additional-arguments))
  (if (node-full-p container node)
      (recursive-egnat-grow!* (become-subtree container node)
                              container operation
                              item additional-arguments)
      (push-content node container operation item
                    additional-arguments)))


(-> egnat-grow! (mutable-egnat-container t cl-ds.meta:grow-function t list)
    (values mutable-egnat-container
            cl-ds.common:eager-modification-operation-status))
(defun egnat-grow! (structure container operation item additional-arguments)
  (recursive-egnat-grow!* (access-root container)
                          container operation item
                          additional-arguments)
  (incf (access-size structure))
  (values structure (cl-ds.common:make-eager-modification-operation-status
                     nil item t)))


(defun traverse-impl (container function)
  (visit-every-bucket function (access-root container))
  container)
