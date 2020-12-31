(cl:in-package #:cl-data-structures.utils.clustering.bubble)


(defun insert-into-tree (tree root element)
  (declare (optimize (speed 3)))
  (let ((stack (vect (cons root nil))))
    (assert (array-has-fill-pointer-p stack))
    (iterate
      (for node = (car (aref stack (1- (length stack)))))
      (for next = (cf-insert tree node element))
      (while (consp next))
      (vector-push-extend next stack))
    (iterate
      (for i from (1- (length stack)) downto 0)
      (for p-i previous i)
      (for cell = (aref stack i))
      (for node = (car cell))
      (for position = (if (null p-i)
                          nil
                          (~> stack (aref p-i) cdr)))
      (unless (or (null p-i)
                  (~> stack (aref p-i) car
                      (typep 'fundamental-cf-node)))
        (assert (~> stack (aref p-i) car vectorp))
        (assert position)
        (~> stack (aref p-i) car
            (absorb-nodes tree node _ position)))
      (for at-root = (zerop i))
      (if (needs-split-p tree node)
          (let ((splitted (split tree node)))
            (assert (vectorp splitted))
            (if at-root
                (let ((new-root (make-subtree tree)))
                  (absorb-nodes tree new-root splitted)
                  (leave new-root))
                (setf (car (aref stack i)) splitted)))
          (leave root)))))


(defun single-thread-bubble-grouping (tree data)
  (declare (optimize (speed 3))
           (vector data))
  (iterate
    (declare (type fixnum i))
    (with length = (length data))
    (with root = (make-leaf tree))
    (for i from 0 below length)
    (for d = (aref data i))
    (setf root (insert-into-tree tree root d))
    (finally (return root))))


(defun gather-leafs (tree root &key (key #'identity))
  (lret ((result (vect)))
    (visit-leafs tree root
                 (rcurry #'vector-push-extend result)
                 :key key)))


(defun draw-sample (tree data)
  (declare (optimize (speed 3))
           (type vector data))
  (let* ((length (length data))
         (parallel-reference-size (min length
                                       (read-parallel-reference-size tree)))
         (reference-data (make-array parallel-reference-size))
         (sample-size (min length
                           (read-parallel-sample-size tree)))
         (distance-function (read-distance-function tree))
         (sample (make-array sample-size))
         (distances (make-array sample-size)))
    (declare (type fixnum sample-size length)
             (type simple-vector sample distances)
             (type function distance-function))
    (map-into sample (cl-ds.utils:lazy-shuffle 0 length))
    (map-into reference-data (cl-ds.utils:lazy-shuffle 0 length))
    (cl-ds.utils:transform #1=(lambda (i) (aref data i)) sample)
    (cl-ds.utils:transform #1# reference-data)
    (iterate outer
      (declare (type fixnum i))
      (for i from 0 below parallel-reference-size)
      (for elt = (aref reference-data i))
      (map-into distances
                (lambda (sample)
                  (funcall distance-function
                           sample
                           elt))
                sample)
      (iterate
        (declare (type fixnum i))
        (for i from 0 below sample-size)
        (iterate
          (declare (type fixnum j))
          (for j from 0 below i)
          (in outer (sum (abs (- (aref distances i)
                                 (aref distances j)))
                         into total))))
      (finally (return-from outer (cons total sample))))))


(defun select-parallel-samples (tree data)
  (~> tree
      read-parallel-samples-count
      make-array
      (lparallel:pmap-into (lambda () (draw-sample tree data)))
      (extremum #'> :key #'car)
      cdr))


(defun select-parallel-global-partitions (tree data samples)
  (declare (optimize (speed 3))
           (type simple-vector samples))
  (let* ((distance-function (read-distance-function tree))
         (samples-count (length samples))
         (result (map-into (make-array samples-count) #'vect))
         (locks (map-into (make-array samples-count) #'bt:make-lock)))
    (declare (type fixnum samples-count)
             (type simple-vector result locks)
             (type function distance-function))
    (lparallel:pmap nil
                    (lambda (x)
                      (iterate
                        (declare (type fixnum i))
                        (for i from 0 below samples-count)
                        (for sample = (aref samples i))
                        (for distance = (funcall distance-function sample x))
                        (finding i minimizing distance into destination)
                        (finally (bt:with-lock-held ((aref locks destination))
                                   (vector-push-extend x (aref result destination))))))
                    data)
    result))


(defun parallel-bubble-grouping (tree data)
  (lret ((result (make-subtree tree)))
    (~>> (select-parallel-samples tree data)
         (select-parallel-global-partitions tree data)
         (lparallel:pmap 'vector (curry #'single-thread-bubble-grouping
                                        tree))
         (absorb-nodes tree result))))
