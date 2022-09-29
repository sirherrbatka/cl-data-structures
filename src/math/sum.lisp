(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function sum sum-function
  (:range &key key sum after)
  (:range &key (key #'identity) (sum 0) (after #'identity))
  ((%sum number))
  ((setf %sum sum))
  ((element)
   (check-type element number)
   (incf %sum element))
  (%sum))


(cl-ds.alg.meta:define-aggregation-function array-sum array-sum-function
  (:range &key key sum after)
  (:range &key (key #'identity) (sum nil) (after #'identity))
  ((%sum (or null array)))
  ((if (null sum)
       (setf %sum nil)
       (setf %sum (copy-array sum))))
  ((element)
   (check-type element array)
   (if (null %sum)
       (setf %sum (copy-array element))
       (iterate
         (for i from 0 below (array-total-size %sum))
         (incf (row-major-aref %sum i) (row-major-aref element i)))))
  (%sum))
