(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    to-vector to-vector-function

  (:range &key key element-type size after)
  (:range &key
   (key #'identity) (element-type t)
   (after #'identity) (size 16))

  (%vector)

  ((setf %vector (make-array size
                             :adjustable t
                             :fill-pointer 0
                             :element-type element-type)))
  ((element)
   (vector-push-extend element %vector))

  (%vector))
