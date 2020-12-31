(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    to-list to-list-function

  (:range &key key after)
  (:range &key (key #'identity) (after #'identity))

  (%list)

  ((setf %list '()))

  ((element)
   (push element %list))

  ((nreverse %list)))
