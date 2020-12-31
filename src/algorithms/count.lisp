(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    count-elements
    count-elements-function

    (:range &key after)
    (:range &key (after #'identity))

    ((%count integer))

    ((setf %count 0))

    ((element)
     (declare (ignore element))
     (incf %count))

    (%count))
