(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function
    standard-deviation
    standard-deviation-function

    (:range around &key key after)
    (:range around &key (key #'identity) (after #'identity))

    (%count %sum %average)

    ((setf %count 0
           %average around
           %sum 0))

    ((element)
     (incf %count)
     (incf %sum (expt (- element %average) 2)))

    ((sqrt (/ %sum %count))))
