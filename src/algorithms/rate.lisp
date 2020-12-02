(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    rate rate-function
    (:range test &key key positive total after)
    (:range test &key (key #'identity) (positive 0) (total 0) (after #'identity))

    (%positive %total %test)

    ((ensure-functionf test)
     (setf %positive positive
           %test test
           %total total))

    ((element)
     (incf %total)
     (when (funcall %test element)
       (incf %positive)))

    ((/ %positive %total)))
