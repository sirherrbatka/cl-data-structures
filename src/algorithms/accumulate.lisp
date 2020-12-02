(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    accumulate accumulate-function

  (:range fn &key key initial-value after)
  (:range fn &key (key #'identity) (initial-value :unbound) (after #'identity))

  (%value %fn %first-iteration %initial-value-present)

  ((setf %value initial-value
         %fn fn
         %first-iteration t
         %initial-value-present (not (eq :unbound initial-value))))

  ((element)
   (if %first-iteration
       (setf %value (if %initial-value-present
                        (funcall %fn %value element)
                        element)
             %first-iteration nil)
       (setf %value (funcall %fn %value element))))

  (%value))
