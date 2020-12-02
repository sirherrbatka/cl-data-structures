(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function
    entropy entropy-function

    (:range &key key test count-fn size after)

    (:range &key
     (key #'identity)
     (test 'eql)
     (size 16)
     (after #'identity)
     (count-fn (constantly 1)))

    ((%table hash-table)
     (%total-count integer)
     (%count-fn function))

    ((ensure-functionf count-fn)
     (setf %table (make-hash-table :test test :size size)
           %count-fn (ensure-function count-fn)
           %total-count 0))

    ((element)
     (let ((count (the integer (funcall %count-fn element))))
       (incf %total-count count)
       (incf (gethash element %table 0) count)))

    ((- (iterate
          (for (class count) in-hashtable %table)
          (for prob = (/ count %total-count))
          (assert (<= prob 1.0))
          (sum (* prob (log prob)))))))
