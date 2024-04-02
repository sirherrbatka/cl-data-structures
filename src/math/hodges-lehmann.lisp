(cl:in-package #:cl-data-structures.math)


(defun calculate-hodges-lehmann-estimator (parallel vector &aux (length (length vector)))
  (declare (type (cl-ds.utils:extendable-vector t) vector))
  (cond ((zerop length) 0.0)
        ((= length 1) (first-elt vector))
        (t
         (bind ((length (length vector))
                ((:dflet index (i j))
                 (+ (- (* length i)
                       (/ (* i (1+ i)) 2)
                       1 i)
                    j))
                (median-length (1+ (index (1- length) (1- length))))
                (middle (truncate median-length 2))
                (median-buffer (make-array median-length :element-type 'double-float))
                (indexes (iterate
                           (with result = (make-array length))
                           (for i from 0 below length)
                           (setf (aref result i) i)
                           (finally (return result))))
                ((:dflet average-of-pairs (i))
                 (iterate
                   (for j from (1+ i) below length)
                   (for index = (index i j))
                   (setf (aref median-buffer index)
                         (coerce (/ (+ (aref vector i)
                                       (aref vector j))
                                    2)
                                 'double-float)))))
           (funcall (if parallel #'lparallel:pmap #'map)
                    nil #'average-of-pairs indexes)
           (setf median-buffer (funcall (if parallel #'lparallel:psort #'sort)
                                        median-buffer #'<))
           (if (oddp median-length)
               (aref median-buffer middle)
               (/ (+ (aref median-buffer middle)
                     (aref median-buffer (1- middle)))
                  2))))))


(cl-ds.alg.meta:define-aggregation-function
    hodges-lehmann-estimator hodges-lehmann-estimator-function

    (:range &key key parallel after)
    (:range &key (key #'identity) (parallel nil) (after #'identity))

    ((%data (cl-ds.utils:extendable-vector t)) (%parallel boolean))

    ((setf %data (vect)
           %parallel parallel))

    ((element)
     (vector-push-extend element %data))

    ((calculate-hodges-lehmann-estimator %parallel %data)))
