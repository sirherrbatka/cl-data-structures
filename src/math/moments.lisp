(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function
    moments moments-function

  (:range from count about &key key after)

  (:range from count about &key (key #'identity) (after #'identity))

  (%moments %start %count %lambdas)

  ((setf %lambdas (make-array count)
         %moments (make-array count :initial-element 0.0
                                    :element-type 'single-float)
         %count 0
         %start from)
   (iterate
     (for i from from)
     (for index from 0 below count)
     (setf (aref %lambdas index) (let ((power i))
                                   (lambda (value)
                                     (expt (- value about) power))))))

  ((element)
   (incf %count)
   (iterate
     (for i from 0 below (length %lambdas))
     (incf (aref %moments i) (funcall (aref %lambdas i) element))))

  ((map-into %moments (rcurry #'/ %count) %moments)
   (make-instance 'cl-ds.adapters:offset-vector-range
                  :vector %moments
                  :offset %start
                  :upper-bound (length %moments))))
