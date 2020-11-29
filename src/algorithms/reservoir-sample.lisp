(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    reservoir-sample reservoir-sample-function

    (:range sample-size &key key)
    (:range sample-size &key (key #'identity))

    ((%result (cl-ds.utils:extendable-vector t))
     (%i integer)
     (%sample-size positive-fixnum))

    ((check-type sample-size positive-fixnum)
     (setf %result (make-array sample-size
                               :fill-pointer 0)
           %i 0
           %sample-size sample-size))

    ((element)
     (if (< (fill-pointer %result) %sample-size)
         (vector-push element %result)
         (let ((random (random (1+ %i))))
           (when (< random %sample-size)
             (setf (aref %result random) element))))
     (incf %i))

    (%result))
