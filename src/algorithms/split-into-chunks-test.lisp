(cl:in-package #:cl-user)
(defpackage split-into-chunks-tests
  (:use :cl :prove :cl-data-structures.aux-package))

(cl:in-package #:split-into-chunks-tests)

(plan 4)

(let* ((vector #(0 1 2 3 4 5 6 7 8 9 10 11))
       (result (cl-ds.alg:to-vector (cl-ds.alg:in-batches vector 3))))
  (is (cl-ds:at result 0) #(0 1 2) :test #'vector=)
  (is (cl-ds:at result 1) #(3 4 5) :test #'vector=)
  (is (cl-ds:at result 2) #(6 7 8) :test #'vector=)
  (is (cl-ds:at result 3) #(9 10 11) :test #'vector=))

(finalize)
