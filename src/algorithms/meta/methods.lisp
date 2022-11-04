(cl:in-package #:cl-data-structures.algorithms.meta)

#|
Range function invokaction protocol.
|#

(defmethod apply-range-function ((object cl-ds:fundamental-container)
                                 (function aggregation-function)
                                 all)
  (apply-aggregation-function object function all))


(defmethod apply-range-function ((object cl:sequence)
                                 (function aggregation-function)
                                 all)
  (apply-aggregation-function object function all))


(defmethod apply-range-function (range
                                 (function aggregation-function)
                                 all)
  (apply-aggregation-function range function all))


(defmethod apply-range-function ((range cl-ds:traversable)
                                 (function layer-function)
                                 all)
  (warn "Appling range function to ~a object which is not a range."
        range)
  (apply-layer range function all))


(defmethod apply-range-function ((range cl-ds:fundamental-range)
                                 (function layer-function)
                                 all)
  (apply-layer (cl-ds:clone range) function all))


(defmethod apply-range-function ((range cl:hash-table)
                                 (function layer-function)
                                 all)
  (apply-layer (cl-ds:whole-range range) function all))


(defmethod apply-range-function ((range cl:sequence)
                                 (function layer-function)
                                 all)
  (apply-layer (cl-ds:whole-range range) function all))


(defmethod apply-range-function ((range cl-ds:fundamental-container)
                                 (function layer-function)
                                 all)
  (apply-layer (cl-ds:whole-range range) function all))


(defmethod apply-aggregation-function (range
                                       (function aggregation-function)
                                       all)
  (let ((aggregator (construct-aggregator range function all))
        (success nil))
    (unwind-protect
         (progn
           (handler-case
               (across-aggregate range
                                 (lambda (x)
                                   (pass-to-aggregation aggregator
                                                        x)))
             (early-aggregation-exit nil))
           (setf success t))
      (unless success (cleanup aggregator)))
    (extract-result aggregator)))
