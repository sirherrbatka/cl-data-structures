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

(defmethod cl-ds.utils:cloning-information append ((object aggregations-container))
  '((:aggregators aggregators)
    (:input input)))

(defgeneric make-aggregations-container (input aggregator)
  (:method ((input aggregations-container) aggregator)
    (cl-ds.utils:quasi-clone input :aggregators (cons aggregator (aggregators input))))
  (:method (input aggregator)
    (make-instance 'aggregations-container
                   :input input
                   :aggregators (list aggregator))))

(defun run (aggregations-container)
  (let ((success nil)
        (aggregators (~> aggregations-container aggregators reverse))
        (range (input aggregations-container)))
    (unwind-protect
         (progn
           (handler-case
               (across-aggregate range
                                 (lambda (x)
                                   (iterate
                                     (for aggregator in aggregators)
                                     (pass-to-aggregation aggregator
                                                          x))))
             (early-aggregation-exit nil))
           (setf success t))
      (unless success (iterate
                        (for aggregator in aggregators)
                        (cleanup aggregator))))
    (if (endp (rest aggregators))
        (~> aggregators first extract-result)
        (mapcar #'extract-result aggregators))))

(defmethod apply-aggregation-function (range
                                       (function aggregation-function)
                                       all
                                       &optional run)
  (let* ((aggregator (construct-aggregator range function all))
         (container (make-aggregations-container range aggregator)))
    (if run
        (run container)
        container)))
