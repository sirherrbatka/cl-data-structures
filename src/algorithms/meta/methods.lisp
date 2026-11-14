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

(defgeneric make-aggregations-container (input args)
  (:method ((input aggregations-container) args)
    (cl-ds.utils:quasi-clone input :aggregators (cons args (aggregators input))))
  (:method (input args)
    (make-instance 'aggregations-container
                   :input input
                   :aggregators (list args))))

(defun aggregations-container-aggregator (aggregations-container)
  (call-constructor (aggregator-constructor
                     (input aggregations-container)
                     (let-aggregator ((aggregators (~>> aggregations-container aggregators
                                                        (mapcar (lambda (x)
                                                                  (construct-aggregator nil (first x) (second x)))))))
                         ((element)
                           (iterate
                             (for aggregator in aggregators)
                             (pass-to-aggregation aggregator
                                                  element)))
                         ((map nil #'extract-result aggregators))
                       (iterate
                         (for aggregator in aggregators)
                         (cleanup aggregator)))
                     nil
                     nil)))

(defun run (aggregations-container)
  (captured-aggregations-list
   (with-captured-aggregations
     (let* ((success nil)
            (compound-aggregator (aggregations-container-aggregator aggregations-container)))
       (unwind-protect
            (progn
              (handler-case
                  (across-aggregate (input aggregations-container)
                                    (aggregator-pass compound-aggregator))
                (early-aggregation-exit nil))
              (setf success t))
         (unless success (cleanup compound-aggregator)))
       (extract-result compound-aggregator)))))

(defmethod apply-aggregation-function (range
                                       (function aggregation-function)
                                       all
                                       &optional run)
  (let* ((container (make-aggregations-container range (list function all))))
    (if run
        (run container)
        container)))

(defmethod apply-aggregation-function ((range aggregations-container) function all &optional run)
  (push (list function all) (aggregators range))
  (if run (run range) range))
