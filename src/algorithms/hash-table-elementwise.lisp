(cl:in-package #:cl-data-structures.algorithms)


(defclass hash-table-elementwise-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric hash-table-elementwise (range)
  (:generic-function-class hash-table-elementwise-function)
  (:method (range)
    (apply-range-function range #'hash-table-elementwise (list range))))


(defclass hash-table-elementwise-forward-proxy (forward-proxy-range)
  ())


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:traversable)
                                       (function hash-table-elementwise-function)
                                       arguments)
  (make 'hash-table-elementwise-forward-proxy :original-range range))


(defmethod cl-ds.alg.meta:layer-aggregator-constructor ((function hash-table-elementwise-function)
                                                        outer-constructor
                                                        arguments)
  (cl-ds.alg.meta:let-aggregator
      ((inners nil))

      ((element)
        (check-type element hash-table)
        (when (null inners)
          (setf inners (make-hash-table :test (hash-table-test element)
                                        :size (hash-table-size element))))
        (maphash (lambda (key value)
                   (let ((aggregator (ensure (gethash key inners)
                                       (cl-ds.alg.meta:call-constructor outer-constructor))))
                     (cl-ds.alg.meta:pass-to-aggregation aggregator
                                                         value)))
                 element))
      ((unless (null inners)
         (lret ((result (make-hash-table :test (hash-table-test inners)
                                         :size (hash-table-size inners))))
           (maphash (lambda (key value)
                      (setf (gethash key result) (cl-ds.alg.meta:extract-result value)))
                    inners))))

       (unless (null inners)
         (maphash-values #'cl-ds.alg.meta:cleanup inners))))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range hash-table-elementwise-forward-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (let ((outer-fn (call-next-method)))
    (assert (functionp outer-fn))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.alg.meta:layer-aggregator-constructor #'hash-table-elementwise
                                                  outer-fn
                                                  arguments)
     function
     arguments)))
