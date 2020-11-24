(cl:in-package #:cl-data-structures.algorithms)


(defclass without-proxy (filtering-proxy)
  ((%predicate :initarg :predicate
               :reader read-predicate)))


(defmethod cl-ds.utils:cloning-information append
    ((proxy without-proxy))
  '((:predicate read-predicate)))


(defclass forward-without-proxy (without-proxy
                                 forward-filtering-proxy)
  ())


(defclass bidirectional-without-proxy (forward-without-proxy
                                       bidirectional-filtering-proxy)
  ())


(defmethod should-skip ((range without-proxy) element can-mutate)
  (declare (ignore can-mutate))
  (funcall (read-predicate range) element))


(defclass without-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric without (range predicate &key key)
  (:generic-function-class without-function)
  (:method (range predicate &key (key #'identity))
    (apply-range-function range #'without
                          (list range predicate
                                :key key))))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (function without-function)
                        all)
  (make 'bidirectional-without-proxy
        :predicate (second all)
        :key (getf (cddr all) :key)
        :original-range range))


(defmethod apply-layer ((range cl-ds:traversable)
                        (function without-function)
                        all)
  (make 'forward-without-proxy
        :predicate (second all)
        :key (getf (cddr all) :key)
        :original-range range))


(defmethod wrap-chunk ((range without-proxy)
                       (chunk cl-ds:fundamental-forward-range))
  (without chunk (read-predicate range) :key (read-key range)))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range without-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (declare (optimize (speed 3) (safety 0)))
  (let ((outer-fn (or outer-constructor
                      (cl-ds.alg.meta:aggregator-constructor
                       '() nil function arguments)))
        (predicate (ensure-function (read-predicate range)))
        (key (ensure-function (read-key range))))
    (cl-ds.utils:cases ((:variant (eq key #'identity)))
      (cl-ds.alg.meta:aggregator-constructor
       (read-original-range range)
       (cl-ds.alg.meta:let-aggregator ((inner (cl-ds.alg.meta:call-constructor outer-fn)))
           ((element)
             (unless (funcall predicate (funcall key element))
               (cl-ds.alg.meta:pass-to-aggregation inner element)))

           ((cl-ds.alg.meta:extract-result inner))

         (cl-ds.alg.meta:cleanup inner))
       function
       arguments))))
