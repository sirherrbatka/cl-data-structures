(cl:in-package #:cl-data-structures.algorithms)


(defclass only-different-proxy (filtering-proxy)
  ((%comparsion :initarg :comparsion
                :reader read-comparsion)
   (%previous :initarg :previous
              :accessor access-previous)))


(defmethod cl-ds.utils:cloning-information append
    ((proxy only-different-proxy))
  '((:comparsion read-comparsion)
    (:previous access-previous)))


(defclass forward-only-different-proxy
    (only-different-proxy
     forward-filtering-proxy)
  ())


(defmethod should-skip ((range only-different-proxy)
                        element
                        can-mutate)
  (prog1 (funcall (read-comparsion range)
                  element
                  #1=(access-previous range))
    (when can-mutate
      (setf #1# element))))


(defclass only-different-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defmethod wrap-chunk ((range forward-only-different-proxy)
                       (chunk cl-ds:fundamental-forward-range))
  (make 'forward-only-different-proxy
        :original-range chunk
        :key (read-key range)
        :comparsion (read-comparsion range)))


(defgeneric only-different-function (range &key
                                           test
                                           initial-value
                                           key)
  (:generic-function-class only-different-function)
  (:method (range &key (test 'eql)
                       initial-value
                       (key #'identity))
    (apply-range-function range
                          #'only-different-function
                          (list range
                                :test test
                                :initial-value initial-value
                                :key key))))


(defmethod apply-layer ((range cl-ds:traversable)
                        (function only-different-function)
                        all)
  (make 'forward-only-different-proxy
        :comparsion (cl-ds.utils:at-list (cdr all) :test)
        :previous (cl-ds.utils:at-list (cdr all) :initial-value)
        :key (cl-ds.utils:at-list (cdr all) :key)
        :original-range range))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range only-different-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (declare (optimize (speed 3) (safety 0)))
  (let ((outer-fn (or outer-constructor
                      (cl-ds.alg.meta:aggregator-constructor
                       '() nil function arguments)))
        (comparsion (ensure-function (read-comparsion range)))
        (key (ensure-function (read-key range))))
    (cl-ds.utils:cases ((:variant (eq key #'identity)))
      (cl-ds.alg.meta:aggregator-constructor
       (read-original-range range)
       (cl-ds.alg.meta:let-aggregator ((inner (cl-ds.alg.meta:call-constructor outer-fn))
                                       (previous (access-previous range)))
           ((element)
             (let ((key (funcall key element)))
               (unless (funcall comparsion previous key)
                 (cl-ds.alg.meta:pass-to-aggregation inner element)
                 (setf previous key))))

           ((cl-ds.alg.meta:extract-result inner))

         (cl-ds.alg.meta:cleanup inner))
       function
       arguments))))

