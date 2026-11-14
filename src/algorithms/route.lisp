(cl:in-package #:cl-data-structures.algorithms)


(defclass route-proxy (proxy-range)
  ((%content :initarg :content
             :reader content)))


(defmethod cl-ds.utils:cloning-information append
    ((range route-proxy))
  '((:content content)))


(defclass forward-route-proxy (route-proxy
                               fundamental-forward-range)
  ())


(defclass bidirectional-route-proxy (forward-route-proxy
                                     bidirectional-proxy-range)
  ())


(defclass random-access-route-proxy (bidirectional-route-proxy
                                     random-access-proxy-range)
  ())


(defclass route-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric %route (range &key content)
  (:generic-function-class route-function)
  (:method (range &key content)
    (apply-range-function range #'%route
                          (list range :content content))))


(defmethod apply-layer ((range cl-ds:traversable)
                        (fn route-function)
                        all)
  (make-proxy range 'forward-route-proxy
              :content (getf (rest all) :content)))


(defmethod apply-layer ((range fundamental-forward-range)
                        (fn route-function)
                        all)
  (make-proxy range 'forward-route-proxy
              :content (getf (rest all) :content)))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (fn route-function)
                        all)
  (make-proxy range 'bidirectional-route-proxy
              :content (getf (rest all) :content)))


(defmethod apply-layer ((range fundamental-random-access-range)
                        (fn route-function)
                        all)
  (make-proxy range 'random-access-route-proxy
              :content (getf (rest all) :content)))


(defmacro route (input form &rest forms)
  `(apply-range-function ,input #'%route (let ((cl-ds.alg.meta:*run* nil))
                                           (list ,input :content (~> nil ,form ,@forms)))))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range route-proxy)
                                                  outer-constructor
                                                  function
                                                  (arguments list))
  (bind ((outer-fn (call-next-method))
         (content (content range)))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.alg.meta:let-aggregator
         ((next (call-constructor outer-fn))
          (inner (cl-ds.alg.meta:aggregations-container-aggregator content)))

         ((element)
           (cl-ds.alg.meta:pass-to-aggregation next element)
           (cl-ds.alg.meta:pass-to-aggregation inner element))

         ((cl-ds.alg.meta:extract-result next)
          (cl-ds.alg.meta:extract-result inner))

       (progn
         (cl-ds.alg.meta:cleanup next)
         (cl-ds.alg.meta:cleanup inner)))
     function
     arguments)))

