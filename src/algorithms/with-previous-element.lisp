(cl:in-package #:cl-data-structures.algorithms)


(defvar *previous-element*)


(defclass with-previous-element-proxy (proxy-range)
  ((%key :initarg :key
         :reader read-key
         :initform #'identity)))


(defmethod cl-ds.utils:cloning-information append ((proxy with-previous-element-proxy))
  '((:key read-key)))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range with-previous-element-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (let ((outer-fn (call-next-method))
        (key (read-key range)))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.alg.meta:let-aggregator
         ((previous-bound nil)
          (previous-element nil)
          (next (cl-ds.alg.meta:call-constructor outer-fn)))

         ((element)
           (if previous-bound
               (let ((*previous-element* previous-element))
                 (setf previous-element (funcall key element) previous-bound t)
                 (cl-ds.alg.meta:pass-to-aggregation next element))
               (progn
                 (setf previous-element (funcall key element) previous-bound t)
                 (cl-ds.alg.meta:pass-to-aggregation next element))))

         ((cl-ds.alg.meta:extract-result next))

       (cl-ds.alg.meta:cleanup next))
     function
     arguments)))


(defclass forward-with-previous-element-proxy (with-previous-element-proxy fundamental-forward-range)
  ())


(defclass bidirectional-with-previous-element-proxy (with-previous-element-proxy bidirectional-proxy-range)
  ())


(defclass random-access-with-previous-element-proxy (with-previous-element-proxy random-access-proxy-range)
  ())


(defclass with-previous-element-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric with-previous-element (range &key key)
  (:generic-function-class with-previous-element-function)
  (:method (range &key (key #'identity))
    (apply-range-function range #'with-previous-element (list key))))


(defmethod apply-layer ((range cl-ds:traversable)
                        (fn with-previous-element-function)
                        all)
  (make-proxy range 'forward-with-previous-element-proxy :key (first all)))


(defmethod apply-layer ((range fundamental-forward-range)
                        (fn with-previous-element-function)
                        all)
  (make-proxy range 'forward-with-previous-element-proxy :key (first all)))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (fn with-previous-element-function)
                        all)
  (make-proxy range 'bidirectional-with-previous-element-proxy :key (first all)))


(defmethod apply-layer ((range fundamental-random-access-range)
                        (fn with-previous-element-function)
                        all)
  (make-proxy range 'random-access-with-previous-element-proxy :key (first all)))
