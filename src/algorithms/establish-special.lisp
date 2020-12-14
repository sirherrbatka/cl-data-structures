(cl:in-package #:cl-data-structures.algorithms)


(defclass establish-special-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric establish-special (range bindings &key key)
  (:generic-function-class establish-special-function)
  (:method (range bindings &key (key #'identity))
    (apply-range-function range #'establish-special
                          (list range bindings :key key))))


(defclass establish-special-proxy ()
  ((%key :initarg :key
         :reader read-key)
   (%bindings :initarg :bindings
              :reader read-bindings)))


(defmethod cl-ds.utils:cloning-information append
    ((range establish-special-proxy))
  '((:key read-key)
    (:bindings read-bindings)))


(defclass forward-establish-special-proxy-range
    (establish-special-proxy forward-proxy-range)
  ())


(defclass bidirectional-establish-special-proxy-range
    (establish-special-proxy bidirectional-proxy-range)
  ())


(defclass random-access-establish-special-proxy-range
    (establish-special-proxy bidirectional-proxy-range)
  ())


(defmethod cl-ds:traverse ((range forward-establish-special-proxy-range)
                           function)
  (let* ((function (ensure-function function))
         (bindings (read-bindings range))
         (variables (mapcar #'first bindings))
         (functions (mapcar (compose #'ensure-function #'second) bindings))
         (key (ensure-function (read-key range))))
    (call-next-method range
                      (lambda (element &aux (value (funcall key element)))
                        (progv
                            variables
                            (mapcar (lambda (function)
                                      (assert (functionp function))
                                      (funcall function value))
                                    functions)
                          (funcall function element))))))


(defmethod cl-ds:across ((range forward-establish-special-proxy-range)
                         function)
  (let* ((function (ensure-function function))
         (bindings (read-bindings range))
         (variables (mapcar #'first bindings))
         (functions (mapcar (compose #'ensure-function #'second) bindings))
         (key (ensure-function (read-key range))))
    (call-next-method range
                      (lambda (element &aux (value (funcall key element)))
                        (progv
                            variables
                            (mapcar (lambda (function)
                                      (assert (functionp function))
                                      (funcall function value))
                                    functions)
                          (funcall function element))))))


(defmethod apply-layer ((range cl-ds:traversable)
                        (fn establish-special-function)
                        all)
  (bind (((bindings . rest) (rest all))
         (key (getf rest :key)))
    (make-proxy range 'forward-establish-special-proxy-range
                :key key
                :bindings bindings)))


(defmethod apply-layer ((range cl-ds:fundamental-bidirectional-range)
                        (fn establish-special-function)
                        all)
  (bind (((bindings . rest) (rest all))
         (key (getf rest :key)))
    (make-proxy range 'bidirectional-establish-special-proxy-range
                :key key
                :bindings bindings)))


(defmethod apply-layer ((range cl-ds:fundamental-random-access-range)
                        (fn establish-special-function)
                        all)
  (bind (((bindings . rest) (rest all))
         (key (getf rest :key)))
    (make-proxy range 'random-access-establish-special-proxy-range
                :key key
                :bindings bindings)))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range establish-special-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (declare (optimize (speed 3) (safety 0)))
  (let* ((special-key (ensure-function (read-key range)))
         (bindings (read-bindings range))
         (variables (mapcar #'first bindings))
         (functions (mapcar (compose #'ensure-function #'second) bindings))
         (outer-fn (call-next-method)))
    (assert (functionp outer-fn))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.utils:cases ((:variant (eq special-key #'identity)
                                   (eq special-key 'identity)))
       (cl-ds.alg.meta:let-aggregator
           ((inner (cl-ds.alg.meta:call-constructor outer-fn)))

           ((element)
            (let ((var (funcall special-key element)))
              (progv
                  variables
                  (mapcar (lambda (function)
                            (assert (functionp function))
                            (funcall function var))
                          functions)
                (cl-ds.alg.meta:pass-to-aggregation inner element))))

           ((cl-ds.alg.meta:extract-result inner))

         (cl-ds.alg.meta:cleanup inner)))
     function
     arguments)))
