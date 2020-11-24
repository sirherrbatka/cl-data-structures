(cl:in-package #:cl-data-structures.algorithms)


(defclass flatten-proxy (cl-ds:chunking-mixin
                         proxy-range)
  ((%key :initarg :key
         :reader read-key)
   (%current :initform nil
             :initarg :current
             :accessor access-current)))


(defmethod cl-ds.utils:cloning-information append
    ((range flatten-proxy))
  '((:key read-key)
    (:current access-current)))


(defclass forward-flatten-proxy (flatten-proxy
                                 fundamental-forward-range)
  ())


(labels ((impl (function x)
           (labels ((inner (x)
                      (if (listp x)
                          (map nil #'inner x)
                          (funcall function x))))
             (inner x))))
  (defmethod cl-ds:traverse ((range flatten-proxy) function)
    (ensure-functionf function)
    (cl-ds:traverse (read-original-range range)
                    (compose (curry #'impl function) (read-key range)))
    range)


  (defmethod cl-ds:across ((range flatten-proxy) function)
    (ensure-functionf function)
    (cl-ds:across (read-original-range range)
                  (compose (curry #'impl function) (read-key range)))
    range))


(defmethod cl-ds:reset! ((range flatten-proxy))
  (setf (access-current range) nil)
  (call-next-method)
  range)


(defmethod cl-ds:peek-front ((range forward-flatten-proxy))
  (bind ((current (access-current range))
         ((:labels value (element))
          (cond ((null element) nil)
                ((atom element) element)
                ((listp element)
                 (iterate
                   (for elt in element)
                   (for result = (value elt))
                   (until result)
                   (finally (return result))))))
         (result (value current)))
    (if (null result)
        (~> range cl-ds:clone cl-ds:consume-front)
        (values result t))))


(defmethod cl-ds:consume-front ((range forward-flatten-proxy))
  (bind ((key (read-key range))
         ((:accessors (current access-current)) range)
         ((:labels value ())
          (cond ((null current) nil)
                ((atom current) (shiftf current nil))
                ((listp current)
                 (iterate
                   (for elt = (first current))
                   (setf current (rest current))
                   (cond ((listp elt)
                          (setf current (append elt current)))
                         ((atom elt)
                          (leave elt)))
                   (until (null current))))))
         (result (value)))
    (if (null result)
        (iterate
          (with outer = (read-original-range range))
          (for (values f-val more) = (cl-ds:consume-front outer))
          (when (not more)
            (leave (values nil nil)))
          (setf current (funcall key f-val))
          (for result = (value))
          (unless (null result)
            (leave (values result t))))
        (values result t))))


(defclass flatten-lists-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric flatten-lists (range &key key)
  (:generic-function-class flatten-lists-function)
  (:method (range &key (key #'identity))
    (ensure-functionf key)
    (apply-range-function range #'flatten-lists
                          (list range :key key))))


(defmethod apply-layer ((range traversable)
                        (function flatten-lists-function)
                        all)
  (make 'forward-flatten-proxy
        :key (getf (rest all) :key)
        :original-range range))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range flatten-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (declare (optimize (speed 3) (safety 0)))
  (let ((flatten-key (ensure-function (read-key range)))
        (outer-fn (call-next-method)))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.utils:cases ((:variant (eq flatten-key #'identity)))
       (cl-ds.alg.meta:let-aggregator ((inner (cl-ds.alg.meta:call-constructor outer-fn)))
           ((element)
             (labels ((impl (x)
                        (if (listp x)
                            (map nil #'impl x)
                            (cl-ds.alg.meta:pass-to-aggregation inner x))))
               (~>> element (funcall flatten-key) impl)))

           ((cl-ds.alg.meta:extract-result inner))

        (cl-ds.alg.meta:cleanup inner)))
     function
     arguments)))
