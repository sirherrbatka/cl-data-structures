(cl:in-package #:cl-data-structures.algorithms)


(defclass abstract-sliding-window-proxy ()
  ((%window-size :initarg :window-size
                 :reader read-window-size)))


(defclass forward-sliding-window-proxy
    (abstract-sliding-window-proxy
     forward-proxy-range)
  ())


(defclass bidirectional-sliding-window-proxy
    (abstract-sliding-window-proxy
     bidirectional-proxy-range)
  ())


(defclass random-access-sliding-window-proxy
    (abstract-sliding-window-proxy
     random-access-proxy-range)
  ())


(defmethod clone ((range abstract-sliding-window-proxy))
  (make (class-of range)
        :window-size (read-window-size range)
        :original-range (~> range read-original-range clone)))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range abstract-sliding-window-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (bind ((window-size (read-window-size range))
         (outer-fn (call-next-method)))
    (assert (functionp outer-fn))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.alg.meta:let-aggregator
         ((window (make-list window-size))
          (tail (last window))
          (inner (cl-ds.alg.meta:call-constructor outer-fn))
          (current window))

         ((element)
          (setf (first current) element)
          (setf current (rest current))
          (when (endp current)
            (~>> window
                 copy-list
                 (cl-ds.alg.meta:pass-to-aggregation inner))
            (psetf (cdr tail) window
                   tail window
                   window (rest window)
                   (cdr window) nil)
            (setf current tail)))

         ((cl-ds.alg.meta:extract-result inner)))

     function
     arguments)))


(defclass sliding-window-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric sliding-window (range batch-size)
  (:generic-function-class in-batches-function)
  (:method (range window-size)
    (check-type window-size positive-integer)
    (apply-range-function range #'sliding-window
                          (list range window-size))))


(defmethod apply-layer ((range traversable)
                        (fn in-batches-function)
                        all)
  (make-proxy range 'forward-sliding-window-proxy
              :window-size (second all)))


(defmethod apply-layer ((range cl-ds:fundamental-bidirectional-range)
                        (fn in-batches-function)
                        all)
  (make-proxy range 'bidirectional-sliding-window-proxy
              :window-size (second all)))


(defmethod apply-layer ((range cl-ds:fundamental-random-access-range)
                        (fn in-batches-function)
                        all)
  (make-proxy range 'random-access-sliding-window-proxy
              :window-size (second all)))
