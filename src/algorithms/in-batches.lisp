(cl:in-package #:cl-data-structures.algorithms)


(defclass abstract-in-batches-proxy ()
  ((%batch-size :initarg :batch-size
                :reader read-batch-size)))


(defclass forward-in-batches-proxy
    (abstract-in-batches-proxy
     forward-proxy-range)
  ())


(defclass bidirectional-in-batches-proxy
    (abstract-in-batches-proxy
     bidirectional-proxy-range)
  ())


(defclass random-access-in-batches-proxy
    (abstract-in-batches-proxy
     random-access-proxy-range)
  ())


(defmethod clone ((range abstract-in-batches-proxy))
  (make (class-of range)
        :batch-size (read-batch-size range)
        :original-range (~> range read-original-range clone)))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range abstract-in-batches-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (let ((batch-size (read-batch-size range))
        (outer-fn (call-next-method)))
    (declare (type positive-integer batch-size))
    (assert (functionp outer-fn))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.alg.meta:layer-aggregator-constructor #'in-batches
                                                  outer-fn
                                                  (list batch-size))
     function
     arguments)))


(defclass in-batches-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defmethod cl-ds.alg.meta:layer-aggregator-constructor ((function in-batches-function)
                                                        outer-constructor
                                                        arguments)
  (let ((batch-size (first arguments)))
    (check-type batch-size positive-integer)
    (cl-ds.alg.meta:let-aggregator
        ((chunks (vect))
         (chunk-counter 0))

        ((element)
          (when (zerop chunk-counter)
            (vector-push-extend (cl-ds.alg.meta:call-constructor outer-constructor)
                                chunks))
          (setf chunk-counter (mod (1+ chunk-counter)
                                   batch-size))
          (cl-ds.alg.meta:pass-to-aggregation (last-elt chunks)
                                              element))

        ((map 'vector #'cl-ds.alg.meta:extract-result chunks)))))


(defgeneric in-batches (range batch-size)
  (:generic-function-class in-batches-function)
  (:method (range batch-size)
    (check-type batch-size positive-integer)
    (apply-range-function range #'in-batches
                          (list range batch-size))))


(defmethod apply-layer ((range traversable)
                        (fn in-batches-function)
                        all)
  (make-proxy range 'forward-in-batches-proxy
              :batch-size (second all)))


(defmethod apply-layer ((range cl-ds:fundamental-bidirectional-range)
                        (fn in-batches-function)
                        all)
  (make-proxy range 'bidirectional-in-batches-proxy
              :batch-size (second all)))


(defmethod apply-layer ((range cl-ds:fundamental-random-access-range)
                        (fn in-batches-function)
                        all)
  (make-proxy range 'random-access-in-batches-proxy
              :batch-size (second all)))
