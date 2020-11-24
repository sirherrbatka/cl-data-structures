(cl:in-package #:cl-data-structures.algorithms)


(defclass repeat-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric repeat (range times)
  (:generic-function-class repeat-function)
  (:method (range times)
    (apply-range-function range #'repeat
                          (list range #'repeat times))))


(defclass repeat-proxy (proxy-range)
  ((%times :initarg :times
           :reader read-times)
   (%position :initform 0
              :initarg :position
              :accessor access-position)
   (%initial-position :initform 0
                      :initarg :position
                      :reader read-initial-position)))


(defclass forward-repeat-proxy (cl-ds:chunking-mixin
                                repeat-proxy
                                forward-proxy-range)
  ())


(defmethod cl-ds:clone ((range repeat-proxy))
  (make (type-of range)
        :original-range (~> range read-original-range cl-ds:clone)
        :times (read-times range)
        :position (access-position range)))


(defmethod cl-ds:consume-front ((range repeat-proxy))
  (if (or (null (read-times range))
          (< (access-position range) (read-times range)))
      (bind (((:values result more) (~> range
                                        read-original-range
                                        cl-ds:consume-front)))
        (if more
            (values result more)
            (progn
              (incf (access-position range))
              (~> range read-original-range cl-ds:reset!)
              (cl-ds:consume-front range))))
      (values nil nil)))


(defmethod cl-ds:peek-front ((range repeat-proxy))
  (if (or (null (read-times range))
          (< (access-position range) (read-times range)))
      (~> range cl-ds:clone cl-ds:consume-front)
      (~> range read-original-range cl-ds:peek-front)))


(defmethod cl-ds:reset! ((range repeat-proxy))
  (setf (access-position range) (read-initial-position range))
  (call-next-method))


(defmethod cl-ds.alg.meta:apply-layer ((range fundamental-range)
                                       (fn repeat-function)
                                       all)
  (let ((times (getf (rest all) :times)))
    (check-type times positive-integer)
    (make 'forward-repeat-proxy
          :times times
          :original-range range)))


(defmethod cl-ds:across ((range repeat-proxy) function)
  (iterate
    (with og-range = (~> range read-original-range))
    (for i from (access-position range) below (read-times range))
    (cl-ds:across og-range function))
  range)


(defmethod cl-ds:traverse ((range repeat-proxy) function)
  (iterate
    (with og-range = (~> range read-original-range))
    (for i from (access-position range) below (read-times range))
    (cl-ds:traverse og-range function)
    (cl-ds:reset! og-range)
    (incf (access-position range)))
  range)


(defmethod cl-ds.alg.meta:aggregator-constructor ((range repeat-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (declare (optimize (speed 3) (safety 0)))
  (let ((outer-fn (call-next-method))
        (position (access-position range))
        (size (read-size range)))
    (declare (type fixnum position size))
    (assert (functionp outer-fn))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.alg.meta:let-aggregator
         ((inner (cl-ds.alg.meta:call-constructor outer-fn))
          (data (vect)))

         ((element)
           (vector-push-extend element data))

         ((iterate
            (declare (type fixnum i))
            (for i from position below size)
            (iterate
              (for elt in-vector data)
              (cl-ds.alg.meta:pass-to-aggregation inner elt)))
           (cl-ds.alg.meta:extract-result inner)

          (cl-ds.alg.meta:cleanup inner)))
     function
     arguments)))
