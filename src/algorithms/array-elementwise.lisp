(cl:in-package #:cl-data-structures.algorithms)


(defclass array-elementwise-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric array-elementwise (range)
  (:generic-function-class array-elementwise-function)
  (:method (range)
    (apply-range-function range #'array-elementwise (list range))))


(defclass array-elementwise-forward-proxy (forward-proxy-range)
  ())


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:traversable)
                                       (function array-elementwise-function)
                                       arguments)
  (make 'array-elementwise-forward-proxy :original-range range))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range array-elementwise-forward-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (declare (optimize (speed 3)))
  (let ((outer-fn (call-next-method)))
    (assert (functionp outer-fn))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.alg.meta:let-aggregator
         ((inners nil))

         ((element)
           (check-type element array)
           (when (null inners)
             (setf inners (copy-array element :element-type t))
             (iterate
               (declare (type fixnum i))
               (for i from 0 below (if (array-has-fill-pointer-p inners)
                                       (fill-pointer inners)
                                       (array-total-size inners)))
               (setf (row-major-aref inners i)
                     (cl-ds.alg.meta:call-constructor outer-fn))))
           (iterate
             (declare (type fixnum i))
             (for i from 0 below (if (array-has-fill-pointer-p element)
                                     (length element)
                                     (array-total-size element)))
             (cl-ds.alg.meta:pass-to-aggregation (row-major-aref inners i)
                                                 (row-major-aref element i))))

         ((unless (null inners)
            (iterate
              (declare (type fixnum i))
              (for i from 0 below (if (array-has-fill-pointer-p inners)
                                      (fill-pointer inners)
                                      (array-total-size inners)))
              (setf #1=(row-major-aref inners i)
                    (cl-ds.alg.meta:extract-result #1#))
              (finally (return inners)))))

       (unless (null inners)
         (iterate
           (declare (type fixnum i))
           (for i from 0 below (if (array-has-fill-pointer-p inners)
                                   (fill-pointer inners)
                                   (array-total-size inners)))
           (cl-ds.alg.meta:cleanup (row-major-aref inners i)))))
     function
     arguments)))
