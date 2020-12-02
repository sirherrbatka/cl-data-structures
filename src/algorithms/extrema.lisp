(cl:in-package #:cl-data-structures.algorithms)


(cl-ds.alg.meta:define-aggregation-function
    extrema extrema-function

  (:range fn &key key value-key after)
  (:range fn &key (key #'identity) (value-key #'identity) (after #'identity))

  (%low-value %high-value %fn %first-iteration %value-key)

  ((ensure-functionf value-key fn)
   (setf %fn fn
         %value-key value-key
         %first-iteration t))

  ((element)
   (let ((value-key (ensure-function %value-key))
         (fn (ensure-function %fn)))
     (cl-ds.utils:lazy-let ((elt (funcall value-key element))
                            (high-value (funcall value-key %high-value))
                            (low-value (funcall value-key %low-value)))
       (cond (%first-iteration
              (setf %low-value element
                    %high-value element
                    %first-iteration nil))
             ((funcall fn elt high-value)
              (setf %high-value element))
             ((funcall fn low-value elt)
              (setf %low-value element))))))

  ((list* %high-value %low-value)))
