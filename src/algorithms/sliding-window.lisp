(cl:in-package #:cl-data-structures.algorithms)


(defclass abstract-sliding-window-proxy ()
  ((%window-size :initarg :window-size
                 :reader read-window-size)
   (%initial-window :initarg :initial-window
                    :reader read-initial-window)
   (%initial-current-position :initarg :initial-current-position
                              :reader read-initial-current-position)
   (%window :initarg :window
            :accessor access-window)
   (%tail :initarg :tail
          :accessor access-tail)
   (%current :initarg :current
             :accessor access-current))
  (:default-initargs :initial-current-position 0))


(defclass forward-sliding-window-proxy
    (abstract-sliding-window-proxy
     forward-proxy-range)
  ())


(defmethod cl-ds:reset! ((range abstract-sliding-window-proxy))
  (call-next-method)
  (with-accessors ((window access-window)
                   (initial-window read-initial-window)
                   (tail access-tail)
                   (current access-current)
                   (initial-current-position read-initial-current-position))
      range
      (setf window (copy-list initial-window)
            tail (last window)
            current (nthcdr initial-current-position window)))
  range)


(defmethod clone ((range abstract-sliding-window-proxy))
  (let* ((old-window (access-window range))
         (old-current (access-current range))
         (old-current-position (iterate
                                 (for i from 0)
                                 (for li on old-window)
                                 (finding i such-that (eq li old-current))))
         (new-window (copy-list old-window))
         (new-current (nthcdr old-current-position new-window))
         (new-tail (last new-window)))
    (make (class-of range)
          :window new-window
          :initial-window (copy-list new-window)
          :initial-current-position old-current-position
          :tail new-tail
          :current new-current
          :window-size (read-window-size range)
          :original-range (~> range read-original-range clone))))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range abstract-sliding-window-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (bind ((old-window (access-window range))
         (old-current (access-current range))
         (outer-fn (call-next-method)))
    (assert (functionp outer-fn))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.alg.meta:layer-aggregator-constructor
      #'sliding-window
      outer-fn
      (list (read-window-size range)
            :window (access-window range)
            :current-position (iterate
                                (for i from 0)
                                (for li on old-window)
                                (finding i such-that (eq li old-current)))))
     function
     arguments)))


(defclass sliding-window-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defmethod cl-ds.alg.meta:layer-aggregator-constructor ((function sliding-window-function)
                                                        outer-fn
                                                        arguments)
  (bind (((batch-size . arguments) arguments)
         (old-current-position (getf arguments :current-position 0))
         (old-window (getf arguments :window (make-list batch-size))))
    (cl-ds.alg.meta:let-aggregator
        ((window (copy-list old-window))
         (tail (last window))
         (inner (cl-ds.alg.meta:call-constructor outer-fn))
         (current (nthcdr old-current-position window)))

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

        ((cl-ds.alg.meta:extract-result inner)))))


(defgeneric sliding-window (range batch-size)
  (:generic-function-class sliding-window-function)
  (:method (range window-size)
    (check-type window-size positive-integer)
    (apply-range-function range #'sliding-window
                          (list range window-size))))


(defmethod apply-layer ((range traversable)
                        (fn sliding-window-function)
                        all)
  (let* ((window-size (second all))
         (window (make-list window-size)))
    (check-type window-size positive-integer)
    (make-proxy range 'forward-sliding-window-proxy
                :window-size window-size
                :initial-window (copy-list window)
                :window window
                :tail (last window)
                :current window)))


(defmethod cl-ds:consume-front ((range abstract-sliding-window-proxy))
  (with-accessors ((window access-window)
                   (current access-current)
                   (tail access-tail))

      range
    (iterate
      (for (values data more) = (call-next-method))
      (unless more
        (leave (values nil nil)))
      (setf (first current) data)
      (setf current (rest current))
      (when (endp current)
        (let ((result (copy-list window)))
          (psetf (cdr tail) window
                 tail window
                 window (rest window)
                 (cdr window) nil)
          (setf current tail)
          (leave (values result t)))))))


(defmethod cl-ds:traverse ((range abstract-sliding-window-proxy)
                           function)
  (ensure-functionf function)
  (iterate
    (for (values data more) = (cl-ds:consume-front range))
    (while more)
    (funcall function data)
    (finally (return range))))


(defmethod cl-ds:across ((range abstract-sliding-window-proxy)
                         function)
  (~> range cl-ds:clone (cl-ds:traverse function)))
