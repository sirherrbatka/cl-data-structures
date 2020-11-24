(cl:in-package #:cl-data-structures.math)


(defclass moving-average-range (cl-ds.alg:proxy-range
                                cl-ds:fundamental-forward-range)
  ((%initial-count :initarg :count
                   :reader read-initial-count
                   :initform 0)
   (%initial-sum :initarg :sum
                 :reader read-initial-sum
                 :initform 0)
   (%count :initarg :count
           :accessor access-count
           :initform 0)
   (%sum :initarg :sum
         :accessor access-sum
         :initform 0)
   (%key :initarg :key
         :reader read-key)))


(defmethod cl-ds.utils:cloning-information append
    ((range moving-average-range))
  '((:count access-count)
    (:sum access-sum)
    (:key read-key)))


(defmethod cl-ds:reset! ((range moving-average-range))
  (call-next-method)
  (setf (access-sum range) (read-initial-sum range)
        (access-count range) (read-initial-count range))
  range)


(defmethod cl-ds:peek-front ((range moving-average-range))
  (bind (((:values v more) (call-next-method)))
    (if (no more)
        (values v nil)
        (let* ((count (~> range access-count 1+))
               (key (~> range read-key))
               (sum (+ (access-sum range)
                       (funcall key v))))
          (values (/ sum count)
                  t)))))


(defmethod cl-ds:consume-front ((range moving-average-range))
  (bind (((:values v more) (call-next-method)))
    (if (no more)
        (values v nil)
        (let* ((count (~> range access-count 1+))
               (key (~> range read-key))
               (value (funcall key v))
               (sum (+ (access-sum range)
                       value)))
          (setf (access-sum range) sum)
          (incf (access-count range))
          (values (/ sum count)
                  t)))))


(defmethod cl-ds:traverse ((range moving-average-range) function)
  (let* ((key (read-key range)))
    (call-next-method range
                      (lambda (elt &aux (value (funcall key elt)))
                        (funcall function
                                 (/ (incf (access-sum range) value)
                                    (incf (access-count range))))))))


(defmethod cl-ds:across ((range moving-average-range) function)
  (let* ((key (read-key range))
         (sum (access-sum range))
         (count (access-count range)))
    (call-next-method range
                      (lambda (elt &aux (value (funcall key elt)))
                        (funcall function
                                 (/ (incf sum value)
                                    (incf count range)))))))


(defclass moving-average-function (cl-ds.alg.meta:layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric moving-average (range &key key)
  (:generic-function-class moving-average-function)
  (:method (range &key (key #'identity))
    (ensure-functionf key)
    (cl-ds.alg.meta:apply-range-function range #'moving-average
                                         (list range :key key))))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-forward-range)
                                       (fn moving-average-function)
                                       all)
  (cl-ds.alg:make-proxy range 'moving-average-range
                        :key (getf (rest all) :key)))
