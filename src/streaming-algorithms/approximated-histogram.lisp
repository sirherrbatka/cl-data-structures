(cl:in-package #:cl-ds.sa)


(defclass approximated-histogram ()
  ((%max-bins :initarg :maximal-bins-count
              :reader maximal-bins-count
              :reader read-max-bins)
   (%bins :initarg :bins :accessor access-bins)
   (%fill-pointer :initarg :fill-pointer
                  :accessor access-fill-pointer)
   (%count :initarg :count
           :accessor access-count)
   (%min :initarg :min :accessor access-min)
   (%max :initarg :max :accessor access-max))
  (:default-initargs
   :maximal-bins-count 128
   :fill-pointer 0
   :count 0
   :bins (vector)
   :min most-positive-double-float
   :max most-negative-double-float))


(defmethod print-object ((object approximated-histogram)
                         stream)
  (print-unreadable-object (object stream :type t)
    (format stream "count: ~a min: ~a max: ~a"
            (access-count object)
            (access-min object)
            (access-max object))))


(defun make-approximated-histogram (&key (maximal-bins-count 128) initial-content)
  (check-type maximal-bins-count integer)
  (cl-ds:check-argument-bounds maximal-bins-count
                               (<= 4 maximal-bins-count array-total-size-limit))
  (if (emptyp initial-content)
      (make 'approximated-histogram
            :maximal-bins-count maximal-bins-count
            :bins (map-into (make-array (1+ maximal-bins-count))
                            #'make-approximated-histogram-bin))
      (make 'approximated-histogram
            :maximal-bins-count maximal-bins-count
            :bins  (~> (map 'vector
                            (lambda (value)
                              (make-approximated-histogram-bin :value (coerce value 'double-float)
                                                               :count 1.0d0))
                            initial-content)
                       (sort #'< :key #'approximated-histogram-bin-value))
            :fill-pointer (length initial-content)
            :count (length initial-content)
            :min (coerce (reduce #'min initial-content) 'double-float)
            :max (coerce (reduce #'max initial-content) 'double-float))))


(defstruct approximated-histogram-bin
  (value 0.0d0 :type double-float)
  (count 0.0d0 :type double-float))


(defun approximated-histogram-bin-clone (bin)
  (check-type bin approximated-histogram-bin)
  (make-approximated-histogram-bin
   :value (approximated-histogram-bin-value bin)
   :count (approximated-histogram-bin-count bin)))


(defun approximated-histogram-bin-sum (bin)
  (check-type bin approximated-histogram-bin)
  (* (abs (approximated-histogram-bin-count bin))
     (approximated-histogram-bin-value bin)))


(defun approximated-histogram-trim (histogram)
  (check-type histogram approximated-histogram)
  (bind (((:accessors (fill-pointer access-fill-pointer))
          histogram)
         (max-bins (read-max-bins histogram))
         (bins (access-bins histogram)))
    (iterate
      (while (> fill-pointer max-bins))
      (for delta = most-positive-double-float)
      (for pos = 0)
      (iterate
        (for i from 0 below (1- fill-pointer))
        (for b1 = (aref bins i))
        (for b2 = (aref bins (1+ i)))
        (for x = (- (approximated-histogram-bin-value b2)
                    (approximated-histogram-bin-value b1)))
        (when (< x delta)
          (setf pos i delta x)))
      (for b1 = (aref bins pos))
      (for b2 = (aref bins (1+ pos)))
      (for count = (+ (approximated-histogram-bin-count b1)
                      (approximated-histogram-bin-count b2)))
      (for value = (/ (+ (approximated-histogram-bin-sum b1)
                         (approximated-histogram-bin-sum b2))
                      count))
      (setf (approximated-histogram-bin-count b2) count)
      (setf (approximated-histogram-bin-value b2) value)
      (replace bins bins
               :start1 pos
               :start2 (1+ pos))
      (decf fill-pointer)))
  histogram)


(defun approximated-histogram-bins (histogram)
  (check-type histogram approximated-histogram)
  (replace (make-array (access-fill-pointer histogram))
           (access-bins histogram)))


(defun approximated-histogram-bin-position (histogram value)
  (check-type value double-float)
  (check-type histogram approximated-histogram)
  (cl-ds.utils:lower-bound (access-bins histogram)
                           value
                           #'<
                           :end (access-fill-pointer histogram)
                           :key #'approximated-histogram-bin-value))


(defun approximated-histogram-add (histogram value)
  (check-type histogram approximated-histogram)
  (setf value (coerce value 'double-float))
  (bind (((:accessors (min access-min)
                      (max access-max)
                      (count access-count)
                      (bins access-bins)
                      (fill-pointer access-fill-pointer))
          histogram)
         (position (approximated-histogram-bin-position histogram value))
         (length (length bins))
         ((:flet adjust-bins ())
          (when (= length fill-pointer)
            (setf bins (adjust-array bins (1+ length))))))
    (minf min value)
    (maxf max value)
    (incf count)
    (cond ((and (< position fill-pointer)
                (~> (aref bins position)
                    approximated-histogram-bin-value
                    (= value)))
           (let* ((bin (aref bins position)))
             (incf (approximated-histogram-bin-count bin))))
          ((= position fill-pointer)
           (adjust-bins)
           (setf (aref bins fill-pointer)
                 (make-approximated-histogram-bin
                  :value value
                  :count 1.0d0))
           (incf fill-pointer))
          (t
           (adjust-bins)
           (replace bins bins
                    :start1 (1+ position)
                    :start2 position)
           (incf fill-pointer)
           (setf (aref bins position)
                 (make-approximated-histogram-bin
                  :value value
                  :count 1.0d0))))
    (approximated-histogram-trim histogram)
    histogram))


(defun approximated-histogram-count (histogram)
  (check-type histogram approximated-histogram)
  (reduce #'+
          (approximated-histogram-bins histogram)
          :key #'approximated-histogram-bin-count))


(defun approximated-histogram-count-lower
    (histogram value
     &rest other-values
     &aux (all-values (cons value other-values)))
  (check-type histogram approximated-histogram)
  (assert (cl-ds.utils:ordered-p all-values #'<))
  (bind (((:accessors (min access-min)
                      (max access-max)
                      (count access-count)
                      (bins access-bins)
                      (fill-pointer access-fill-pointer))
          histogram)
         (sum 0.0d0)
         (i 0)
         ((:flet impl (value))
          (cond ((< value min)
                 (return-from approximated-histogram-count-lower (list 0.0d0)))
                ((>= value max)
                 (return-from approximated-histogram-count-lower
                   (list (coerce count 'double-float)))))
          (bind (((:values position prev-sum)
                  (iterate
                    (while (< i fill-pointer))
                    (for bin = (aref bins i))
                    (for bin-value = (approximated-histogram-bin-value bin))
                    (while (< bin-value value))
                    (incf sum (approximated-histogram-bin-count bin))
                    (incf i)
                    (finally (return (values i sum)))))
                 (bin-at-position (aref bins position))
                 (value-at-position (approximated-histogram-bin-value bin-at-position))
                 (count-at-position (approximated-histogram-bin-count bin-at-position))
                 (this-sum (+ prev-sum (/ count-at-position 2)))
                 (next-position (1+ position))
                 (bin-at-next-position
                  (if (< next-position fill-pointer)
                      (aref bins next-position)
                      (make-approximated-histogram-bin :value max)))
                 (b-diff (- value value-at-position))
                 (p-diff (- (approximated-histogram-bin-value bin-at-next-position)
                            value-at-position))
                 (bp-ratio (/ b-diff p-diff))
                 (term1 (/ (* bp-ratio bp-ratio) 2))
                 (term2 (- bp-ratio term1))
                 (first (~> count-at-position
                            (* term1 )
                            (+ this-sum)))
                 (ss (~> bin-at-next-position
                         approximated-histogram-bin-count
                         (* term2)
                         (+ first))))
            ss)))
    (mapcar #'impl all-values)))


(defun approximated-histogram-quantile
    (histogram quantile
     &rest other-quantiles
     &aux (all-quantiles (cons quantile other-quantiles)))
  (check-type histogram approximated-histogram)
  (assert (cl-ds.utils:ordered-p all-quantiles #'<))
  (bind (((:accessors (min access-min)
                      (max access-max)
                      (count access-count)
                      (bins access-bins)
                      (fill-pointer access-fill-pointer))
          histogram)
         (lhs-total 0.0d0)
         (rhs-total 0.0d0)
         (lhs-count 0)
         (rhs-count 0)
         (lhs-centroid 0.0d0)
         (lhs -1)
         (rhs-centroid 0.0d0)
         ((:flet impl (quantile &aux (target-sum (* quantile count))))
          (cond ((<= quantile 0)
                 (return-from impl (list min)))
                ((>= quantile 1)
                 (return-from impl (list max))))
          (iterate
            (while (< rhs-total target-sum))
            (for rhs = (1+ lhs))
            (if (< lhs 0)
                (setf lhs-centroid min
                      lhs-count 0)
                (let ((bin (aref bins lhs)))
                  (setf lhs-centroid (approximated-histogram-bin-value bin)
                        lhs-count (approximated-histogram-bin-count bin))))
            (if (> rhs fill-pointer)
                (setf rhs-centroid max
                      rhs-count 0)
                (let ((bin (aref bins rhs)))
                  (setf rhs-centroid (approximated-histogram-bin-value bin)
                        rhs-count (approximated-histogram-bin-count bin))))
            (setf lhs-total rhs-total)
            (incf rhs-total (/ (+ lhs-count rhs-count) 2))
            (setf lhs rhs))
          (let* ((a (- rhs-count lhs-count))
                 (z 0))
            (if (zerop a)
                (let ((b (- rhs-total lhs-total)))
                  (if (zerop b)
                      (setf z 0)
                      (setf z (/ (- target-sum lhs-total) b))))
                (let ((b (* 2 lhs-count))
                      (c (* 2 (- lhs-total target-sum))))
                  (setf z (/ (- (sqrt (- (* b b) (* 4 a c))) b)
                             (* 2 a)))))
            (~> (- rhs-centroid lhs-centroid)
                (* z)
                (+ lhs-centroid)))))
    (mapcar #'impl all-quantiles)))


(defmethod cl-ds:at ((object approximated-histogram) location &rest more-locations)
  (cl-ds:assert-one-dimension more-locations)
  (first (approximated-histogram-quantile object location)))


(defun approximated-histogram-median (histogram)
  (check-type histogram approximated-histogram)
  (first (approximated-histogram-quantile histogram 0.5)))


(defun approximated-histogram-sum (histogram)
  (check-type histogram approximated-histogram)
  (reduce #'+ (access-bins histogram)
          :key #'approximated-histogram-bin-sum
          :end (access-fill-pointer histogram)))


(defun approximated-histogram-cumulant-sum (histogram)
  (check-type histogram approximated-histogram)
  (iterate
    (with fill-pointer = (access-fill-pointer histogram))
    (with result = (make-array fill-pointer :element-type 'double-float))
    (with total = 0.0d0)
    (with bins = (access-bins histogram))
    (for i from 0 below fill-pointer)
    (for bin = (aref bins i))
    (setf (aref result i)
          (incf total (approximated-histogram-bin-sum bin)))
    (finally (return result))))


(defun approximated-histogram-values (histogram)
  (check-type histogram approximated-histogram)
  (iterate
    (with fill-pointer = (access-fill-pointer histogram))
    (with result = (make-array fill-pointer :element-type 'double-float))
    (with bins = (access-bins histogram))
    (for i from 0 below fill-pointer)
    (for bin = (aref bins i))
    (setf (aref result i)
          (approximated-histogram-bin-value bin))
    (finally (return result))))


(defun approximated-histogram-counts (histogram)
  (check-type histogram approximated-histogram)
  (iterate
    (with fill-pointer = (access-fill-pointer histogram))
    (with result = (make-array fill-pointer :element-type 'double-float))
    (with bins = (access-bins histogram))
    (for i from 0 below fill-pointer)
    (for bin = (aref bins i))
    (setf (aref result i)
          (approximated-histogram-bin-count bin))
    (finally (return result))))


(defun approximated-histogram-truncated-mean (histogram fraction)
  (check-type histogram approximated-histogram)
  (check-type fraction real)
  (assert (< 0.0 fraction 1.0))
  (bind ((bins (access-bins histogram))
         (low fraction)
         (high (- 1.0 low))
         (fill-pointer (access-fill-pointer histogram))
         ((low-quantile high-quantile)
          (approximated-histogram-quantile histogram low high))
         (low-bound (approximated-histogram-bin-position histogram
                                                         low-quantile))
         (high-bound (approximated-histogram-bin-position histogram
                                                          high-quantile))
         (total 0.0d0)
         (count 0.0d0))
    (if (= low-bound high-bound)
        0.0d0
        (let ((first-bin (aref bins low-bound))
              (last-bin (if (< high-bound fill-pointer)
                            (aref bins high-bound)
                            nil)))
          (incf total (/ (approximated-histogram-bin-sum first-bin) 2.0))
          (incf count (/ (approximated-histogram-bin-count first-bin) 2.0))
          (unless (null last-bin)
            (incf total (/ (approximated-histogram-bin-sum last-bin) 2.0))
            (incf count (/ (approximated-histogram-bin-count last-bin) 2.0)))
          (iterate
            (for i from (1+ low-bound) below high-bound)
            (for bin = (aref bins i))
            (incf total (approximated-histogram-bin-sum bin))
            (incf count (approximated-histogram-bin-count bin))
            (finally (return (/ total count))))))))


(defun approximated-histogram-mean (histogram)
  (check-type histogram approximated-histogram)
  (if (zerop (access-count histogram))
      0.0d0
      (/ (approximated-histogram-sum histogram)
         (access-count histogram))))


(defun approximated-histogram-variance (histogram)
  (check-type histogram approximated-histogram)
  (bind (((:accessors (count access-count)
                      (bins access-bins)
                      (fill-pointer access-fill-pointer))
          histogram))
    (when (< count 2)
      (return-from approximated-histogram-variance 0.0d0))
    (iterate
      (with mean = (approximated-histogram-mean histogram))
      (for i from 0 below fill-pointer)
      (for bin = (aref bins i))
      (for delta = (- mean (approximated-histogram-bin-value bin)))
      (summing (* delta delta (approximated-histogram-bin-count bin))
        into sum)
      (finally (return (/ sum (1- count)))))))


(defun approximated-histogram-standard-deviation (histogram)
  (~> histogram approximated-histogram-variance sqrt))


(defun approximated-histogram-rank-order (histogram value &rest other-values)
  (check-type histogram approximated-histogram)
  (~>> (apply #'approximated-histogram-count-lower histogram value other-values)
       (cl-ds.utils:transform (rcurry #'/ (access-count histogram)))))


(defun approximated-histogram-bounds (histogram)
  (check-type histogram approximated-histogram)
  (cons (access-min histogram)
        (access-max histogram)))


(defmethod union ((first-sketch approximated-histogram)
                  &rest more-sketches
                  &aux
                    (list (cons first-sketch more-sketches))
                    (total-bin-count (reduce #'+ list :key #'access-fill-pointer)))
  (check-type first-sketch approximated-histogram)
  (assert (cl-ds.utils:homogenousp list :key #'class-of))
  (let* ((new-bins (make-array total-bin-count))
         (result (make 'approximated-histogram
                       :fill-pointer total-bin-count
                       :maximal-bins-count (read-max-bins first-sketch)
                       :min (reduce #'min list :key #'access-min)
                       :max (reduce #'max list :key #'access-max)
                       :count (reduce #'+ list :key #'access-count)
                       :bins new-bins)))
    (iterate
      (with i = 0)
      (for sketch in list)
      (for bins = (access-bins sketch))
      (iterate
        (for j from 0 below (access-fill-pointer sketch))
        (for bin = (aref bins j))
        (setf (aref new-bins i) (approximated-histogram-bin-clone bin))
        (incf i)))
    (approximated-histogram-trim result)
    (setf (access-bins result)
          (adjust-array new-bins (1+ (read-max-bins result))))
    result))


(cl-ds.alg.meta:define-aggregation-function
    approximated-histogram approximated-histogram-function

    (:range &key key maximal-bins-count data-sketch)
    (:range &key (key #'identity)
     (maximal-bins-count 128)
     (data-sketch (clean-sketch #'approximated-histogram :maximal-bins-count maximal-bins-count)))

    (%data-sketch)

    ((check-type data-sketch approximated-histogram)
     (setf %data-sketch (cl-ds:clone data-sketch)))

    ((element)
     (approximated-histogram-add %data-sketch element))

    (%data-sketch))


(defmethod clean-sketch ((function approximated-histogram-function)
                         &rest all &key maximal-bins-count)
  (declare (ignore all))
  (make-approximated-histogram :maximal-bins-count maximal-bins-count))


(defmethod cl-ds:clone ((object approximated-histogram))
  (make (class-of object)
        :bins (map 'vector
                   #'approximated-histogram-bin-clone
                   (access-bins object))
        :maximal-bins-count (read-max-bins object)
        :fill-pointer (access-fill-pointer object)
        :count (access-count object)
        :min (access-min object)
        :max (access-max object)))
