(cl:in-package #:cl-data-structures.algorithms)


(declaim (inline gen-w))
(-> gen-w (fixnum &optional double-float) double-float)
(defun gen-w (count &optional (w 1.0d0))
  (declare (optimize (speed 3) (safety 0)))
  (* w (exp (/ (log (random 1.0d0))
               count))))


(declaim (inline calculate-skip-count))
(-> calculate-skip-count (positive-double-float) positive-fixnum)
(defun calculate-skip-count (w)
  (declare (optimize (speed 3) (safety 0)))
  (1+ (the fixnum (floor (/ (the negative-double-float (log (random 1.0d0)))
                            (the negative-double-float (log (- 1 w))))))))


(defstruct (reservoir-sampling (:constructor make-reservoir-sampling*))
  result
  (w 0.0d0 :type double-float)
  (skip-count 0 :type fixnum))


(defun make-reservoir-sampling (sample-size)
  (check-type sample-size positive-fixnum)
  (let ((w (gen-w sample-size)))
    (make-reservoir-sampling*
     :result (make-array sample-size :fill-pointer 0)
     :w w
     :skip-count (calculate-skip-count w))))


(defun reservoir-sampling-sample-size (reservoir-sampling)
  (~> reservoir-sampling reservoir-sampling-result (array-dimension 0)))


(cl-ds.utils:define-list-of-slots reservoir-sampling ()
  (result reservoir-sampling-result)
  (w reservoir-sampling-w)
  (skip-count reservoir-sampling-skip-count)
  (sample-size reservoir-sampling-sample-size))


(defun reservoir-sampling-push (reservoir-sampling element)
  (check-type reservoir-sampling reservoir-sampling)
  (cl-ds.utils:with-slots-for (reservoir-sampling reservoir-sampling)
    (cond ((< (fill-pointer result) sample-size)
           (vector-push element result))
          ((zerop skip-count)
           (setf skip-count (calculate-skip-count w)
                 (aref result (random sample-size)) element
                 w (gen-w sample-size w)))
          (t (decf skip-count)))))


(cl-ds.alg.meta:define-aggregation-function
    reservoir-sample reservoir-sample-function

    (:range sample-size &key key after)
    (:range sample-size &key (key #'identity) (after #'identity))

    ((%result reservoir-sampling))

    ((setf %result (make-reservoir-sampling sample-size)))

    ((element)
     (reservoir-sampling-push %result element))

    ((reservoir-sampling-result %result)))
