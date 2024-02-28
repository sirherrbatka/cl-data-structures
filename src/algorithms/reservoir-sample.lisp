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
  replacement
  test
  (w 0.0d0 :type double-float)
  (skip-count 0 :type fixnum))


(defun make-reservoir-sampling (sample-size &optional (replacement t) (test #'eql))
  (check-type sample-size positive-fixnum)
  (let ((w (gen-w sample-size)))
    (make-reservoir-sampling*
     :result (make-array sample-size :fill-pointer 0)
     :replacement replacement
     :test test
     :w w
     :skip-count (calculate-skip-count w))))


(defun reservoir-sampling-sample-size (reservoir-sampling)
  (~> reservoir-sampling reservoir-sampling-result (array-dimension 0)))


(cl-ds.utils:define-list-of-slots reservoir-sampling ()
  (result reservoir-sampling-result)
  (w reservoir-sampling-w)
  (replacement reservoir-sampling-replacement)
  (test reservoir-sampling-test)
  (skip-count reservoir-sampling-skip-count)
  (sample-size reservoir-sampling-sample-size))


(defun reservoir-sampling-push (reservoir-sampling element)
  (check-type reservoir-sampling reservoir-sampling)
  (cl-ds.utils:with-slots-for (reservoir-sampling reservoir-sampling)
    (cond ((and (< (fill-pointer result) sample-size)
                (nand (not replacement)
                      (find element result :test test)))
           (vector-push element result))
          ((and (zerop skip-count)
                (nand (not replacement)
                      (find element result :test test)))
           (setf skip-count (calculate-skip-count w)
                 (aref result (random sample-size)) element
                 w (gen-w sample-size w)))
          (t (decf skip-count)))))


(cl-ds.alg.meta:define-aggregation-function
    reservoir-sample reservoir-sample-function

    (:range sample-size &key key after test replacement)
    (:range sample-size &key (key #'identity) (after #'identity) (test #'eql) (replacement t))

    ((%result reservoir-sampling))

    ((setf %result (make-reservoir-sampling sample-size replacement test)))

    ((element)
     (reservoir-sampling-push %result element))

    ((reservoir-sampling-result %result)))
