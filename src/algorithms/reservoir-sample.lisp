(cl:in-package #:cl-data-structures.algorithms)


(declaim (inline gen-w))
(-> gen-w (fixnum &optional double-float) double-float)
(defun gen-w (count &optional (w 1.0d0))
  (declare (optimize (speed 3) (safety 0)))
  (* w (exp (/ (log (random 1.0d0))
               count))))


(declaim (inline skip-count))
(-> skip-count (positive-double-float) positive-fixnum)
(defun skip-count (w)
  (declare (optimize (speed 3) (safety 0)))
  (1+ (the fixnum (floor (/ (the negative-double-float (log (random 1.0d0)))
                            (the negative-double-float (log (- 1 w))))))))


(cl-ds.alg.meta:define-aggregation-function
    reservoir-sample reservoir-sample-function

    (:range sample-size &key key after)
    (:range sample-size &key (key #'identity) (after #'identity))

    ((%result (cl-ds.utils:extendable-vector t))
     (%w double-float)
     (%skip-count fixnum)
     (%sample-size positive-fixnum))

    ((check-type sample-size positive-fixnum)
     (setf %result (make-array sample-size
                               :fill-pointer 0)
           %w (gen-w sample-size)
           %skip-count (skip-count %w)
           %sample-size sample-size))

    ((element)
     (cond ((< (fill-pointer %result) %sample-size)
            (vector-push element %result))
           ((zerop %skip-count)
            (setf %skip-count (skip-count %w)
                  (aref %result (random %sample-size)) element
                  %w (gen-w %sample-size %w)))
           (t (decf %skip-count))))

    (%result))
