(cl:in-package #:cl-data-structures.algorithms.meta)


(defvar *aggregation-results* nil)

(defparameter *run* t)

(defun push-to-aggregation-results (result)
  (unless (null *aggregation-results*)
    (vector-push-extend result *aggregation-results*))
  result)

(defmacro with-captured-aggregations (&body body)
  `(let ((*aggregation-results* (vect)))
     ,@body
     *aggregation-results*))

(defun captured-aggregations-list (captured-aggregations)
  (switch ((length captured-aggregations))
    (0 (values))
    (1 (aref captured-aggregations 0))
    (t (iterate
         (with result = '())
         (for elt in-vector captured-aggregations)
         (push elt result)
         (finally (return result))))))
