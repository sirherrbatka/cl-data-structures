(cl:in-package #:cl-ds.utils)


(defun or* (&rest functions)
  (lambda (&rest rest)
    (iterate
      (for function in functions)
      (for result = (apply function rest))
      (when result
        (leave result)))))


(defun and* (&rest functions)
  (lambda (&rest rest)
    (iterate
      (for function in functions)
      (for result = (apply function rest))
      (always result)
      (finally (return result)))))


(defun if-else (predicate true false)
  (ensure-functionf predicate true false)
  (lambda (&rest all)
    (if (apply predicate all)
        (apply true all)
        (apply false all))))


(defun cycle-over-address (dimensions &rest pinned)
  (bind ((address (make-array (length dimensions)
                              :element-type 'fixnum
                              :initial-element 0))
         (length (length dimensions))
         (skipped 0)
         (total-count 1)
         (result nil))
    (when (oddp (length pinned))
      (error "Passed odd number of arguments as dimensions to pin"))
    (iterate
      (with batches = (batches pinned 2))
      (for (axis position) in batches)
      (setf (ldb (byte 1 axis) skipped) 1)
      (setf (elt address axis) position)
      (finally (decf length (length batches))))
    (iterate
      (for i from 0)
      (for dim in dimensions)
      (unless (ldb-test (byte 1 i) skipped)
        (setf total-count (* total-count dim))))
    (setf result (coerce address 'list))
    (setf dimensions (coerce dimensions '(vector fixnum)))
    (lambda ()
      (labels ((cycle (pointer)
                 (iterate
                   (while (ldb-test (byte 1 pointer) skipped))
                   (incf pointer)
                   (while (< pointer length)))
                 (when (< pointer length)
                   (or (cycle (1+ pointer))
                       (when (< (1+ #1=(aref address pointer))
                                #2=(aref dimensions pointer))
                         (iterate
                           (for i from (1+ pointer) below length)
                           (unless (ldb-test (byte 1 i) skipped)
                             (setf (aref address i) 0)))
                         (incf #1#))))))
        (unless (zerop total-count)
          (unless (cycle 0))
          (decf total-count)
          (shiftf result (coerce address 'list)))))))


(defun ignore-errors* (function)
  (lambda (&rest all)
    (ignore-errors (apply function all))))


(defun generator (function initial-state)
  (ensure-functionf function)
  (lambda (&rest all)
    (declare (ignore all))
    (shiftf initial-state (funcall function initial-state))))


(defun prevent-duplicates (&key (test 'eql) (key #'identity))
  (let ((table (make-hash-table :test test)))
    (lambda (value &aux (data (funcall key value)))
      (ensure (gethash data table) data))))
