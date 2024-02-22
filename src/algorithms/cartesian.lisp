(cl:in-package #:cl-data-structures.algorithms)


(defclass forward-cartesian-range (cl-ds:chunking-mixin
                                   cl-ds:fundamental-forward-range)
  ((%content :initarg :content
             :type vector
             :reader read-content)
   (%function :initarg :function
              :type function
              :reader read-function)))


(defun deep-copy-of-content (range)
  (~>> range read-content
       (map 'vector #'cl-ds:clone)))


(defmethod cl-ds.utils:cloning-information append
    ((range forward-cartesian-range))
  '((:content deep-copy-of-content)
    (:function read-function)))


(defmethod cl-ds:reset! ((range forward-cartesian-range))
  (map nil #'cl-ds:reset! (read-content range))
  range)


(defmethod cl-ds:clone ((range forward-cartesian-range))
  (cl-ds.utils:clone range))


(defmethod cl-ds:consume-front ((range forward-cartesian-range))
  (bind ((content (read-content range))
         (data (map 'list
                    (lambda (x) (multiple-value-list (cl-ds:peek-front x)))
                    content))
         (more (every #'second data)))
    (if more
        (let ((result (apply (read-function range) (mapcar #'first data))))
          (iterate
            (for i from 0 below (length content))
            (for c in-vector content)
            (cl-ds:consume-front c)
            (when (nth-value 1 (cl-ds:peek-front c))
              (iterate
                (for j from 0 below i)
                (for c in-vector content)
                (cl-ds:reset! c))
              (finish)))
          (values result t))
        (values nil nil))))


(defmethod cl-ds:peek-front ((range forward-cartesian-range))
  (bind ((content (read-content range))
         (data (map 'list
                    (lambda (x) (multiple-value-list (cl-ds:peek-front x)))
                    content))
         (more (every #'second data)))
    (if more
        (values (apply (read-function range) (mapcar #'first data))
                t)
        (values nil nil))))


(defun cartesian (function range &rest more-ranges)
  (declare (dynamic-extent more-ranges))
  (ensure-functionf function)
  (make 'forward-cartesian-range
        :function function
        :content (~>> (cons range more-ranges)
                      (map 'vector
                           (lambda (x)
                             (etypecase x
                               (cl-ds:fundamental-forward-range (cl-ds:clone x))
                               (cl-ds:fundamental-container (cl-ds:whole-range x))
                               (cl:sequence (cl-ds:whole-range x))))))))


(defun cartesian-array (function element-type range &rest more-ranges)
  (declare (dynamic-extent more-ranges))
  (bind ((ranges-count (~> more-ranges length 1+))
         (vector (~> (apply #'cartesian function range more-ranges)
                     cl-ds.alg:to-vector))
         (length (length vector))
         (edge-length (truncate (expt length (/ 1 ranges-count))))
         (result (make-array (make-list ranges-count :initial-element edge-length)
                             :element-type element-type)))
    (iterate
      (for i from 0 below length)
      (setf (row-major-aref result i) (aref vector i)))
    result))
