(cl:in-package #:cl-ds.utils)


(defclass half-matrix ()
  ((%size :type fixnum
          :reader read-size
          :initarg :size)
   (%content :type simple-array
             :reader read-content
             :initarg :content)
   (%key :type function
         :reader read-key
         :initarg :key
         :initform #'identity)))


(defgeneric mutate-matrix (destination fn m1 &rest more)
  (:method ((destination half-matrix) fn m1 &rest more)
    (let ((dest (read-content destination))
          (more (cons m1 more)))
      (unless (every (compose (curry #'eql (read-size destination))
                              #'read-size)
                     more)
        (error "Sizes don't mach!"))
      (apply #'map-into dest fn (mapcar #'read-content more))
      destination)))


(declaim (inline index-in-content-of-distance-matrix))
(-> index-in-content-of-distance-matrix (positive-fixnum index index) index)
(defun index-in-content-of-distance-matrix (size row column)
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (the index (+ (- (* size row)
                   (/ (* row (1+ row))
                      2)
                   1
                   row)
                column)))


(defun make-half-matrix (type size &key (query-key #'identity) (initial-element 0 initial-element-bound-p))
  (let ((result (if initial-element-bound-p
                    (make-array (1+ (index-in-content-of-distance-matrix size
                                                                         (1- size)
                                                                         (1- size)))
                                :initial-element initial-element
                                :element-type type)
                    (make-array (1+ (index-in-content-of-distance-matrix size
                                                                         (1- size)
                                                                         (1- size)))
                                :element-type type))))
    (assure half-matrix
      (make 'half-matrix
            :size size
            :key query-key
            :content result))))


(defun parallel-fill-distance-matrix-from-vector (matrix function sequence
                                                  &key
                                                    (key #'identity)
                                                    (key-context #'identity)
                                                    (function-context #'identity))
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0))
           (type half-matrix matrix)
           (type sequence sequence)
           (type function function))
  (assert (<= (length sequence) (read-size matrix)))
  (let* ((size (length sequence))
         (content (read-content matrix))
         (type (array-element-type content))
         (indexes (iterate
                    (for i below (length sequence))
                    (collect i into forward at start)
                    (collect i into backward)
                    (finally (return (map '(vector list) #'list backward forward))))))
    (declare (type vector indexes))
    (lparallel:pmap
     nil
     (lambda (index)
       (fbind ((dist-function (funcall function-context function))
               (key-function (funcall key-context key)))
         (iterate
           (for i in index)
           (for x = (aref sequence i))
           (iterate
             (for j from (1+ i) below size)
             (for y = (aref sequence j))
             (setf (aref content (index-in-content-of-distance-matrix size i j))
                   (coerce (dist-function (key-function x) (key-function y))
                           type))))))
     indexes)
    matrix))


(-> parallel-make-distance-matrix-from-vector ((or list symbol)
                                               (or function symbol)
                                               vector
                                               &key
                                               (:key (-> (t) t))
                                               (:query-key (-> (t) t))
                                               (:key-context (-> (t) t))
                                               (:function-context (-> (t) t)))
    half-matrix)
(defun parallel-make-distance-matrix-from-vector (type function sequence
                                                  &key
                                                    (key #'identity)
                                                    (key-context #'identity)
                                                    (function-context #'identity)
                                                    (query-key #'identity))
  (declare (optimize (speed 3)))
  (let ((result (make-half-matrix type (length sequence))))
    (parallel-fill-distance-matrix-from-vector result function sequence
                                               :key key
                                               :key-context key-context
                                               :function-context function-context)
    (setf (slot-value result '%key) query-key)
    result))


(defun fill-distance-matrix-from-vector (matrix function sequence &key (key #'identity))
  (assert (<= (length sequence) (read-size matrix)))
  (let* ((size (length sequence))
         (content (read-content matrix))
         (type (array-element-type content)))
    (labels ((fill-matrix (column)
               (iterate
                 (with first = (funcall key (aref sequence column)))
                 (for row from (1+ column) below size)
                 (for second = (funcall key (aref sequence row)))
                 (setf (aref content (index-in-content-of-distance-matrix size
                                                                          column
                                                                          row))
                       (coerce (funcall function first second) type)))))
      (iterate
        (for i from 0 below size)
        (fill-matrix i))
      matrix)))


(-> make-distance-matrix-from-vector ((or list symbol)
                                      (-> (t t) t)
                                      vector
                                      &key
                                      (:key (-> (t) t))
                                      (:initial-element t)
                                      (:query-key (-> (t) t)))
    half-matrix)
(defun make-distance-matrix-from-vector (type function sequence
                                         &key
                                           (key #'identity)
                                           (query-key #'identity)
                                           (initial-element 0))
  (when (< (length sequence) 2)
    (error "Can't create distance matrix for vector of size ~a" (length sequence)))
  (let ((result (make-half-matrix type (length sequence) :initial-element initial-element)))
    (fill-distance-matrix-from-vector result function sequence :key key)
    (setf (slot-value result '%key) query-key)
    result))


(defun mref (matrix from to)
  (declare (type index from to)
           (type half-matrix matrix)
           (optimize (speed 3)
                     (safety 0)
                     (space 0)
                     (debug 0)))
  (let* ((size (slot-value matrix '%size))
         (key (slot-value matrix '%key))
         (from (funcall key from))
         (to (funcall key to)))
    (declare (type index size to from)
             (type function key))
    (cond
      ((or (>= from size) (>= to size) (eql to from))
       (error "No such position in the matrix."))
      (t (let ((content (slot-value matrix '%content))
               (from (min from to))
               (to (max from to)))
           (declare (type simple-array content))
           (aref content (index-in-content-of-distance-matrix size from to)))))))


(defun distance (matrix from to)
  (mref matrix from to))


(defun (setf mref) (value matrix from to)
  (declare (type index from to)
           (type half-matrix matrix)
           (optimize (speed 3) (safety 0)
                     (space 0) (debug 0)))
  (let* ((size (slot-value matrix '%size))
         (key (slot-value matrix '%key))
         (to (funcall key to))
         (from (funcall key from)))
    (declare (type index size))
    (cond ((or (>= from size) (>= to size))
           (error "No such position in the matrix!"))
          ((eql from to)
           (error "Can't set dinstance to self"))
          (t (let ((content (slot-value matrix '%content))
                   (from (min from to))
                   (to (max from to)))
               (declare (type simple-array content))
               (setf
                (aref content (index-in-content-of-distance-matrix size from to))
                value))))))


(defun (setf distance) (value matrix from to)
  (setf (mref matrix from to) value))


(defgeneric each-in-matrix (fn matrix)
  (:method (fn (matrix half-matrix))
    (declare (type (-> (index index single-float) t) fn))
    (with-vectors ((content (slot-value matrix '%content)))
      (iterate
        (with size = (slot-value matrix '%size))
        (with index = 0)
        (for i from 0 below size)
        (iterate
          (for j from (1+ i) below size)
          (funcall fn i j (content index))
          (incf index)))
      matrix)))
