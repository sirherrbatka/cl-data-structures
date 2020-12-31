(cl:in-package #:cl-ds.utils)


(defun ordered-p (sequence fn &key (key #'identity))
  "Predictate. Checks if SEQUENCE is ordered by FN (that is, if call to (fn prev current) always returns T).

  @b(Side Effects:) none. "
  (iterate
    (for s in-sequence sequence)
    (for elt = (funcall key s))
    (for pelt previous elt)
    (if-first-time
     t
     (always (funcall fn pelt elt)))))


(-> merge-ordered-vectors ((or vector null) (-> (t t) boolean) vector &rest vector)
    (values vector index))
(defun merge-ordered-vectors (result compare-fn vector &rest vectors)
  "Take few ordered vectors (by function compare-fn) and create new vector of element-type RESULT-TYPE by copying values from vectors in such way that it is also ordered. Return new vector.

 @b(Arguments and values:)
 @begin(list)
 @item(result-type -- element-type of result vector)
 @item(compare-fn -- function that describes ordering of vectors)
 @item(vector) -- ordered vector that is about to be merged
 @item(vectors -- more ordered vectors that are about to be merged)
 @end(list)

 @b(Side effects:) none."
  (let* ((vectors (cons vector vectors))
         (position 0)
         (result (if (null result)
                     (make-array (reduce #'+ vectors :key #'length))
                     result))
         (index.vector (make-array 0
                                   :element-type 'list
                                   :adjustable t
                                   :fill-pointer 0)))
    (map nil
         (lambda (x) (unless (zerop (array-dimension x 0))
                       (vector-push-extend (list* 0 x)
                                           index.vector)))
         vectors)
    (iterate
      (while (< position (length result)))
      (for item = (iterate
                    (for current index-of-vector index.vector)
                    (for (index . vector) = (aref index.vector current))
                    (for value = (aref vector index))
                    (for minimum first (list* value current)
                         then (if (funcall compare-fn value (car minimum))
                                  (list* value current)
                                  minimum))
                    (finally (return minimum))))
      (while item)
      (for (minimum . index) = item)
      (setf (aref result position) minimum)
      (incf position)
      (for current = (aref index.vector index))
      (when  (= (incf (car current))
                (array-dimension (cdr current)
                                 0))
        (swapop index.vector index)))
    (values result position)))


(declaim (inline lower-bound))
(-> lower-bound (vector t (-> (t t) t) &key (:key function) (:start non-negative-fixnum) (:end non-negative-fixnum))
    index)
(defun lower-bound (vector element comparsion &key (key #'identity) (start 0) (end (length vector)))
  (declare (optimize (speed 3)))
  (iterate
    (declare (type fixnum current))
    (for current = (+ start (truncate (- end start) 2)))
    (until (eql end start))
    (if (funcall comparsion
                 (funcall key (aref vector current))
                 element)
        (setf start (the index (1+ current)))
        (setf end (the index current)))
    (finally (return current))))


(declaim (inline binary-search))
(-> binary-search (t vector (-> (t t) t) (-> (t t) t)
                     &key (:key function) (:start fixnum) (:end integer))
    (values t boolean))
(defun binary-search (element vector lower test
                      &key (key #'identity) (start 0) (end (length vector)))
  (let ((lower-bound (lower-bound vector element lower
                                  :key key
                                  :start start
                                  :end end)))
    (if (and (not (eql lower-bound end))
             (funcall test element (funcall key (aref vector lower-bound))))
        (values (aref vector lower-bound) t)
        (values nil nil))))


(defun on-ordered-intersection (function first-order second-order
                                &key
                                  (on-first-missing #'identity)
                                  (on-second-missing #'identity)
                                  (key #'identity)
                                  (first-key key)
                                  (second-key key)
                                  (same #'=)
                                  (less #'<))
  (declare (optimize (speed 3) (safety 1)))
  (check-type first-order vector)
  (check-type second-order vector)
  (cases ((simple-vector-p first-order)
          (simple-vector-p second-order)
          (:variant
           (eq same #'=)
           (eq same #'eql))
          (:variant
           (eq less #'<)
           (eq less #'>))
          (:variant
           (typep first-order '(vector fixnum))
           (typep first-order '(vector non-negative-fixnum)))
          (:variant
           (typep second-order '(vector non-negative-fixnum))
           (typep second-order '(vector fixnum))))
    (with-vectors (first-order second-order)
      (iterate
        (declare (type fixnum a b first-length second-length))
        (with a = 0)
        (with b = 0)
        (with first-length = (length first-order))
        (with second-length = (length second-order))
        (while (< a first-length))
        (while (< b second-length))
        (for av = (funcall first-key (first-order a)))
        (for bv = (funcall second-key (second-order b)))
        (cond ((funcall same av bv)
               (funcall function (first-order a) (second-order b))
               (incf a)
               (incf b))
              ((funcall less av bv)
               (funcall on-second-missing (first-order a))
               (incf a))
              (t
               (funcall on-first-missing (second-order b))
               (incf b)))
        (finally
         (iterate
           (declare (type fixnum i))
           (for i from a below (length first-order))
           (funcall on-second-missing (first-order i)))
         (iterate
           (declare (type fixnum i))
           (for i from b below (length second-order))
           (funcall on-first-missing (second-order i))))))))


(defun ordered-intersection (compare-fn test-fn vector &rest more-vectors)
  (push vector more-vectors)
  (iterate
    (with indexes = (map '(vector fixnum) (constantly 0) more-vectors))
    (with sizes = (map '(vector fixnum) #'length more-vectors))
    (with vector-type = (iterate
                          (for vector in (rest more-vectors))
                          (for prev-vector
                               previous vector
                               initially (first more-vectors))
                          (unless (equal (array-element-type vector)
                                         (array-element-type prev-vector))
                            (leave t))
                          (finally (return (array-element-type prev-vector)))))
    (with result = (make-array (reduce #'min more-vectors :key #'length)
                               :element-type vector-type
                               :adjustable t
                               :fill-pointer 0))
    (while (every #'< indexes sizes))
    (iterate
      (with ok = t)
      (with minimum = nil)
      (with minimum-i = 0)
      (for i from 0)
      (for index in-vector indexes)
      (for vector in more-vectors)
      (for value = (aref vector index))
      (for prev-value previous value)
      (if (first-iteration-p)
          (setf minimum value)
          (progn
            (when ok
              (setf ok (funcall test-fn value prev-value)))
            (when (and (not ok) (funcall compare-fn value minimum))
              (setf minimum value
                    minimum-i i))))
      (finally (if ok
                   (progn
                     (map-into indexes #'1+ indexes)
                     (vector-push-extend minimum result))
                   (incf (aref indexes minimum-i)))))
    (finally (return (adjust-array result
                                   (fill-pointer result))))))


(defun ordered-exclusion (compare-fn test-fn vector &rest more-vectors)
  (iterate
    (with indexes = (map '(vector fixnum) (constantly 0) more-vectors))
    (with vector-type = (iterate
                          (for vector in (rest more-vectors))
                          (for prev-vector
                               previous vector
                               initially (first more-vectors))
                          (unless (equal (array-element-type vector)
                                         (array-element-type prev-vector))
                            (leave t))
                          (finally (return (array-element-type prev-vector)))))
    (with result = (make-array (reduce #'min more-vectors :key #'length)
                               :element-type vector-type
                               :adjustable t
                               :fill-pointer 0))
    (for v in-vector vector)
    (map-into indexes
              (lambda (vector)
                (lower-bound vector v compare-fn))
              more-vectors)
    (for not-present =
         (iterate
           (for other-vector in more-vectors)
           (for index in-vector indexes)
           (always (or (eql index (length other-vector))
                       (let ((data (aref other-vector index)))
                         (not (funcall test-fn data v)))))))
    (when not-present
      (vector-push-extend v result))
    (finally (return (adjust-array result
                                   (fill-pointer result))))))
