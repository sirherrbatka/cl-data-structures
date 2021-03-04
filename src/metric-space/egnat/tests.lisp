(cl:in-package :cl-user)
(defpackage metric-egnat-tests
  (:use :prove :cl :iterate))
(cl:in-package :metric-egnat-tests)

(plan 45)

(defun levenshtein (str1 str2)
  (check-type str1 string)
  (check-type str2 string)
  (let ((n (length str1))
        (m (length str2)))
    (cond ((= 0 n) (return-from levenshtein m))
          ((= 0 m) (return-from levenshtein n)))
    (let ((col (make-array (1+ m) :element-type 'fixnum))
          (prev-col (make-array (1+ m) :element-type 'fixnum)))
      (iterate
        (for i from 0 below (1+ m))
        (setf (aref prev-col i) i))
      (iterate
        (for i below n)
        (setf (aref col 0) (1+ i))
        (iterate
          (for j below m)
          (setf (aref col (1+ j))
                (min (1+ (aref col j))
                     (1+ (aref prev-col (1+ j)))
                     (+ (aref prev-col j)
                        (if (char-equal (aref str1 i) (aref str2 j)) 0 1)))))
        (rotatef col prev-col))
      (aref prev-col m))))

(let* ((path (asdf:system-relative-pathname :cl-data-structures "test/files/words.txt"))
       (data (serapeum:vect))
       (count 0))
  (with-open-file (stream path)
    (iterate
      (for word = (read-line stream nil nil))
      (until (null word))
      (vector-push-extend word data)))
  (let* ((set (cl-ds:make-from-traversable
               data
               'cl-ds.ms.egnat:mutable-egnat-metric-set
               (lambda (a b)
                 (incf count)
                 (levenshtein a b))
               'non-negative-fixnum
               :branching-factor 50
               :parallel t
               :samples-count 5))
         (whole-content (cl-ds.alg:to-vector (cl-ds:whole-range set))))
    (is (length whole-content) (length data))
    (is (sort whole-content #'string<) (sort data #'string<)
        :test 'equalp)
    (setf count 0)
    (iterate
      (with near = (cl-ds:near set "rose" 1))
      (for n = (cl-ds:consume-front near))
      (while n)
      (for distance = (levenshtein n "rose"))
      (ok (<= distance 1))
      (count t into result)
      (finally (is result 40)))
    (ok (null (zerop count)))
    (ok (< count (length data)))))

(finalize)
