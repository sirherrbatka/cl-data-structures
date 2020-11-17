(cl:in-package #:cl-data-structures.utils.distance)


(-> bhattacharyya-distance ((vector single-float) (vector single-float))
    single-float)
(defun bhattacharyya-distance (h1 h2)
  (let* ((mean1 (mean h1))
         (mean2 (mean h2))
         (score (iterate
                  (for a in-vector h1)
                  (for b in-vector h2)
                  (sum (sqrt (* a b))))))
    (~>> (* mean1 mean2 (length h1) (length h2))
         sqrt
         (/ 1)
         (* score)
         (- 1)
         sqrt
         (coerce _ 'single-float))))
