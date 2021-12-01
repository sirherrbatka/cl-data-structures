(cl:in-package #:cl-ds.sa)


(prove:plan 8)

(let ((histogram (make 'approximated-histogram)))
  (let ((count-lower (first (approximated-histogram-count-lower histogram 30))))
    (prove:is count-lower 0.0d0))
  (iterate
    (for i from 1 below 100)
    (approximated-histogram-add histogram (coerce i 'double-float)))
  (let ((count-lower (first (approximated-histogram-count-lower histogram 30))))
    (prove:ok (< 29 count-lower 30)))
  (let ((median (first (approximated-histogram-quantile histogram 0.5))))
    (prove:ok (< 49 median 51)))
  (let ((high (first (approximated-histogram-quantile histogram 0.95))))
    (prove:ok (< 94 high 96)))
  (bind (((median high) (approximated-histogram-quantile histogram 0.5 0.95)))
    (prove:ok (< 49 median 51))
    (prove:ok (< 94 high 96)))
  (prove:is (access-count (union histogram histogram))
            (* 2 (access-count histogram)))
  (prove:ok (< 0.28
               (first (approximated-histogram-rank-order histogram 30))
               0.31)))

(prove:finalize)
