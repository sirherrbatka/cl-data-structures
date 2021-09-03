(cl:in-package #:cl-ds.dicts.skip-list)

(prove:plan 13)

(let ((dict (cl-ds:make-from-traversable
             '((1 . 1) (2 . 2) (3 . 3) (4 . 4) (5 . 5))
             'mutable-skip-list-dictionary
             #'< #'=)))
  (prove:is (cl-ds:size dict) 5)
  (iterate
    (for i from 1 to 5)
    (prove:is (cl-ds:at dict i) i))
  (iterate
    (for i from 1 to 5)
    (cl-ds:update! dict i (1+ i)))
  (iterate
    (for i from 1 to 5)
    (prove:is (cl-ds:at dict i) (1+ i)))
  (prove:is (~> dict cl-ds.alg:to-list)
            '((1 . 2) (2 . 3) (3 . 4) (4 . 5) (5 . 6))
            :test #'equal)
  (prove:is (cl-ds:lower-bound dict 3) 3))

(prove:finalize)
