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

(let ((dict (cl-ds:make-from-traversable
             (mapcar (lambda (x) (cons x x)) '(147865 138799 129983 119199 102141 11920))
             'mutable-skip-list-dictionary
             #'> #'=)))
  (prove:is (cl-ds:size dict) 6)
  (prove:is (cl-ds.alg:count-elements (cl-ds:between* dict :low 102141 :high 0))
            2))

(prove:finalize)
