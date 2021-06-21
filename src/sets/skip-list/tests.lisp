(cl:in-package :cl-user)
(defpackage skip-list-set-tests
  (:use :cl :cl-ds :cl-data-structures.aux-package))
(cl:in-package :skip-list-set-tests)


(prove:plan 624)

(let ((set (cl-ds.sets.skip-list:make-mutable-skip-list-set #'< #'=)))
  (prove:ok (not (cl-ds:at set 1)))
  (prove:is (cl-ds:size set) 0)
  (cl-ds:put! set 1)
  (prove:is (cl-ds:size set) 1)
  (prove:ok (cl-ds:at set 1))
  (prove:ok (not (cl-ds:at set 0))))

(iterate
  (with set = (cl-ds.sets.skip-list:make-mutable-skip-list-set #'< #'=))
  (with data = (iota 15))
  (for i from 0)
  (for d on data)
  (iterate
    (for k on data)
    (until (eq d k))
    (prove:ok (cl-ds:at set (first k))))
  (prove:is (cl-ds:size set) i)
  (cl-ds:put! set (first d))
  )

(iterate
  (with set = (cl-ds.sets.skip-list:make-mutable-skip-list-set #'< #'=))
  (with data = (reverse (iota 15)))
  (for i from 0)
  (for d on data)
  (iterate
    (for k on data)
    (until (eq d k))
    (prove:ok (cl-ds:at set (first k))))
  (prove:is (cl-ds:size set) i)
  (cl-ds:put! set (first d))
  )

(let ((set (cl-ds.sets.skip-list:make-mutable-skip-list-set #'< #'=))
      (data (shuffle (iota 15))))
  (iterate
    (for i from 0)
    (for d on data)
    (iterate
      (for k on data)
      (until (eq d k))
      (prove:ok (cl-ds:at set (first k))))
    (prove:is (cl-ds:size set) i)
    (cl-ds:put! set (first d)))
  (iterate
    (for i from 15 downto 0)
    (for d on data)
    (prove:is (cl-ds:size set) i)
    (prove:ok (cl-ds:at set (first d)))
    (cl-ds:erase! set (first d))
    (iterate
      (for e on data)
      (prove:is (cl-ds:at set (car e)) nil)
      (until (eq d e)))
    (iterate
      (for elt in (rest d))
      (prove:ok (cl-ds:at set elt)))))

(let* ((set (cl-ds:make-from-traversable (iota 15)
                                         'cl-ds.sets.skip-list:mutable-skip-list-set
                                         #'< #'=))
       (below-range (cl-ds:between* set :high 7))
       (below-7 (cl-ds.alg:to-list below-range)))
  (prove:is below-7 '(0 1 2 3 4 5 6) :test 'equal)
  (cl-ds:erase*! set below-range)
  (prove:is (~> set cl-ds.alg:to-list) '(7 8 9 10 11 12 13 14) :test 'equal))

(let* ((set (cl-ds:make-from-traversable (iota 1000)
                                         'cl-ds.sets.skip-list:mutable-skip-list-set
                                         #'< #'=))
       (between-range (cl-ds:between* set :low 250 :high 500))
       (between (cl-ds.alg:to-list between-range)))
  (prove:is between (iota 250 :start 250) :test 'equal)
  (cl-ds:erase*! set between-range)
  (prove:is (~> set cl-ds.alg:to-list)
            (append (iota 250) (iota 500 :start 500))
            :test 'equal))

(prove:finalize)
