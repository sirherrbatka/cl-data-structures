(cl:in-package #:cl-data-structures.sets.qp-trie)

(prove:plan 504)

(let ((trie (make-mutable-qp-trie-set))
      (sort (curry #'cl-ds.utils:lexicographic-compare #'< #'=))
      (data (make-array 500)))
  (map-into data (lambda () (map-into (make-array 4 :element-type '(unsigned-byte 8))
                                      (lambda () (random #.(expt 2 8))))))
  (setf data (sort data sort))
  (iterate
    (for vect in-vector data)
    (cl-ds:put! trie vect))
  (iterate
    (for vect in-vector data)
    (prove:ok (cl-ds:at trie vect)))
  (prove:is (cl-ds.alg:count-elements trie)
            (~> data (remove-duplicates :test #'vector=) length))
  (prove:is (cl-ds:size trie) (cl-ds.alg:count-elements trie))
  (prove:is
   (~> trie cl-ds:whole-range cl-ds.alg:to-vector
       (sort sort))
   data
   :test #'equalp)
  (let ((reference-point (cl-ds.utils:lower-bound data (aref data 10) sort)))
    (prove:is
     (~> (cl-ds:between* trie :high (aref data reference-point))
         cl-ds.alg:to-vector
         (sort sort))
     (take reference-point data)
     :test #'equalp)))

(prove:finalize)
