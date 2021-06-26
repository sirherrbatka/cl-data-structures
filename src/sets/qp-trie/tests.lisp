(cl:in-package #:cl-data-structures.sets.qp-trie)

(prove:plan 508)

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
     :test #'equalp)
    (prove:is
     (~> (cl-ds:between* trie :low (aref data reference-point))
         cl-ds.alg:to-vector
         (sort sort))
     (drop reference-point data)
     :test #'equalp)))


(bind ((trie (make-mutable-qp-trie-set))
       (sort (curry #'cl-ds.utils:lexicographic-compare #'< #'=))
       (data (vect))
       ((:flet point (&rest points))
        (vector-push-extend (map-into (make-array 4 :element-type '(unsigned-byte 8))
                                      #'identity
                                      points)
                            data)))
  (point 0 1 2)
  (point 0 1 3)
  (point 0 1 4)
  (point 0 1 5)
  (point 0 2 5)
  (point 0 3 5)
  (iterate
    (for point in-vector data)
    (cl-ds:put! trie point))
  (prove:is (cl-ds:size trie) 6)
  (prove:is (~> trie cl-ds.alg:to-vector)
            data
            :test #'equalp)
  (let ((range (cl-ds:between* trie :low (aref data 2))))
    (cl-ds:erase*! trie range)
    (prove:is (~> trie cl-ds.alg:to-vector
                  (sort sort))
              (~> (serapeum:take 2 data)
                  (sort sort))
              :test #'equalp)))


(prove:finalize)
