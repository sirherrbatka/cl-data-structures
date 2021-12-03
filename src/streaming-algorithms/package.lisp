(cl:in-package #:cl-user)


(defpackage :cl-data-structures.streaming-algorithms
  (:use #:common-lisp #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.sa)
  (:shadow cl:union)
  (:local-nicknames
   (#:hll #:cl-data-structures.streaming-algorithms.hyperloglog)
   (#:ph #:cl-data-structures.streaming-algorithms.polynomial-hashing))
  (:export
   #:approximated-counts
   #:approximated-counts-distance
   #:approximated-set-cardinality
   #:approximated-top-k
   #:bloom-filter
   #:clean-sketch
   #:fundamental-data-sketch
   #:gather-minhash-corpus
   #:hyperloglog-jaccard
   #:make-hash-array
   #:make-minhash
   #:make-one-bit-minhash
   #:minhash
   #:internal-array
   #:minhash-jaccard/double-float
   #:minhash-jaccard/fixnum
   #:minhash-jaccard/single-float
   #:one-bit-minhash-jaccard/double-float
   #:one-bit-minhash-jaccard/fixnum
   #:one-bit-minhash-jaccard/single-float
   #:union
   #:approximated-histogram
   #:approximated-histogram-add
   #:approximated-histogram-mean
   #:approximated-histogram-median
   #:approximated-histogram-bin-other-sums
   #:approximated-histogram-bounds
   #:approximated-histogram-bin-position
   #:approximated-histogram-cumulant-sum
   #:approximated-histogram-values
   #:approximated-histogram-bins
   #:approximated-histogram-bin-sum
   #:approximated-histogram-bin-value
   #:approximated-histogram-bin-count
   #:approximated-histogram-quantile
   #:approximated-histogram-variance
   #:approximated-histogram-sum
   #:approximated-histogram-add
   #:approximated-histogram-count-lower
   #:approximated-histogram-count-rank-order))
