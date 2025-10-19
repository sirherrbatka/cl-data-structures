(cl:defpackage :cl-data-structures.streaming-algorithms
  (:use #:common-lisp #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.sa)
  (:shadow cl:union)
  (:local-nicknames
   (#:hll #:cl-data-structures.streaming-algorithms.hyperloglog)
   (#:ph #:cl-data-structures.streaming-algorithms.polynomial-hashing))
  (:export
   #:approximated-counts
   #:approximated-counts-distance
   #:approximated-histogram
   #:approximated-histogram-add
   #:approximated-histogram-count
   #:approximated-histogram-bin-count
   #:approximated-histogram-bin-position
   #:approximated-histogram-bin-sum
   #:approximated-histogram-bin-value
   #:approximated-histogram-bins
   #:approximated-histogram-bounds
   #:approximated-histogram-count-lower
   #:approximated-histogram-count-rank-order
   #:approximated-histogram-cumulant-sum
   #:approximated-histogram-truncated-mean
   #:approximated-histogram-mean
   #:approximated-histogram-trim
   #:approximated-histogram-median
   #:approximated-histogram-quantile
   #:approximated-histogram-rank-order
   #:approximated-histogram-sum
   #:approximated-histogram-values
   #:approximated-histogram-counts
   #:approximated-histogram-variance
   #:approximated-histogram-mode
   #:approximated-histogram-standard-deviation
   #:approximated-set-cardinality
   #:approximated-top-k
   #:bloom-filter
   #:bloom-filter-jaccard
   #:clean-sketch
   #:fundamental-data-sketch
   #:gather-minhash-corpus
   #:hyperloglog-jaccard
   #:internal-array
   #:make-approximated-histogram
   #:make-hash-array
   #:make-minhash
   #:make-one-bit-minhash
   #:minhash
   #:minhash-jaccard/double-float
   #:approximated-set-cardinality-add
   #:simhash
   #:simhash-distance
   #:minhash-jaccard/fixnum
   #:minhash-jaccard/single-float
   #:one-bit-minhash-jaccard/double-float
   #:one-bit-minhash-jaccard/fixnum
   #:one-bit-minhash-jaccard/single-float
   #:union))
