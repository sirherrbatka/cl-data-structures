(cl:in-package #:cl-user)


(defpackage :cl-data-structures.utils
  (:use #:common-lisp #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.utils)
  (:export
   #:add-into-queue
   #:add-sinks
   #:add-to-list
   #:adjust-size-to-fill-pointer
   #:all-parents
   #:ancestor-p
   #:and*
   #:as-cons-tree
   #:binary-search
   #:bind-lambda
   #:bucket-sort
   #:cartesian
   #:cartesian-table
   #:cases
   #:clone
   #:cloning-information
   #:cloning-list
   #:cond+
   #:cond-compare
   #:copy-slots
   #:copy-without
   #:cycle-over-address
   #:define-list-of-slots
   #:draw-random-vector
   #:draw-sample-vector
   #:each-in-matrix
   #:end-execution
   #:ensure-call-ahead-of
   #:erase-from-vector
   #:extendable-vector
   #:fill-distance-matrix-from-vector
   #:fixnum-hash
   #:future-carousel
   #:generator
   #:check-value
   #:half-matrix
   #:hash-integer
   #:homogenousp
   #:if-else
   #:square
   #:ignore-errors*
   #:import-all-package-symbols
   #:index
   #:insert-or-replace
   #:inverted-hash-table
   #:lambda-list-to-bindings
   #:lambda-list-to-call-form
   #:lazy-let
   #:lazy-shuffle
   #:let-generator
   #:lexicographic-compare
   #:lolol
   #:lower-bound
   #:lparallel-future
   #:make-distance-matrix-from-vector
   #:make-future-carousel
   #:make-half-matrix
   #:make-new-skip-vector
   #:make-pipe-fragment
   #:merge-ordered-vectors
   #:method-lambda-list-to-function-lambda-list
   #:mref
   #:mutate-matrix
   #:normalize-sequence-to-span
   #:normalize-sequence-to-sum
   #:on-ordered-intersection
   #:optimize-value
   #:or*
   #:ordered-exclusion
   #:ordered-intersection
   #:ordered-p
   #:parallel-fill-distance-matrix-from-vector
   #:parallel-make-distance-matrix-from-vector
   #:pipe-fragment
   #:pop-last
   #:prevent-duplicates
   #:quasi-clone
   #:quasi-clone*
   #:quasi-other-class
   #:quasi-other-class*
   #:read-size
   #:rebind
   #:remove-fill-pointer
   #:rol64
   #:scan
   #:select-top
   #:skip-vector-without
   #:start-execution
   #:swap-if
   #:swapop
   #:todo
   #:transform
   #:try-find
   #:try-find-cell
   #:try-remove
   #:unfold-table
   #:with-keys
   #:with-rebind
   #:with-slots-for
   #:with-vectors
   #:xorshift))
