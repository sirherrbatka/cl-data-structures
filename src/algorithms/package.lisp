(in-package #:cl-user)


(defpackage :cl-data-structures.algorithms
  (:use #:common-lisp #:docstample #:docstample.mechanics #:serapeum
        #:cl-ds.utils #:alexandria #:iterate #:cl-ds)
  (:shadowing-import-from #:iterate #:collecting #:summing #:in)
  (:nicknames #:cl-ds.alg)
  (:export
   #:accumulate
   #:aggregate
   #:aggregation-function
   #:apply-aggregation-function
   #:apply-layer
   #:apply-range-function
   #:change-each!
   #:gather-prior-states
   #:group-by
   #:group-by-function
   #:layer-function
   #:make-state
   #:multi-aggregation-function
   #:multi-aggregation-stages
   #:on-each
   #:proxy-box-range
   #:proxy-range
   #:range-function
   #:state-result
   #:summary
   #:summary-function
   #:transformation!-function))
