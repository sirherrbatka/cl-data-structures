(cl:defpackage :cl-data-structures.metric-space
  (:use #:common-lisp
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.ms)
  (:export
   #:distance
   #:metric-space-dictionary
   #:metric-space-set
   #:mutable-metric-space-dictionary
   #:mutable-metric-space-set))


(cl:defpackage :cl-data-structures.metric-space.egnat
  (:use #:common-lisp
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.ms.egnat)
  (:export
   #:make-mutable-egnat-metric-set
   #:mutable-egnat-metric-set
   #:egnat-metric-set))
