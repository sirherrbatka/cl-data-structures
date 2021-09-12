(cl:defpackage :cl-data-structures.composite
  (:use #:common-lisp
        #:cl-data-structures.aux-package)
  (:nicknames #:cl-ds.composite)
  (:export
   :position-modification
   :position-modification!
   :make-mutable-composite-container
   :make-functional-composite-container))
