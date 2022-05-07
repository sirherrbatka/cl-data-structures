(ql:quickload :cl-lore)
(cl:defpackage :cl-data-structures.documentation
  (:use #:cl #:cl-lore
        #:cl-lore.api.syntax
        #:cl-lore.extensions.documentation.api
        #:cl-lore.extensions.sequence-graphs.api)
  (:export #:build-docs))

(cl:in-package #:cl-data-structures.documentation)


(def-chunks *cl-data-structures*)
(setf documentation-utils-extensions:*documentation* (documentation-utils-extensions:make-documentation-collection))
(ql:quickload :cl-data-structures)

(cl-lore.api.syntax:syntax
 cl-lore.extensions.documentation.api
 cl-lore.extensions.sequence-graphs.api)

(cl-lore.api.syntax:define-save-output-function
    build-docs
    (:cl-data-structures.documentation
     (<documentation-names>)
     cl-lore.mechanics:<mechanics-html-output-generator>
     *cl-data-structures*)
    (:output-options (:css cl-lore.mechanics:*mechanics-html-style*))

  ("vars.lisp"
   "key-concepts.lore"
   "conventions.lore"
   "introduction.lore"
   "dicts.lore"
   "sets.lore"
   "sequences.lore"
   "manual.lore"
   "queues.lore"
   "in-depth.lore")

  (title "CL-DATA-STRUCTURES")
  (include "cl-ds intro")
  (include "cl-ds API")
  (include "dicts")
  (include "sets")
  (include "queues")
  (include "sequences")
  (include "cl-ds algorithms")
  (include "cl-ds file system")
  (include "cl-ds math")
  (include "cl-ds threads")
  (include "cl-ds streaming")
  (include "cl-ds internals")
  )

(build-docs "/home/shka/lore")
