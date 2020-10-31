(cl:in-package #:cl-data-structures.sets.skip-list)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function make-mutable-skip-list-set
        (:description "Constructs and returns a new instance of mutable-skip-list."))

  (type mutable-skip-list-set
        (:description "Mutable skip list set.")))
