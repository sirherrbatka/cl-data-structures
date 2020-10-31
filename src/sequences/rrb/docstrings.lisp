(cl:in-package #:cl-data-structures.sequences.rrb-vector)

(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (type functional-rrb-vector
    (:description "Functional variant of the RRB vector."))

  (type mutable-rrb-vector
    (:description "Mutable variant of the RRB vector."))

  (type transactional-rrb-vector
    (:description "Transactional variant of the RRB vector."))

  (function make-transactional-rrb-vector
    (:description "Creates and returns a new instance of transactional-rrb-vector"))

  (function make-functional-rrb-vector
    (:description "Creates and returns a new instance of functiona-rrb-vector"))

  (function make-mutable-rrb-vector
    (:description "Creates and returns a new instance of mutable-rrb-vector.")))
