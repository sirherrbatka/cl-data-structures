(cl:in-package #:cl-data-structures.sets.qp-trie)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function make-mutable-qp-trie-set
            (:description "Constructs and returns a new instance of the mutable-qp-trie-set"))

  (type mutable-qp-trie-set
         (:description "Mutable variant of the mutable-qp-trie-set.")))
