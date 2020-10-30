(cl:in-package #:cl-data-structures.queues.2-3-tree)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function make-functional-2-3-queue
            (:returns "Empty functional-2-3-queue"))

  (function make-mutable-2-3-queue
            (:returns "Empty mutable-2-3-queue"))

  (function make-transactional-2-3-queue
            (:returns "Empty transactional-2-3-queue"))

  (type functional-2-3-queue
         (:description "An functional queue based around 2-3 tree data structur."))

  (type mutable-2-3-queue
         (:description "An mutable queue based around 2-3 tree data structur."))

  (type transactional-2-3-queue
         (:description "An transactional queue based around 2-3 tree data structure.")))
