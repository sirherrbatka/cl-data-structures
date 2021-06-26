(cl:in-package #:cl-data-structures.queues)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (type fundamental-queue
    (:description "A fundamental base class of all queues."))

  (type fundamental-mutable-queue
    (:description "A fundamental base class of all mutable queues."))

  (type fundamental-functional-queue
    (:description "A fundamental base class of all functional queues.")))
