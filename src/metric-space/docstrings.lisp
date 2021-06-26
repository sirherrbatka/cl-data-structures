(cl:in-package #:cl-data-structures.metric-space)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (type metric-space-dictionary
    (:description "Fundamental class for all metric-space-dictionaries"))

  (type mutable-metric-space-dictionary
    (:description "Fundamental class for all mutable metric space dictionaries"))

  (type metric-space-set
    (:description "Fundamental class for all metric space sets."))

  (type mutable-metric-space-set
    (:description "Fundamental class for all mutable metric space sets.")))
