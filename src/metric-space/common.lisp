(cl:in-package #:cl-data-structures.metric-space)


(defgeneric distance (container bucket element)
  (:method ((container metric-space-set)
            (bucket t)
            (element t))
    (funcall (read-metric-fn container) bucket element)))
