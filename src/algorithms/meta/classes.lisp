(cl:in-package #:cl-data-structures.algorithms.meta)


(defclass range-function (closer-mop:standard-generic-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass layer-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass transformation!-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass aggregation-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(define-condition early-aggregation-exit (condition)
  ())
