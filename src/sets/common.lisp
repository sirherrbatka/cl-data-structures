(cl:in-package #:cl-data-structures.sets)


(defclass fundamental-set (cl-ds:fundamental-container)
  ())


(defclass mutable-set (fundamental-set cl-ds:mutable)
  ())


(defmethod cl-ds:put! ((container mutable-set) item)
  (cl-ds.meta:position-modification #'cl-ds:put!
                                    container
                                    container
                                    item))


(defmethod cl-ds:erase! ((container mutable-set) location)
  (cl-ds.meta:position-modification #'cl-ds:erase!
                                    container
                                    container
                                    location))

(defmethod cl-ds:erase*! ((container mutable-set) range)
  (cl-ds.meta:position-modification #'cl-ds:erase!*
                                    container
                                    container
                                    range))
