(cl:in-package #:cl-data-structures.dicts)


(defmethod cl-ds:add ((container functional-dictionary) location new-value)
  (cl-ds.meta:position-modification #'cl-ds:add container container location :value new-value))


(defmethod cl-ds:add! ((container mutable-dictionary) location new-value)
  (cl-ds.meta:position-modification #'cl-ds:add! container container location :value new-value))


(defmethod cl-ds:insert ((container functional-dictionary) location new-value)
  (cl-ds.meta:position-modification #'cl-ds:insert container container location :value new-value))


(defmethod cl-ds:erase ((container functional-dictionary) location)
  (cl-ds.meta:position-modification #'cl-ds:erase container container location))


(defmethod cl-ds:erase-if ((container functional-dictionary) location condition-fn)
  (cl-ds.meta:position-modification #'cl-ds:erase-if container container
                                    location :condition-fn condition-fn))


(defmethod cl-ds:erase-if! ((container mutable-dictionary) location condition-fn)
  (cl-ds.meta:position-modification #'cl-ds:erase-if! container container
                                    location :condition-fn condition-fn))


(defmethod cl-ds:erase! ((container mutable-dictionary) location)
  (cl-ds.meta:position-modification #'cl-ds:erase! container container location))


(defmethod cl-ds:update ((container functional-dictionary) location new-value)
  (cl-ds.meta:position-modification #'cl-ds:update container container location
                                    :value new-value))


(defmethod cl-ds:update! ((container mutable-dictionary) location new-value)
  (cl-ds.meta:position-modification #'cl-ds:update! container container location
                                    :value new-value))


(defmethod cl-ds:update-if ((container functional-dictionary) location new-value condition-fn)
  (cl-ds.meta:position-modification #'cl-ds:update-if container container location
                                    :value new-value
                                    :condition-fn condition-fn))


(defmethod cl-ds:update-if! ((container mutable-dictionary) location new-value condition-fn)
  (cl-ds.meta:position-modification #'cl-ds:update-if! container container location
                                    :value new-value
                                    :condition-fn condition-fn))


(defmethod cl-ds:insert! ((container mutable-dictionary) location value)
  (cl-ds.meta:position-modification #'cl-ds:insert!
                                    container
                                    container
                                    location
                                    :value value))


(defmethod (setf cl-ds:at) (new-value (container mutable-dictionary) location
                            &rest more-locations)
  (cl-ds:assert-one-dimension more-locations)
  (let ((status (nth-value
                 1
                 (cl-ds.meta:position-modification #'(setf cl-ds:at)
                                                   container
                                                   container
                                                   location
                                                   :value new-value))))
    (values new-value status)))


(defmethod cl-ds:insert! ((container mutable-sparse-vector) location value)
  (cl-ds.meta:position-modification #'cl-ds:insert!
                                    container
                                    container
                                    location
                                    :value value))


(defmethod (setf cl-ds:at) (new-value (container mutable-sparse-vector) location
                            &rest more-locations)
  (cl-ds:assert-one-dimension more-locations)
  (let ((status (nth-value
                 1
                 (cl-ds.meta:position-modification #'(setf cl-ds:at)
                                                   container
                                                   container
                                                   location
                                                   :value new-value))))
    (values new-value status)))


(defmethod cl-ds:add! ((container mutable-sparse-vector) location new-value)
  (cl-ds.meta:position-modification #'cl-ds:add! container container location :value new-value))


(defmethod cl-ds:update! ((container mutable-sparse-vector) location new-value)
  (cl-ds.meta:position-modification #'cl-ds:update! container container location :value new-value))


(defmethod cl-ds:update-if! ((container mutable-sparse-vector) location new-value condition-fn)
  (cl-ds.meta:position-modification #'cl-ds:update-if! container container location
                                    :value new-value
                                    :condition-fn condition-fn))


(defmethod cl-ds:erase! ((container mutable-sparse-vector) location)
  (cl-ds.meta:position-modification #'cl-ds:erase! container container location))


(defmethod cl-ds:insert ((container functional-sparse-vector) location new-value)
  (cl-ds.meta:position-modification #'cl-ds:insert container container location :value new-value))


(defmethod cl-ds:add ((container functional-sparse-vector) location new-value)
  (cl-ds.meta:position-modification #'cl-ds:add container container location :value new-value))


(defmethod cl-ds:update ((container functional-sparse-vector) location new-value)
  (cl-ds.meta:position-modification #'cl-ds:update container container location :value new-value))


(defmethod cl-ds:update-if ((container functional-sparse-vector) location new-value condition-fn)
  (cl-ds.meta:position-modification #'cl-ds:update-if container container location
                                    :value new-value
                                    :condition-fn condition-fn))


(defmethod cl-ds:erase ((container functional-sparse-vector) location)
  (cl-ds.meta:position-modification #'cl-ds:erase container container location))


(defmethod cl-ds:add! ((container mutable-dictionary) location new-value)
  (cl-ds.meta:position-modification #'cl-ds:add! container container location :value new-value))


(defmethod cl-ds:insert ((container functional-dictionary) location new-value)
  (cl-ds.meta:position-modification #'cl-ds:insert container container location :value new-value))


(defmethod cl-ds:erase ((container functional-dictionary) location)
  (cl-ds.meta:position-modification #'cl-ds:erase container container location))


(defmethod cl-ds:erase-if ((container functional-dictionary) location condition-fn)
  (cl-ds.meta:position-modification #'cl-ds:erase-if container container
                                    location :condition-fn condition-fn))


(defmethod cl-ds:erase-if! ((container mutable-dictionary) location condition-fn)
  (cl-ds.meta:position-modification #'cl-ds:erase-if! container container
                                    location :condition-fn condition-fn))


(defmethod cl-ds:erase! ((container mutable-dictionary) location)
  (cl-ds.meta:position-modification #'cl-ds:erase! container container location))


(defmethod cl-ds:update ((container functional-dictionary) location new-value)
  (cl-ds.meta:position-modification #'cl-ds:update container container location
                                    :value new-value))


(defmethod cl-ds:update! ((container mutable-dictionary) location new-value)
  (cl-ds.meta:position-modification #'cl-ds:update! container container location
                                    :value new-value))


(defmethod cl-ds:update-if ((container functional-dictionary) location new-value condition-fn)
  (cl-ds.meta:position-modification #'cl-ds:update-if container container location
                                    :value new-value
                                    :condition-fn condition-fn))


(defmethod cl-ds:update-if! ((container mutable-dictionary) location new-value condition-fn)
  (cl-ds.meta:position-modification #'cl-ds:update-if! container container location
                                    :value new-value
                                    :condition-fn condition-fn))


(defmethod (setf cl-ds:at) (new-value (container mutable-dictionary) location
                            &rest more-locations)
  (cl-ds:assert-one-dimension more-locations)
  (let ((status (nth-value
                 1
                 (cl-ds.meta:position-modification #'(setf cl-ds:at)
                                                   container
                                                   container
                                                   location
                                                   :value new-value))))
    (values new-value status)))
