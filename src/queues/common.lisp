(cl:in-package #:cl-data-structures.queues)


(defclass fundamental-queue (cl-ds:fundamental-container)
  ((%size :initarg :size
          :initform 0
          :reader cl-ds:size
          :accessor access-size)))


(defclass fundamental-mutable-queue (cl-ds:mutable fundamental-queue)
  ())


(defclass fundamental-transactional-queue (cl-ds:transactional fundamental-queue)
  ())


(defclass fundamental-functional-queue (cl-ds:functional fundamental-queue)
  ())


(defmethod cl-ds:put! ((container fundamental-mutable-queue)
                       item)
  (cl-ds.meta:position-modification #'cl-ds:put! container
                                    container item))


(defmethod cl-ds:put! ((container fundamental-transactional-queue)
                       item)
  (cl-ds.meta:position-modification #'cl-ds:put! container
                                    container item))


(defmethod cl-ds:take-out! ((container fundamental-mutable-queue))
  (cl-ds.meta:position-modification #'cl-ds:take-out! container
                                    container nil))


(defmethod cl-ds:take-out! ((container fundamental-transactional-queue))
  (cl-ds.meta:position-modification #'cl-ds:take-out! container
                                    container nil))


(defmethod cl-ds:take-out ((container fundamental-functional-queue))
  (cl-ds.meta:position-modification #'cl-ds:take-out container
                                    container nil))


(defmethod cl-ds:put ((container fundamental-functional-queue)
                      item)
  (cl-ds.meta:position-modification #'cl-ds:put container
                                    container item))


(defmethod cl-ds:take-out! ((container fundamental-functional-queue))
  (cl-ds.meta:position-modification #'cl-ds:take-out container
                                    container nil))
