(cl:in-package #:cl-ds.dicts)


(defclass fundamental-dictionary (cl-ds:fundamental-container)
  ())


(defclass fundamental-hashing-dictionary (fundamental-dictionary)
  ((%hash-fn
    :type (-> (t) fixnum)
    :initarg :hash-fn
    :reader read-hash-fn)
   (%equal-fn
    :type (-> (t t) boolean)
    :initarg :equal-fn
    :reader read-equal-fn)))


(defclass fundamental-sparse-vector (fundamental-dictionary)
  ())


(defclass functional-sparse-vector (fundamental-sparse-vector)
  ())


(defclass mutable-sparse-vector (fundamental-sparse-vector)
  ())


(defclass transactional-sparse-vector (mutable-sparse-vector)
  ())


(defclass functional-dictionary (fundamental-dictionary cl-ds:functional)
  ())


(defclass lazy-dictionary (cl-ds:lazy functional-dictionary)
  ())


(defclass mutable-dictionary (fundamental-dictionary cl-ds:mutable)
  ())


(defclass transactional-dictionary (mutable-dictionary cl-ds:transactional)
  ())


(defclass mutable-hashing-dictionary (fundamental-hashing-dictionary mutable-dictionary)
  ())


(defclass functional-hashing-dictionary (fundamental-hashing-dictionary functional-dictionary)
  ())


(defclass transactional-hashing-dictionary (fundamental-hashing-dictionary transactional-dictionary)
  ())


(defclass lazy-hashing-dictionary (functional-hashing-dictionary lazy-dictionary)
  ())
