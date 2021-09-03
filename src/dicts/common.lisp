(cl:in-package #:cl-ds.dicts)


(defclass lazy-box-dictionary (cl-ds.common:lazy-box-container lazy-dictionary)
  ())


(defmethod cl-ds:at ((container lazy-box-dictionary) location &rest more-locations)
  (assert (null more-locations))
  (cl-ds.common:force-version container)
  (cl-ds:at (cl-ds.common:access-content container) location))


(defmethod cl-ds:become-lazy ((container cl-ds.dicts:fundamental-dictionary))
  (make 'lazy-box-dictionary
        :content (cl-ds:become-transactional container)))


(defmethod cl-ds:whole-range ((container lazy-box-dictionary))
  (cl-ds.common:make-lazy-range cl-ds.common:forward-lazy-range
                                container
                                (cl-ds:whole-range container)))


(defmethod cl-ds.meta:map-bucket ((container fundamental-hashing-dictionary)
                                  bucket
                                  function)
  (map nil
       (lambda (x) (funcall function
                       (cons
                        (cl-ds.common:hash-content-location x)
                        (cl-ds.common:hash-dict-content-value x))))
       bucket)
  bucket)


(defmethod cl-ds.meta:map-bucket ((container fundamental-sparse-vector)
                                  bucket
                                  function)
  (funcall function bucket)
  bucket)
