(cl:in-package #:cl-ds.dicts)


(flet ((locate-tuple (container bucket hash location)
         (declare (type fundamental-hashing-dictionary container)
                  (type list bucket)
                  (type fixnum hash))
         (fbind ((comp (read-equal-fn container)))
           (iterate
             (for tuple in bucket)
             (finding
              tuple
              such-that (and (eql (cl-ds.common:hash-content-hash tuple)
                                  hash)
                             (comp (cl-ds.common:hash-content-location tuple)
                                   location)))))))

  (defmethod cl-ds.meta:grow-bucket! ((operation cl-ds.meta:insert-function)
                                      (container fundamental-hashing-dictionary)
                                      (bucket list)
                                      location
                                      &rest all
                                      &key hash value)
    (declare (ignore all))
    (let* ((tuple (locate-tuple container bucket hash location))
           (old-value (and tuple (cl-ds.common:hash-dict-content-value tuple))))
      (if (null tuple)
          (push (cl-ds.common:make-hash-dict-content
                      :location location
                      :value (cl-ds:force value)
                      :hash hash)
                bucket)
          (setf (cl-ds.common:hash-content-hash tuple) hash
                (cl-ds.common:hash-dict-content-value tuple) (cl-ds:force value)))
      (values bucket
              (if (null tuple)
                  (cl-ds.common:make-eager-modification-operation-status
                   nil nil t)
                  (cl-ds.common:make-eager-modification-operation-status
                   t old-value t)))))


  (defmethod cl-ds.meta:grow-bucket! ((operation cl-ds.meta:update-function)
                                      (container fundamental-hashing-dictionary)
                                      (bucket list)
                                      location
                                      &rest all
                                      &key hash value)
    (declare (ignore all))
    (let* ((tuple (locate-tuple container bucket hash location))
           (old-value (and tuple (cl-ds.common:hash-dict-content-value tuple))))
      (if (null tuple)
          (values bucket
                  cl-ds.common:empty-eager-modification-operation-status)
          (progn
            (setf (cl-ds.common:hash-dict-content-value tuple) hash
                  (cl-ds.common:hash-dict-content-value tuple) (cl-ds:force value))
            (values bucket
                    (cl-ds.common:make-eager-modification-operation-status
                     t old-value t))))))


  (defmethod cl-ds.meta:grow-bucket! ((operation cl-ds.meta:add-function)
                                      (container fundamental-hashing-dictionary)
                                      (bucket list)
                                      location
                                      &rest all
                                      &key hash value)
    (declare (ignore all))
    (let* ((tuple (locate-tuple container bucket hash location))
           (old-value (and tuple (cl-ds.common:hash-dict-content-value tuple))))
      (if (null tuple)
          (progn
            (push (cl-ds.common:make-hash-dict-content
                        :location location
                        :value (cl-ds:force value)
                        :hash hash)
                  bucket)
            (values bucket
                    cl-ds.common:empty-changed-eager-modification-operation-status))
          (values bucket
                  (cl-ds.common:make-eager-modification-operation-status
                   t
                   old-value
                   nil))))))


(defmethod cl-ds.meta:shrink-bucket! ((operation cl-ds.meta:erase-function)
                                      (container fundamental-hashing-dictionary)
                                      (bucket list)
                                      location
                                      &rest all
                                      &key hash)
  (declare (optimize (speed 3)
                     (safety 0)
                     (debug 0)
                     (space 0))
           (ignore all)
           (type fixnum hash))
  (fbind ((comp (read-equal-fn container)))
    (iterate
      (for cell on bucket)
      (for p-cell previous cell)
      (for tuple = (first cell))
      (when (and (eql (cl-ds.common:hash-content-hash tuple) hash)
                 (comp (cl-ds.common:hash-content-location tuple)
                       location))
        (if (null p-cell)
            (setf bucket (rest cell))
            (setf (cdr p-cell) (cdr cell)))
        (return-from cl-ds.meta:shrink-bucket!
          (values (or bucket 'cl-ds.meta:null-bucket)
                  (cl-ds.common:make-eager-modification-operation-status
                   t
                   (cl-ds.common:hash-dict-content-value tuple)
                   t))))
      (finally
       (return (values (or bucket 'cl-ds.meta:null-bucket)
                       cl-ds.common:empty-eager-modification-operation-status))))))


(defmethod cl-ds.meta:shrink-bucket! ((operation cl-ds.meta:erase-if-function)
                                      (container fundamental-hashing-dictionary)
                                      (bucket list)
                                      location
                                      &rest all
                                      &key hash condition-fn)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0))
           (type fixnum hash)
           (ignore all))
  (fbind ((comp (read-equal-fn container)))
    (iterate
      (for cell on bucket)
      (for p-cell previous cell)
      (for tuple = (first cell))
      (when (and (eql (cl-ds.common:hash-content-hash tuple) hash)
                 (comp (cl-ds.common:hash-content-location tuple)
                       location))
        (if  (funcall condition-fn
                      (cl-ds.common:hash-content-location tuple)
                      (cl-ds.common:hash-dict-content-value tuple))
             (progn
               (if (null p-cell)
                   (setf bucket (rest cell))
                   (setf (cdr p-cell) (cdr cell)))
               (return-from cl-ds.meta:shrink-bucket!
                 (values (or bucket 'cl-ds.meta:null-bucket)
                         (cl-ds.common:make-eager-modification-operation-status
                          t
                          (cl-ds.common:hash-dict-content-value tuple)
                          t))))
             (finish)))
      (finally
       (return (values bucket
                       cl-ds.common:empty-eager-modification-operation-status))))))


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
                       (cl-ds.common:hash-content-location x)
                       (cl-ds.common:hash-dict-content-value x)))
       bucket)
  bucket)


(defmethod cl-ds.meta:map-bucket ((container fundamental-sparse-vector)
                                  bucket
                                  function)
  (funcall function bucket)
  bucket)
