(cl:in-package #:cl-ds.dicts)


(deftype bucket () 'list)


(defgeneric find-content (container bucket location &key &allow-other-keys))


(defmethod find-content ((container fundamental-hashing-dictionary)
                         (bucket list) location &rest all &key hash)
  (declare (ignore all))
  (bind ((equal-fn (read-equal-fn container))
         ((:dflet compare-fn (a b))
          (and (eql (cl-ds.common:hash-content-hash a) hash)
               (funcall equal-fn
                        (cl-ds.common:hash-content-location a)
                        b)))
         ((:values r f) (cl-ds.utils:try-find
                         location bucket :test #'compare-fn)))
    (values (and f (cl-ds.common:hash-dict-content-value r))
            f)))


(defmacro bucket-growing-macro ((container bucket location hash value)
                                result status changed)
  (with-gensyms (!bucket !equal-fn !compare-fn)
    (once-only (container bucket location hash value)
      `(let ((,!equal-fn (read-equal-fn ,container)))
         (flet ((,!compare-fn (a b)
                  (and (eql (cl-ds.common:hash-content-hash a)
                            (cl-ds.common:hash-content-hash b))
                       (funcall ,!equal-fn
                                (cl-ds.common:hash-content-location a)
                                (cl-ds.common:hash-content-location b)))))
           (declare (dynamic-extent (function ,!compare-fn)))
           (let ((,!bucket (cl-ds.common:make-hash-dict-content
                            :hash ,hash
                            :location ,location
                            :value ,value)))
             (multiple-value-bind (^next-list ^replaced ^old-value)
                 (cl-ds.utils:insert-or-replace ,bucket
                                                ,!bucket
                                                :test (function ,!compare-fn))
               (when ,changed
                 (setf (cl-ds.common:hash-dict-content-value ,!bucket)
                       (~> ,!bucket
                           cl-ds.common:hash-dict-content-value
                           cl-ds:force)))
               (values ,result
                       ,status))))))))


(defmethod cl-ds.meta:shrink-bucket ((operation cl-ds.meta:erase-function)
                                     (container fundamental-hashing-dictionary)
                                     (bucket list)
                                     location
                                     &rest all
                                     &key hash)
  (declare (ignorable all))
  (bind ((equal-fn (read-equal-fn container))
         ((:dflet location-test (node location))
          (and (eql hash (cl-ds.common:hash-content-hash node))
               (funcall equal-fn location
                        (cl-ds.common:hash-content-location node))))
         ((:values list removed value)
          (cl-ds.utils:try-remove location
                                  bucket
                                  :test #'location-test)))
    (if removed
        (values
         (or list 'cl-ds.meta:null-bucket)
         (cl-ds.common:make-eager-modification-operation-status
          t
          (cl-ds.common:hash-dict-content-value value)
          t)
         t)
        (values
         bucket
         cl-ds.common:empty-eager-modification-operation-status
         nil))))


(defmethod cl-ds.meta:shrink-bucket ((operation cl-ds.meta:erase-if-function)
                                     (container fundamental-hashing-dictionary)
                                     (bucket list)
                                     location
                                     &rest all
                                     &key hash condition-fn)
  (declare (ignore all))
  (bind ((equal-fn (read-equal-fn container))
         ((:dflet location-test (node location))
          (when (and (eql hash (cl-ds.common:hash-content-hash node))
                     (funcall equal-fn location
                              (cl-ds.common:hash-content-location node)))
            (let ((result (funcall condition-fn
                                   (cl-ds.common:hash-content-location node)
                                   (cl-ds.common:hash-dict-content-value node))))
              (unless result
                (return-from cl-ds.meta:shrink-bucket
                  (values
                   bucket
                   cl-ds.common:empty-eager-modification-operation-status)))
              t)))
         ((:values list removed value)
          (cl-ds.utils:try-remove location
                                  bucket
                                  :test #'location-test)))
    (if removed
        (values
         (or list 'cl-ds.meta:null-bucket)
         (cl-ds.common:make-eager-modification-operation-status
          t
          (cl-ds.common:hash-dict-content-value value)
          t))
        (values
         bucket
         cl-ds.common:empty-eager-modification-operation-status))))


(defmethod cl-ds.meta:grow-bucket ((operation cl-ds.meta:insert-function)
                                   (container fundamental-hashing-dictionary)
                                   (bucket list)
                                   location
                                   &rest all
                                   &key hash value)
  (declare (ignore all))
  (bucket-growing-macro
      (container bucket location hash value)

      ^next-list
      (cl-ds.common:make-eager-modification-operation-status
       ^replaced
       (and ^replaced (cl-ds.common:hash-dict-content-value ^old-value))
       t)
      t))


(defmethod cl-ds.meta:grow-bucket ((operation cl-ds.meta:add-function)
                                   (container fundamental-hashing-dictionary)
                                   (bucket list)
                                   location
                                   &rest all
                                   &key hash value)
  (declare (ignore all))
  (bucket-growing-macro
      (container bucket location hash value)

      (if ^replaced bucket ^next-list)
      (cl-ds.common:make-eager-modification-operation-status
       ^replaced
       (and ^replaced (cl-ds.common:hash-dict-content-value ^old-value))
       (not ^replaced))
      (not ^replaced)))


(defmethod cl-ds.meta:grow-bucket ((operation cl-ds.meta:update-if-function)
                                   (container fundamental-hashing-dictionary)
                                   (bucket list)
                                   location
                                   &rest all
                                   &key hash value condition-fn)
  (declare (ignore all))
  (bind ((equal-fn (read-equal-fn container))
         (result (iterate
                   (for elt on bucket)
                   (for node = (car elt))
                   (finding elt such-that
                            (and (eql hash (cl-ds.common:hash-content-hash node))
                                 (funcall equal-fn
                                          location
                                          (cl-ds.common:hash-content-location node))))))
         (node (first result)))
    (if (or (null result)
            (not (funcall condition-fn
                          (cl-ds.common:hash-content-location node)
                          (cl-ds.common:hash-dict-content-value node))))
        (values bucket cl-ds.common:empty-eager-modification-operation-status nil)
        (iterate
          (with tail = nil)
          (for e on bucket)
          (until (eq e result))
          (for i = (first e))
          (collecting i at start into r)
          (when (endp tail)
            (setf tail r))
          (finally
           (setf (cdr tail) (cdr result))
           (return
             (values
              (cons (cl-ds.common:make-hash-dict-content
                     :hash (cl-ds.common:hash-content-hash node)
                     :location location
                     :value (cl-ds:force value))
                    r)
              (cl-ds.common:make-eager-modification-operation-status
               t
               (cl-ds.common:hash-dict-content-value node)
               t))))))))


(defmethod cl-ds.meta:grow-bucket ((operation cl-ds.meta:update-if!-function)
                                   (container fundamental-hashing-dictionary)
                                   (bucket list)
                                   location
                                   &rest all
                                   &key hash value condition-fn)
  (declare (ignore all))
  (bind ((equal-fn (read-equal-fn container))
         (result (iterate
                   (for elt on bucket)
                   (for node = (car elt))
                   (finding elt such-that
                            (and (eql hash (cl-ds.common:hash-content-hash node))
                                 (funcall equal-fn
                                          location
                                          (cl-ds.common:hash-content-location node))))))
         (node (first result)))
    (if (or (null result)
            (not (funcall condition-fn
                          (cl-ds.common:hash-content-location node)
                          (cl-ds.common:hash-dict-content-value node))))
        (values bucket cl-ds.common:empty-eager-modification-operation-status)
        (progn
          (setf (cl-ds.common:hash-content-location node) location
                (cl-ds.common:hash-dict-content-value node) (cl-ds:force value))
          (values bucket
                  (cl-ds.common:make-eager-modification-operation-status
                   t
                   (cl-ds.common:hash-dict-content-value node)
                   t))))))


(defmethod cl-ds.meta:grow-bucket ((operation cl-ds.meta:update-function)
                                   (container fundamental-hashing-dictionary)
                                   (bucket list)
                                   location
                                   &rest all
                                   &key hash value)
  (declare (ignore all))
  (bucket-growing-macro
      (container bucket location hash value)

      (if ^replaced ^next-list bucket)
      (if ^replaced
          (cl-ds.common:make-eager-modification-operation-status
           ^replaced
           (cl-ds.common:hash-dict-content-value ^old-value)
           ^replaced)
          cl-ds.common:empty-eager-modification-operation-status)
      ^replaced))


(defmethod cl-ds.meta:make-bucket ((operation cl-ds.meta:update-function)
                                   (container fundamental-hashing-dictionary)
                                   location
                                   &rest all
                                   &key hash value)
  (declare (ignore all hash value location)
           (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (values nil
          cl-ds.common:empty-eager-modification-operation-status))


(defmethod cl-ds.meta:make-bucket ((operation cl-ds.meta:add-function)
                                   (container fundamental-hashing-dictionary)
                                   location
                                   &rest all
                                   &key hash value)
  (declare (ignore all))
  (values (list (cl-ds.common:make-hash-dict-content
                 :location location
                 :value (cl-ds:force value)
                 :hash hash))
          cl-ds.common:empty-changed-eager-modification-operation-status))


(defmethod cl-ds.meta:make-bucket ((operation cl-ds.meta:insert-function)
                                   (container fundamental-hashing-dictionary)
                                   location
                                   &rest all
                                   &key hash value)
  (declare (ignore all))
  (values (list (cl-ds.common:make-hash-dict-content
                 :location location
                 :value (cl-ds:force value)
                 :hash hash))
          cl-ds.common:empty-changed-eager-modification-operation-status))


(defmethod cl-ds.meta:grow-bucket! ((operation cl-ds.meta:insert!-function)
                                    (container fundamental-sparse-vector)
                                    bucket
                                    location
                                    &rest all
                                    &key)
  (declare (ignore all))
  (values location
          cl-ds.common:empty-changed-eager-modification-operation-status))


(defmethod cl-ds.meta:grow-bucket! ((operation cl-ds.meta:update!-function)
                                    (container fundamental-sparse-vector)
                                    bucket
                                    location
                                    &rest all
                                    &key)
  (declare (ignore all))
  (values cl-ds.meta:null-bucket
          cl-ds.common:empty-eager-modification-operation-status))


(defmethod cl-ds.meta:grow-bucket! ((operation cl-ds.meta:add!-function)
                                    (container fundamental-sparse-vector)
                                    bucket
                                    location
                                    &rest all
                                    &key)
  (declare (ignore all))
  (values cl-ds.meta:null-bucket
          cl-ds.common:empty-eager-modification-operation-status))


(defmethod cl-ds.meta:shrink-bucket ((operation cl-ds.meta:erase-function)
                                     (container fundamental-sparse-vector)
                                     bucket
                                     location
                                     &rest all
                                     &key)
  (declare (ignore all))
  (values cl-ds.meta:null-bucket
          (cl-ds.common:make-eager-modification-operation-status t
                                                                 bucket
                                                                 t)))


(defmethod cl-ds.meta:shrink-bucket! ((operation cl-ds.meta:erase!-function)
                                      (container fundamental-sparse-vector)
                                      bucket
                                      location
                                      &rest all
                                      &key)
  (declare (ignore all))
  (values cl-ds.meta:null-bucket
          (cl-ds.common:make-eager-modification-operation-status t
                                                                 bucket
                                                                 t)))


(defmethod cl-ds.meta:grow-bucket ((operation cl-ds.meta:insert-function)
                                   (container fundamental-sparse-vector)
                                   bucket
                                   location
                                   &rest all
                                   &key)
  (declare (ignore all))
  (values location
          cl-ds.common:empty-changed-eager-modification-operation-status))


(defmethod cl-ds.meta:grow-bucket ((operation cl-ds.meta:update-function)
                                   (container fundamental-sparse-vector)
                                   bucket
                                   location
                                   &rest all
                                   &key)
  (declare (ignore all))
  (values cl-ds.meta:null-bucket
          cl-ds.common:empty-eager-modification-operation-status))


(defmethod cl-ds.meta:grow-bucket ((operation cl-ds.meta:add-function)
                                   (container fundamental-sparse-vector)
                                   bucket
                                   location
                                   &rest all
                                   &key)
  (declare (ignore all))
  (values cl-ds.meta:null-bucket
          cl-ds.common:empty-eager-modification-operation-status))


(defmethod cl-ds.meta:make-bucket ((operation cl-ds.meta:insert-function)
                                   (container fundamental-sparse-vector)
                                   location
                                   &rest all
                                   &key)
  (declare (ignore all)
           (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (values (cl-ds:force location)
          cl-ds.common:empty-changed-eager-modification-operation-status))


(defmethod cl-ds.meta:make-bucket ((operation cl-ds.meta:update-function)
                                   (container fundamental-sparse-vector)
                                   location
                                   &rest all
                                   &key)
  (declare (ignore all))
  (values cl-ds.meta:null-bucket
          cl-ds.common:empty-eager-modification-operation-status))


(defmethod cl-ds.meta:make-bucket ((operation cl-ds.meta:add-function)
                                   (container fundamental-sparse-vector)
                                   location
                                   &rest all
                                   &key)
  (declare (ignore all))
  (values (cl-ds:force location)
          cl-ds.common:empty-changed-eager-modification-operation-status))


(flet ((locate-tuple (container bucket hash location)
         (declare (type fundamental-hashing-dictionary container)
                  (type bucket bucket)
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


(defmethod cl-ds.meta:full-bucket-p ((container fundamental-hashing-dictionary)
                                     bucket)
  (not
   (iterate
     (for i from 1)
     (always (< i (read-bucket-size container))))))


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
