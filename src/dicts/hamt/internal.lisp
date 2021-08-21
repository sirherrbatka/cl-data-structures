(cl:in-package #:cl-ds.dicts.hamt)


(defun make-hash-dict-content (operation container location value hash)
  (bind ((bucket (cl-ds.meta:make-bucket operation container value)))
    (cl-ds.common:make-hash-dict-content
     :hash hash
     :location location
     :value bucket)))


(defgeneric fresh-bucket-status (operation value))


(defmethod fresh-bucket-status ((operation cl-ds.meta:update-function) value)
  cl-ds.common:empty-eager-modification-operation-status)


(defmethod fresh-bucket-status ((operation cl-ds.meta:update-if-function) value)
  cl-ds.common:empty-eager-modification-operation-status)


(defmethod fresh-bucket-status ((operation cl-ds.meta:update!-function) value)
  cl-ds.common:empty-eager-modification-operation-status)


(defmethod fresh-bucket-status ((operation cl-ds.meta:update-if!-function) value)
  cl-ds.common:empty-eager-modification-operation-status)


(defmethod fresh-bucket-status (operation value)
  (cl-ds.common:make-eager-modification-operation-status
   nil nil t))


(defmacro bucket-growing-macro ((container list location hash value)
                                result status changed)
  (with-gensyms (!bucket !equal-fn !compare-fn !status)
    (once-only (container list location hash value)
      `(let ((,!equal-fn (cl-ds.dicts:read-equal-fn ,container)))
         (flet ((,!compare-fn (a b)
                  (and (eql (cl-ds.common:hash-content-hash a)
                            (cl-ds.common:hash-content-hash b))
                       (funcall ,!equal-fn
                                (cl-ds.common:hash-content-location a)
                                (cl-ds.common:hash-content-location b)))))
           (declare (dynamic-extent (function ,!compare-fn)))
           (bind (((:values ,!bucket ,!status) (make-hash-dict-content operation container location value hash)))
             (if (null ,!bucket)
                 (values ,list ,!status)
                 (multiple-value-bind (^next-list ^replaced ^old-value)
                     (cl-ds.utils:insert-or-replace ,list
                                                    ,!bucket
                                                    :test (function ,!compare-fn))
                   (when ,changed
                     (setf (cl-ds.common:hash-dict-content-value ,!bucket)
                           (~> ,!bucket
                               cl-ds.common:hash-dict-content-value
                               cl-ds:force)))
                   (values ,result
                           ,status)))))))))


(defgeneric shrink (operation container list location hash &rest all))
(defgeneric grow (operation container list location hash value &rest all))


(defmethod shrink ((operation cl-ds.meta:erase-function)
                   container
                   bucket
                   location
                   hash
                   &rest all)
  (declare (ignorable all))
  (bind ((equal-fn (cl-ds.dicts:read-equal-fn container))
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


(defmethod shrink ((operation cl-ds.meta:erase-if-function)
                   container
                   (bucket list)
                   location
                   hash
                   &rest all
                   &key condition-fn)
  (declare (ignore all))
  (bind ((equal-fn (cl-ds.dicts:read-equal-fn container))
         ((:dflet location-test (node location))
          (when (and (eql hash (cl-ds.common:hash-content-hash node))
                     (funcall equal-fn location
                              (cl-ds.common:hash-content-location node)))
            (let ((result (funcall condition-fn
                                   (cl-ds.common:hash-content-location node)
                                   (cl-ds.common:hash-dict-content-value node))))
              (unless result
                (return-from shrink
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


(defun find-content (container bucket location hash)
  (bind ((equal-fn (cl-ds.dicts:read-equal-fn container))
         ((:dflet compare-fn (a b))
          (and (eql (cl-ds.common:hash-content-hash a) hash)
               (funcall equal-fn
                        (cl-ds.common:hash-content-location a)
                        b)))
         ((:values r f) (cl-ds.utils:try-find
                         location bucket :test #'compare-fn)))
        (values (and f (cl-ds.common:hash-dict-content-value r))
                f)))



(defmethod grow ((operation cl-ds.meta:insert-function)
                 container
                 bucket
                 location
                 hash
                 value
                 &rest all)
  (declare (ignore all))
  (bucket-growing-macro
      (container bucket location hash value)

      ^next-list
      (cl-ds.common:make-eager-modification-operation-status
       ^replaced
       (and ^replaced (cl-ds.common:hash-dict-content-value ^old-value))
       t)
      t))


(defmethod grow ((operation cl-ds.meta:add-function)
                 container
                 (bucket list)
                 location
                 hash
                 value
                 &rest all)
  (declare (ignore all))
  (iterate
    (with equal-fn = (cl-ds.dicts:read-equal-fn container))
    (for rc on bucket)
    (for c = (first rc))
    (for h = (cl-ds.common:hash-content-hash c))
    (when (and (= h hash) (funcall equal-fn
                                   location
                                   (cl-ds.common:hash-content-location c)))
      (return-from grow
        (values bucket
                (cl-ds.common:make-eager-modification-operation-status
                 t
                 (cl-ds.common:hash-dict-content-value c)
                 nil)))))
  (let ((new-node (make-hash-dict-content operation container location value hash)))
    (values (cons new-node bucket)
            (cl-ds.common:make-eager-modification-operation-status
             nil
             nil
             t))))


(defmethod grow ((operation cl-ds.meta:update-if-function)
                 container
                 (bucket list)
                 location
                 hash
                 value
                 &rest all
                 &key condition-fn)
  (declare (ignore all))
  (bind ((equal-fn (cl-ds.dicts:read-equal-fn container))
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


(defmethod grow ((operation cl-ds.meta:update-if!-function)
                 container
                 (bucket list)
                 location
                 hash
                 value
                 &rest all
                 &key condition-fn)
  (declare (ignore all))
  (bind ((equal-fn (cl-ds.dicts:read-equal-fn container))
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


(defmethod grow ((operation cl-ds.meta:update-function)
                 container
                 (bucket list)
                 location
                 hash
                 value
                 &rest all)
  (declare (ignore all))
  (bind ((result '())
         (rest (iterate
                 (with equal-fn = (cl-ds.dicts:read-equal-fn container))
                 (for rc on bucket)
                 (for c = (first rc))
                 (for l = (cl-ds.common:hash-content-location c))
                 (for h = (cl-ds.common:hash-content-hash c))
                 (finding rc such-that (and (= h hash) (funcall equal-fn l location)))
                 (push c result))))
    (if (endp rest)
        (values bucket cl-ds.common:empty-eager-modification-operation-status)
        (let ((new-node (make-hash-dict-content operation container location value hash)))
          (values (cons new-node
                        (nconc result (rest rest)))
                  (cl-ds.common:make-eager-modification-operation-status
                   t
                   (cl-ds.common:hash-dict-content-value new-node)
                   t))))))
