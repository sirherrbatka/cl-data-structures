(cl:in-package #:cl-ds.dicts.hamt)


(defun make-hash-dict-content (operation container location value hash status)
  (bind (((:values bucket status) (cl-ds.meta:make-bucket operation container value status)))
    (values (cl-ds.common:make-hash-dict-content :hash hash
                                                 :location location
                                                 :value bucket)
            status)))


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
  (bind ((result '())
         (rest (iterate
                 (with equal-fn = (cl-ds.dicts:read-equal-fn container))
                 (for rc on bucket)
                 (for c = (first rc))
                 (for l = (cl-ds.common:hash-content-location c))
                 (for h = (cl-ds.common:hash-content-hash c))
                 (finding rc such-that (and (= h hash) (funcall equal-fn l location)))
                 (push c result))))
    (bind (((:values new-node status)
            (make-hash-dict-content operation container location value hash
                                    (if (endp rest)
                                        (cl-ds.common:make-eager-modification-operation-status
                                         nil
                                         nil
                                         t)
                                        (cl-ds.common:make-eager-modification-operation-status
                                         t
                                         (cl-ds.common:hash-dict-content-value (first rest))
                                         t)))))
      (values (cons new-node
                    (nconc result (rest rest)))
              status))))


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
  (bind (((:values new-node status)
          (make-hash-dict-content operation container location value hash
                                  (cl-ds.common:make-eager-modification-operation-status nil nil t))))
    (values (cons new-node bucket)
            status)))


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
           (bind (((:values node status)
                   (make-hash-dict-content operation
                                           container
                                           location
                                           value
                                           hash
                                           (cl-ds.common:make-eager-modification-operation-status
                                            t
                                            (cl-ds.common:hash-dict-content-value node)
                                            t))))
             (return
               (values (cons node r)
                       status))))))))


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
        (bind (((:values new-node status)
                (make-hash-dict-content operation container location value hash
                                        (cl-ds.common:make-eager-modification-operation-status
                                         t
                                         (~> rest first cl-ds.common:hash-dict-content-value)
                                         t))))
          (values (cons new-node (nconc result (rest rest)))
                  status)))))
