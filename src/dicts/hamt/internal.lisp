(cl:in-package #:cl-ds.dicts.hamt)


(defun make-hash-dict-content (operation container location value hash status all)
  (bind (((:values bucket status) (apply #'cl-ds.meta:make-bucket operation container value status all)))
    (values (cl-ds.common:make-hash-dict-content :hash hash
                                                 :location location
                                                 :value bucket)
            status)))


(defgeneric shrink (operation container list location hash &rest all))
(defgeneric grow (operation container list location hash value &rest all))


(defmethod shrink ((operation cl-ds.meta:destructive-function)
                   container
                   bucket
                   location
                   hash
                   &rest all)
  (bind ((equal-fn (cl-ds.dicts:read-equal-fn container))
         ((prev . result)
          (or (iterate
                (for elt on bucket)
                (for p-elt previous elt)
                (for node = (car elt))
                (finding (cons p-elt elt) such-that
                         (and (eql hash (cl-ds.common:hash-content-hash node))
                              (funcall equal-fn
                                       location
                                       (cl-ds.common:hash-content-location node)))))
              (cons nil nil)))
         (node (first result))
         ((:values new-value status) (if (null result)
                                         (return-from shrink
                                           (values bucket
                                                   cl-ds.common:empty-eager-modification-operation-status))
                                        (apply #'cl-ds.meta:alter-bucket!
                                                 operation container nil
                                                 (cl-ds.common:hash-dict-content-value node)
                                                 all))))
    (when (cl-ds:changed status)
      (if (cl-ds.meta:null-bucket-p new-value)
          (if (null prev)
              (return-from shrink (values (rest result) status))
              (setf (rest prev) (rest result)))
          (setf (cl-ds.common:hash-dict-content-value node) new-value)))
    (values bucket status)))


(defmethod shrink ((operation cl-ds.meta:functional-function)
                   container
                   (bucket list)
                   location
                   hash
                   &rest all)
  (bind ((equal-fn (cl-ds.dicts:read-equal-fn container))
         (result
          (iterate
            (for elt on bucket)
            (for node = (car elt))
            (finding elt such-that
                     (and (eql hash (cl-ds.common:hash-content-hash node))
                          (funcall equal-fn
                                   location
                                   (cl-ds.common:hash-content-location node))))))
         (node (first result))
         ((:values new-value status) (if (null result)
                                         (return-from shrink
                                           (values bucket
                                                   cl-ds.common:empty-eager-modification-operation-status))
                                        (apply #'cl-ds.meta:alter-bucket
                                                 operation container nil
                                                 (cl-ds.common:hash-dict-content-value node) all))))
    (unless (cl-ds:changed status)
      (return-from shrink (values bucket status)))
    (iterate
      (with new-bucket = (list))
      (for elt on bucket)
      (for node = (car elt))
      (if (eq elt result)
          (unless (cl-ds.meta:null-bucket-p new-value)
            (push  (cl-ds.common:make-hash-dict-content :hash hash
                                                        :location location
                                                        :value new-value)
                   new-bucket))
          (push node new-bucket))
      (finally (return-from shrink (values new-bucket status))))))


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


(defmethod grow ((operation cl-ds.meta:functional-function)
                 container
                 bucket
                 location
                 hash
                 value
                 &rest all)
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
            (if (endp rest)
                (make-hash-dict-content operation container location value hash
                                        (cl-ds.common:make-eager-modification-operation-status
                                         nil nil t)
                                        all)
                (bind (((:values altered-bucket status)
                        (apply #'cl-ds.meta:alter-bucket operation container
                               value
                               (cl-ds.common:hash-dict-content-value (first rest))
                               all)))
                  (values (if (cl-ds:changed status)
                              (cl-ds.common:make-hash-dict-content :hash hash
                                                                   :location location
                                                                   :value altered-bucket)
                              (first rest))
                          status)))))
      (values (cons new-node (nconc result (rest rest)))
              status))))


(defmethod grow ((operation cl-ds.meta:destructive-function)
                 container
                 (bucket list)
                 location
                 hash
                 value
                 &rest all)
  (bind ((equal-fn (cl-ds.dicts:read-equal-fn container))
         (result (iterate
                   (for elt on bucket)
                   (for node = (car elt))
                   (finding elt such-that
                            (and (eql hash (cl-ds.common:hash-content-hash node))
                                 (funcall equal-fn
                                          location
                                          (cl-ds.common:hash-content-location node))))))
         (node (first result))
         ((:values new-value status) (if (null result)
                                        (apply #'cl-ds.meta:make-bucket operation container value
                                                 all)
                                        (apply #'cl-ds.meta:alter-bucket!
                                                 operation container value
                                                 (cl-ds.common:hash-dict-content-value node)
                                                 all))))
    (when (cl-ds:changed status)
      (if (null result)
          (setf bucket (cons (cl-ds.common:make-hash-dict-content :hash hash
                                                                  :location location
                                                                  :value new-value)
                             bucket))
          (setf (cl-ds.common:hash-dict-content-value node) new-value)))
    (values bucket status)))
