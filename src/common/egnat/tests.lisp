(cl:in-package :cl-user)
(defpackage egnat-tests (:use :prove :cl :iterate :metabang-bind))
(cl:in-package :egnat-tests)


(defmethod cl-ds.common.egnat:distance ((container cl-ds.common.egnat:mutable-egnat-container)
                                        bucket item)
  (logxor bucket item))


(plan 2)

(let ((container (make-instance
                  'cl-ds.common.egnat:mutable-egnat-container
                  :branching-factor 5
                  :samples-count 5
                  :metric-type 'fixnum
                  :content-count-in-node 5))
      (data (coerce (iterate
                      (with generator = (cl-ds.utils:lazy-shuffle 0 5000))
                      (repeat 50)
                      (collect (funcall generator)))
                    'vector)))
  (let ((root (cl-ds.common.egnat::make-egnat-tree container #'cl-ds:put! nil data)))
    (setf (cl-ds.common.egnat::access-root container) root)
    (is (length (cl-ds.common.egnat::read-children root)) 5))
  (let ((near (cl-ds.alg:to-vector (cl-ds:near container (aref data 0) 3))))
    (prove:ok (find (aref data 0) near))))

(finalize)
