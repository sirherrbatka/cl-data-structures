(cl:in-package #:cl-data-structures.metric-space.egnat)


(defclass egnat-metric-set (cl-ds.ms:metric-space-set
                            cl-ds.common.egnat:fundamental-egnat-container)
  ())


(defclass mutable-egnat-metric-set (cl-ds.ms:mutable-metric-space-set
                                    cl-ds.common.egnat:mutable-egnat-container)
  ())


(defmethod cl-ds.common.egnat:distance ((container egnat-metric-set)
                                        bucket item)
  (cl-ds.ms:distance container bucket item))


(defun make-mutable-egnat-metric-set (distance-function distance-type
                                      &key (branching-factor 20) (node-size 50) (samples-count 5)
                                      &allow-other-keys)
  (ensure-functionf distance-function)
  (make 'egnat-metric-set
        :metric-fn distance-function
        :metric-type distance-type
        :branching-factor branching-factor
        :samples-count samples-count
        :content-count-in-node node-size))


(defmethod cl-ds:make-from-traversable ((sequence vector)
                                        (class (eql 'mutable-egnat-metric-set))
                                        &rest arguments)
  (bind ((container (apply #'make-mutable-egnat-metric-set arguments))
         (root (cl-ds.common.egnat:make-egnat-tree container
                                                   #'cl-ds:put!
                                                   (cddr arguments)
                                                   sequence
                                                   (second (member :parallel arguments)))))
    (setf (cl-ds.common.egnat:access-root container) root
          (cl-ds.common.egnat:access-size container) (cl-ds:size sequence))
    container))


(defmethod cl-ds:make-from-traversable ((sequence cl-ds:fundamental-random-access-range)
                                        (class (eql 'mutable-egnat-metric-set))
                                        &rest arguments)
  (bind ((container (apply #'make-mutable-egnat-metric-set arguments))
         (root (cl-ds.common.egnat:make-egnat-tree container
                                                   #'cl-ds:put!
                                                   (cddr arguments)
                                                   sequence
                                                   (second (member :parallel arguments)))))
    (setf (cl-ds.common.egnat:access-root container) root
          (cl-ds.common.egnat:access-size container) (cl-ds:size sequence))
    container))


(defmethod cl-ds:make-from-traversable ((sequence cl-ds:fundamental-forward-range)
                                        (class (eql 'mutable-egnat-metric-set))
                                        &rest arguments)
  (let ((vector (vect)))
    (cl-ds:across sequence (rcurry #'vector-push-extend vector))
    (apply #'cl-ds:make-from-traversable vector class arguments)))
