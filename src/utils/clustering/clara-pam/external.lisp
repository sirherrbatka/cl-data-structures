(cl:in-package #:cl-ds.utils.cluster.clara/pam)


(-> partition-around-medoids (vector
                              cl-ds.utils:half-matrix
                              positive-fixnum
                              &key
                              (:select-medoids-attempts-count (or null positive-fixnum))
                              (:attempts non-negative-fixnum)
                              (:silhouette-sample-size positive-integer)
                              (:minimal-cluster-size positive-fixnum)
                              (:silhouette-sample-count positive-integer)
                              (:cluster-sample-size positive-integer)
                              (:split (or null positive-fixnum))
                              (:merge (or null positive-fixnum)))
    cl-ds.utils.cluster:clustering-result)
(defun partition-around-medoids (input-data
                                 distance-matrix
                                 number-of-medoids
                                 &key
                                   (select-medoids-attempts-count 50)
                                   (attempts 0)
                                   (silhouette-sample-size 500)
                                   (silhouette-sample-count 10)
                                   (cluster-sample-size 1000)
                                   (minimal-cluster-size 10)
                                   split
                                   merge)
  (when (or (zerop (length input-data)))
    (error "Can't cluster because there is no data"))
  (let ((state (make 'pam-algorithm-state
                     :number-of-medoids number-of-medoids
                     :input-data input-data
                     :distance-matrix distance-matrix
                     :split-merge-attempts-count attempts
                     :select-medoids-attempts-count select-medoids-attempts-count
                     :cluster-sample-size cluster-sample-size
                     :silhouette-sample-size silhouette-sample-size
                     :silhouette-sample-count silhouette-sample-count
                     :minimal-cluster-size minimal-cluster-size
                     :split-threshold split
                     :merge-threshold merge)))
    (build-pam-clusters state t)
    (assign-data-points-to-medoids state)
    (reassign-data-points-from-subminimal-clusters state)
    (let ((silhouette (silhouette state)))
      (replace-indexes-in-clusters-with-data state)
      (obtain-result state
                     silhouette))))


(-> clara (vector
           positive-fixnum
           (or symbol function)
           positive-fixnum
           positive-fixnum
           &key
           (:key (or symbol function))
           (:select-medoids-attempts-count (or null positive-fixnum))
           (:silhouette-sample-size positive-integer)
           (:silhouette-sample-count positive-integer)
           (:attempts non-negative-fixnum)
           (:cluster-sample-size positive-integer)
           (:minimal-cluster-size positive-fixnum)
           (:split (or null positive-fixnum))
           (:merge (or null positive-fixnum)))
    cl-ds.utils.cluster:clustering-result)
(defun clara (input-data
              number-of-medoids
              metric-fn
              sample-size
              sample-count
              &key
                (key #'identity)
                (select-medoids-attempts-count 50)
                (attempts 0)
                (silhouette-sample-size 500)
                (silhouette-sample-count 10)
                (cluster-sample-size 1000)
                (minimal-cluster-size 10)
                split
                merge)
  (declare (optimize (debug 3)))
  (let ((state nil))
    (cl-progress-bar:with-progress-bar
        ((* 2 sample-count)
         "Clustering data set of size ~a using CLARA algorithm with ~a samples of size ~a."
         (length input-data)
         sample-count
         sample-size)
      (setf state (build-clara-clusters
                   input-data number-of-medoids
                   (ensure-function metric-fn) sample-size sample-count
                   :key (ensure-function key)
                   :select-medoids-attempts-count select-medoids-attempts-count
                   :silhouette-sample-size silhouette-sample-size
                   :silhouette-sample-count silhouette-sample-count
                   :cluster-sample-size cluster-sample-size
                   :minimal-cluster-size minimal-cluster-size
                   :silhouette-sample-count silhouette-sample-size
                   :attempts attempts :split split :merge merge)))
    (cl-progress-bar:with-progress-bar
        ((length input-data)
         "Assigning data set to ~a clusters."
         (length (access-result-cluster-contents state)))
      (assign-clara-data-to-medoids state))
    (reassign-data-points-from-subminimal-clusters state)
    (replace-indexes-in-clusters-with-data state)
    (obtain-result state (access-silhouette state))))


(-> clara-variable-number-of-medoids (vector
                                      (or symbol function)
                                      positive-fixnum
                                      positive-fixnum
                                      positive-fixnum
                                      positive-fixnum
                                      &key
                                      (:key (or symbol function))
                                      (:select-medoids-attempts-count (or null positive-fixnum))
                                      (:silhouette-sample-size positive-integer)
                                      (:silhouette-sample-count positive-integer)
                                      (:cluster-sample-size positive-integer)
                                      (:attempts non-negative-fixnum)
                                      (:minimal-cluster-size positive-fixnum)
                                      (:split (or null positive-fixnum))
                                      (:merge (or null positive-fixnum)))
    cl-ds.utils.cluster:clustering-result)
(defun clara-variable-number-of-medoids (input-data
                                         metric-fn
                                         sample-size
                                         sample-count
                                         from to
                                         &key
                                           (key #'identity)
                                           (select-medoids-attempts-count 50)
                                           (attempts 0)
                                           (silhouette-sample-size 500)
                                           (silhouette-sample-count 10)
                                           (minimal-cluster-size 10)
                                           (cluster-sample-size 1000)
                                           split
                                           merge)
  (assert (< 0 from to))
  (let ((vector (make-array (1+ (- to from))))
        (metric-fn (ensure-function metric-fn))
        (key (ensure-function key))
        (final nil))
    (cl-progress-bar:with-progress-bar
        ((* (* 2 sample-count) (- (1+ to) from))
         "Clustering data set of size ~a using CLARA algorithm searching for optimal medoids count (between ~a and ~a)."
         (length input-data)
         from to)
      (flet ((make-thread (i)
               (nest
                (cl-ds.utils:with-rebind (cl-progress-bar:*progress-bar* i))
                (bt:make-thread)
                (lambda ())
                (cl-ds.utils:rebind)
                (build-clara-clusters
                 input-data i metric-fn
                 sample-size sample-count
                 :key key
                 :silhouette-sample-size silhouette-sample-size
                 :silhouette-sample-count silhouette-sample-count
                 :select-medoids-attempts-count select-medoids-attempts-count
                 :cluster-sample-size cluster-sample-size
                 :minimal-cluster-size minimal-cluster-size
                 :attempts attempts
                 :split split
                 :merge merge))))
        (iterate
          (for index from 0 below (length vector))
          (for i from from to to)
          (setf (aref vector index) (make-thread i))))
      (iterate
        (for thread in-vector vector)
        (for state = (bt:join-thread thread))
        (for silhouette = (access-silhouette state))
        (maximize silhouette into maximum)
        (when (= silhouette maximum)
          (setf final state))))
    (cl-progress-bar:with-progress-bar
        ((length input-data)
         "Assigning data set to ~a clusters."
         (length (access-cluster-contents final)))
      (assign-clara-data-to-medoids final))
    (reassign-data-points-from-subminimal-clusters final)
    (replace-indexes-in-clusters-with-data final)
    (obtain-result final (access-silhouette final))))
