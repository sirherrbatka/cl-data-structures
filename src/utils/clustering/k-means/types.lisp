(cl:in-package #:cl-data-structures.utils.clustering.k-means)


(defclass k-means-algorithm-state ()
  ((%data :initarg :data
          :type vector
          :reader read-data)
   (%value-key :initarg :value-key
               :initarg :key
               :type function
               :reader read-value-key)
   (%silhouette-sample-size :initarg :silhouette-sample-size
                            :reader silhouette-sample-size)
   (%silhouette-sample-count :initarg :silhouette-sample-count
                             :reader silhouette-sample-count)
   (%clusters :initarg :clusters
              :type vector
              :reader read-clusters)
   (%distortion-epsilon :initarg :distortion-epsilon
                        :type single-float
                        :reader read-distortion-epsilon)
   (%medoids :initarg :medoids
             :type vector
             :accessor access-medoids)
   (%iterations :initarg :iterations
                :reader read-iterations)
   (%medoids-count :initarg :medoids-count
                   :type non-negative-fixnum
                   :reader read-medoids-count))
  (:default-initargs
   :clusters (vect)
   :value-key #'identity
   :silhouette-sample-count 15
   :silhouette-sample-size 500
   :iterations nil
   :medoids (vect)
   :data (vect)))


(cl-ds.utils:define-list-of-slots k-means-algorithm-state ()
  (%data read-data)
  (%clusters read-clusters)
  (%value-key read-value-key)
  (%iterations read-iterations)
  (%distortion-epsilon read-distortion-epsilon)
  (%silhouette-sample-size silhouette-sample-size)
  (%silhouette-sample-count silhouette-sample-count)
  (%medoids access-medoids)
  (%medoids-count read-medoids-count))


(defmethod cl-ds.utils:cloning-information
    append ((obj k-means-algorithm-state))
  '((:data read-data)
    (:clusters access-clusters)
    (:medoids access-medoids)
    (:iterations read-iterations)
    (:value-key read-value-key)
    (:silhouette-sample-size silhouette-sample-size)
    (:silhouette-sample-count silhouette-sample-count)
    (:distortion-epsilon read-distortion-epsilon)
    (:medoids-count read-medoids-count)))


(defmethod initialize-instance :after ((object k-means-algorithm-state)
                                       &rest all)
  (declare (ignore all))
  (cl-ds.utils:with-slots-for (object k-means-algorithm-state)
    (ensure-function %value-key)
    (check-type %distortion-epsilon single-float)
    (check-type %medoids-count integer)
    (check-type %iterations (or integer null))))
