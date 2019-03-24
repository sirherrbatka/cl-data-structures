(in-package #:cl-data-structures.utils.clustering)


(defclass k-means-algorithm-state ()
  ((%data :initarg :data
          :type vector
          :reader read-data)
   (%clusters :initarg :clusters
              :type vector
              :reader read-clusters)
   (%distortion-epsilon :initarg :distortion-epsilon
                        :type single-float
                        :reader read-distortion-epsilon)
   (%medoids :initarg :medoids
             :type vector
             :accessor access-medoids)
   (%medoids-count :initarg :medoids-count
                   :type non-negative-fixnum
                   :reader read-medoids-count))
  (:default-initargs
   :clusters (vect)
   :medoids (vect)
   :data (vect)))


(defmethod cl-ds.utils:cloning-information
    :append ((obj k-means-state))
  '((:data read-data)
    (:clusters access-clusters)
    (:medoids access-medoids)
    (:distorction-epsilon read-distortion-epsilon)
    (:medoids-count read-medoids-count)))


(defun select-initial-medoids (state)
  (let ((medoids-count (read-medoids-count state))
        (medoids (access-medoids state))
        (data (read-data state)))
    (setf medoids (adjust-array medoids medoids-count
                                :fill-pointer medoids-count))
    (cl-ds.utils:draw-random-vector data medoids-count)
    (setf (access-medoids state) medoids)
    state))


(defun k-means (data medoids-count distortion-epsilon)
  (iterate
    (with state = (make-k-means-status data medoids-count
                                       distortion-epsilon))
    (k-means-assign-data-points-to-medoids state)
    (k-means-select-new-medoids state)
    (for distortion = (k-means-distortion state))
    (for prev-distortion
         previous distortion
         initially nil)
    (while (or (null prev-distortion)
               (< (abs (- distortion prev-distortion))
                  distortion-epsilon )))
    (finally (k-means-obtain-result state))))
