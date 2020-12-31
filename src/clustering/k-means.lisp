(cl:in-package #:cl-data-structures.clustering)


(cl-ds.alg.meta:define-aggregation-function
    k-means k-means-function

    (:range number-of-medoids distortion-epsilon
     &key key silhouette-sample-size iterations silhouette-sample-count)

    (:range number-of-medoids distortion-epsilon
     &key (key 'identity) (silhouette-sample-size 500)
     (iterations nil) (silhouette-sample-count 15))

    (%number-of-medoids %distortion-epsilon
     %silhouette-sample-size %iterations
     %silhouette-sample-count %data)

    ((cl-ds:check-argument-bounds distortion-epsilon
                                  (< 0.0 distortion-epsilon))
     (cl-ds:check-argument-bounds silhouette-sample-size
                                  (< 0 silhouette-sample-size))
     (cl-ds:check-argument-bounds silhouette-sample-count
                                  (< 0 silhouette-sample-count))
     (cl-ds:check-argument-bounds number-of-medoids
                                  (< 0 number-of-medoids))
     (unless (null iterations)
       (cl-ds:check-argument-bounds iterations
                                    (< 0 iterations)))
     (setf %number-of-medoids       number-of-medoids
           %distortion-epsilon      distortion-epsilon
           %silhouette-sample-size  silhouette-sample-size
           %iterations              iterations
           %data                    (vect)
           %silhouette-sample-count silhouette-sample-count))

    ((element)
     (vector-push-extend element %data))

    ((cl-ds.utils.cluster.k-means:k-means
      %data %number-of-medoids %distortion-epsilon
      :silhouette-sample-size  %silhouette-sample-size
      :iterations              %iterations
      :silhouette-sample-count %silhouette-sample-count)))
