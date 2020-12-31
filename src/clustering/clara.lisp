(cl:in-package #:cl-data-structures.clustering)

(cl-ds.alg.meta:define-aggregation-function
    clara-variable-number-of-medoids
    clara-variable-number-of-medoids-function

    (:range from to sample-size
     sample-count metric-fn
     &key key select-medoids-attempts-count
     attempts  split merge)

    (:range from to sample-size
     sample-count metric-fn
     &key (key #'identity) (select-medoids-attempts-count 50)
     (attempts 0) split merge)

    (%data %from %to %sample-size %sample-count %metric-fn
     %select-medoids-attempts-count %attempts %split %merge)

    ((setf %data (vect)
           %from from
           %to to
           %sample-size sample-size
           %sample-count sample-count
           %metric-fn metric-fn
           %select-medoids-attempts-count select-medoids-attempts-count
           %attempts attempts
           %split split
           %merge merge))

    ((element)
     (vector-push-extend element %data))

    ((cl-ds.utils.cluster.clara/pam:clara-variable-number-of-medoids
      %data
      %metric-fn %sample-size %sample-count %from %to
      :select-medoids-attempts-count %select-medoids-attempts-count
      :attempts %attempts :split %split :merge %merge)))


(cl-ds.alg.meta:define-aggregation-function
    clara
    clara-function

    (:range number-of-medoids sample-size
     sample-count metric-fn
     &key key select-medoids-attempts-count
     attempts split merge)

    (:range number-of-medoids sample-size sample-count metric-fn
     &key (key #'identity) (select-medoids-attempts-count 50)
     (attempts 0) split merge)

    (%data %number-of-medoids %sample-size %sample-count %metric-fn
     %select-medoids-attempts-count %attempts %split %merge)

    ((setf %data (vect)
           %number-of-medoids number-of-medoids
           %sample-size sample-size
           %sample-count sample-count
           %metric-fn metric-fn
           %select-medoids-attempts-count select-medoids-attempts-count
           %attempts attempts
           %split split
           %merge merge))

    ((element)
     (vector-push-extend element %data))

    ((cl-ds.utils.cluster.clara/pam:clara
      %data
      %number-of-medoids %metric-fn %sample-size %sample-count
      :select-medoids-attempts-count %select-medoids-attempts-count
      :attempts %attempts :split %split :merge %merge)))
