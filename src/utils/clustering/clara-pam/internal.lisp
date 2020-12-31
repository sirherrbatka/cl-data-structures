(cl:in-package #:cl-ds.utils.cluster.clara/pam)


(defun clear-unfinished-clusters (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (setf %unfinished-clusters (adjust-array %unfinished-clusters
                                             (length %cluster-contents)
                                             :fill-pointer (length %cluster-contents)))
    (map-into %unfinished-clusters (constantly nil))))


(defun unfinished-clusters-p (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (some #'identity %unfinished-clusters)))


(defun clear-cluster-contents (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (setf (fill-pointer %cluster-contents) %number-of-medoids)
    (map nil (curry #'(setf fill-pointer) 1) %cluster-contents)))


(defun order-medoids (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (setf %cluster-contents (sort %cluster-contents #'< :key #'first-elt))))


(defun choose-initial-medoids (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (iterate
      (with generator = (cl-ds.utils:lazy-shuffle 0 (length %indexes)))
      (for cluster in-vector %cluster-contents)
      (for new-value = (funcall generator))
      (assert new-value)
      (setf (aref cluster 0) (aref %indexes new-value)))
    (order-medoids state)))


(defun medoid-p (state index)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (let* ((position (cl-ds.utils:lower-bound
                      %cluster-contents
                      (the non-negative-fixnum index)
                      #'<
                      :key #'first-elt))
           (cluster-count (length %cluster-contents)))
      (and (< position cluster-count)
           (= index (~> %cluster-contents
                        (aref position)
                        first-elt))))))


(defun closest-medoid (state index)
  (declare (optimize (speed 3) (safety 1)))
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (unless (medoid-p state index)
      (iterate
        (declare (type number distance))
        (with result = 0)
        (for cluster in-vector %cluster-contents)
        (for i from 0)
        (for medoid = (aref cluster 0))
        (for distance = (cl-ds.utils:mref %distance-matrix index medoid))
        (minimize distance into mini)
        (when (= mini distance)
          (setf result i))
        (finally (return result))))))


(defmethod assign-data-points-to-medoids (state)
  (declare (optimize (speed 3) (safety 0)))
  (assert (unique-assigment state))
  (order-medoids state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (map nil (curry #'(setf fill-pointer) 1) %cluster-contents)
    (iterate
      (with assignments = (lparallel:pmap '(vector (or null fixnum))
                                          (curry #'closest-medoid state)
                                          %indexes))
      (for i in-vector %indexes)
      (for assignment in-vector assignments)
      (for medoid-p = (null assignment))
      (unless medoid-p
        (vector-push-extend i (aref %cluster-contents assignment))))))


(defun silhouette (state)
  (declare (optimize (speed 3) (safety 1)))
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (cl-ds.utils.cluster:calculate-silhouette
     (make 'cl-ds.utils.cluster:clustering-result
           :cluster-contents %cluster-contents
           :silhouette-sample-size %silhouette-sample-size
           :silhouette-sample-count %silhouette-sample-count)
     %distance-matrix)))


(-> choose-effective-medoid (pam-algorithm-state (vector t)) boolean)
(defun choose-effective-medoid (state cluster)
  (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (bind (((:dflet swap-medoid (i))
            (declare (type non-negative-fixnum i))
            (rotatef (aref cluster i) (aref cluster 0)))
           ((:dflet total-distance-to-medoid (&optional old-cost))
            (iterate
              (for i from 1 below (length cluster))
              (for distance = (cl-ds.utils:mref %distance-matrix
                                                (the fixnum (aref cluster 0))
                                                (the fixnum (aref cluster i))))
              (assert distance)
              (sum distance into sum)
              (unless (null old-cost)
                (while (<= sum old-cost)))
              (finally (return sum))))
           (improved-something nil))
      (cl-ds.utils:optimize-value ((minimal-distance-to-medoid
                                    <
                                    (total-distance-to-medoid)))
        (iterate
          (for i from 1 below (length cluster))
          (swap-medoid i)
          (for distance = (total-distance-to-medoid
                           minimal-distance-to-medoid))
          (minimal-distance-to-medoid distance)
          (for improved = (= distance minimal-distance-to-medoid))
          (unless improved
            (swap-medoid i))
          (setf improved-something
                (not (null (or improved improved-something))))))
      improved-something)))


(defun choose-effective-medoids (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (assert (eql (length %unfinished-clusters)
                 (length %cluster-contents)))
    (lparallel:pmap-into %unfinished-clusters
                         (curry #'choose-effective-medoid state)
                         %cluster-contents)
    (order-medoids state)))


(defun scan-for-clusters-of-invalid-size (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (clear-unfinished-clusters state)
    (map-into %unfinished-clusters
              (lambda (x)
                (not (< %merge-threshold
                        (length x)
                        %split-threshold)))
              %cluster-contents)))


(defun unique-assigment (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (= (~> (apply #'concatenate 'vector (coerce %cluster-contents 'list))
           remove-duplicates
           length)
       (reduce #'+ %cluster-contents :key #'length :initial-value 0))))


(defun build-pam-clusters (state &optional split-merge)
  (declare (optimize (speed 3) (safety 0)))
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (bind ((optimal-content nil)
           (clusters-with-optimal-size nil)
           ((:flet split-merge ())
            (when (and split-merge
                       (not (zerop split-merge))
                       %split-merge-attempts-count)
              (iterate
                (scan-for-clusters-of-invalid-size state)
                (while (unfinished-clusters-p state))
                (repeat %split-merge-attempts-count)
                (recluster-clusters-of-invalid-size state)
                (for right-size =
                     (/ (count-if (lambda (x) (< %merge-threshold x %split-threshold))
                                  %cluster-contents)
                        (length %cluster-contents)))
                (ensure clusters-with-optimal-size right-size)
                (minf clusters-with-optimal-size right-size)
                (when (= right-size clusters-with-optimal-size)
                  (setf optimal-content (map-into (copy-array %cluster-contents)
                                                  #'copy-array
                                                  %cluster-contents)))
                (finally (setf %cluster-contents optimal-content))))))
      (iterate
        (with attempts = %select-medoids-attempts-count)
        (for i from 0)
        (unless (null attempts)
          (unless (< i attempts)
            (leave t)))
        (when (zerop (rem i 3))
          (clear-cluster-contents state)
          (choose-initial-medoids state)
          (assign-data-points-to-medoids state))
        ;; (assert (unique-assigment state))
        (clear-unfinished-clusters state)
        (choose-effective-medoids state)
        (always (unfinished-clusters-p state))
        (finally
         (split-merge)
         (clear-unfinished-clusters state))))))


(defun fill-reclustering-index-vector (state indexes count-of-eliminated)
  (declare (optimize (speed 3) (safety 1)))
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (iterate
      (with position = 0)
      (for i from (~> %cluster-contents length 1-) downto 0)
      (repeat count-of-eliminated)
      (for cluster = (aref %cluster-contents i))
      (iterate
        (for value in-vector cluster)
        (setf (aref indexes position) value)
        (incf position))))
  indexes)


(defun prepare-reclustering-index-vector (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (bind ((count-of-eliminated (cl-ds.utils:swap-if
                                 %cluster-contents
                                 (lambda (x)
                                   (not (< %merge-threshold
                                           x
                                           %split-threshold)))
                                 :key #'length))
           (count-of-elements (iterate
                                (for i
                                     from (~> %cluster-contents length 1-)
                                     downto 0)
                                (repeat count-of-eliminated)
                                (sum (~> %cluster-contents
                                         (aref i)
                                         length))))
           ((:dflet expected-cluster-count ())
            (round (/ count-of-elements
                      (/ (+ %split-threshold %merge-threshold)
                         2)))))
      (iterate
        (while (zerop (expected-cluster-count)))
        (until (eql count-of-eliminated (length %cluster-contents)))
        (incf count-of-eliminated)
        (incf count-of-elements (~>> (length %cluster-contents)
                                     (- _ count-of-eliminated)
                                     (aref %cluster-contents)
                                     length)))
      (values
       (fill-reclustering-index-vector
        state
        (make-array count-of-elements :element-type 'non-negative-fixnum)
        count-of-eliminated)
       count-of-eliminated
       (expected-cluster-count)))))


(defun recluster-clusters-of-invalid-size (state)
  (declare (optimize (speed 1) (safety 3)))
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (setf %cluster-contents (shuffle %cluster-contents))
    (bind (((:values indexes count-of-eliminated expected-cluster-count)
            (prepare-reclustering-index-vector state))
           (fresh-state (make
                         'pam-algorithm-state
                         :indexes indexes
                         :distance-matrix %distance-matrix
                         :merge-threshold %merge-threshold
                         :split-threshold %split-threshold
                         :number-of-medoids expected-cluster-count
                         :select-medoids-attempts-count %select-medoids-attempts-count
                         :silhouette-sample-size %silhouette-sample-size
                         :silhouette-sample-count %silhouette-sample-count
                         :split-merge-attempts-count %split-merge-attempts-count
                         :input-data %input-data)))
      (build-pam-clusters fresh-state nil)
      (decf (fill-pointer %cluster-contents) count-of-eliminated)
      (map nil
           (rcurry #'vector-push-extend %cluster-contents)
           (access-cluster-contents fresh-state))
      (assert (unique-assigment state))
      (order-medoids state))))


(defun replace-indexes-in-cluster-with-data (state cluster)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (map-into cluster (curry #'aref %input-data) cluster)))


(defun replace-indexes-in-clusters-with-data (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (lparallel:pmap-into %cluster-contents
                         (curry #'replace-indexes-in-cluster-with-data
                                state)
                         %cluster-contents)))


(defgeneric obtain-result (state silhouette))


(defmethod obtain-result ((state pam-algorithm-state) silhouette)
  (make 'cl-data-structures.utils.clustering:clustering-result
        :cluster-contents (access-cluster-contents state)
        :silhouette silhouette))


(defun index-mapping-function (state)
  (cl-ds.utils:with-slots-for (state clara-algorithm-state)
    (let ((index-mapping %index-mapping))
      (declare (type (simple-array non-negative-fixnum (*)) %index-mapping))
      (lambda (x)
        (declare (optimize (speed 3)
                           (safety 0)
                           (debug 0)))
        (aref index-mapping x)))))


(defun initialize-distance-matrix (state)
  (cl-ds.utils:with-slots-for (state clara-algorithm-state)
    (setf %distance-matrix
          (cl-ds.utils:parallel-make-distance-matrix-from-vector
           'single-float
           %metric-fn
           (lparallel:pmap 'vector
                           (compose %key (curry #'aref %input-data))
                           %indexes)
           :query-key (index-mapping-function state)))
    (cl-progress-bar:update 1)))


(defun draw-clara-sample (state)
  (cl-ds.utils:with-slots-for (state clara-algorithm-state)
    (setf %all-indexes (shuffle %all-indexes)
          %indexes (take %sample-size %all-indexes))
    (assert (<= (length %indexes)
                (length %index-mapping)))
    (iterate
      (for k from 0)
      (for i in-vector %indexes)
      (setf (aref %index-mapping i) k))
    (clear-unfinished-clusters state)
    (clear-cluster-contents state)
    (initialize-distance-matrix state)))


(defun update-result-cluster (state)
  (cl-ds.utils:with-slots-for (state clara-algorithm-state)
    (let ((silhouette (mean (silhouette state))))
      (when (or (null %silhouette)
                (> silhouette %silhouette))
        (setf %silhouette silhouette
              %result-cluster-contents (map-into (copy-array %cluster-contents)
                                                 #'copy-array
                                                 %cluster-contents))))))


(defun assign-clara-data-to-medoids (state)
  (cl-ds.utils:with-slots-for (state clara-algorithm-state)
    (map nil (curry #'(setf fill-pointer) 1) %cluster-contents)
    (order-medoids state)
    (bind ((medoids (lparallel:pmap-into
                     (copy-array %cluster-contents)
                     (lambda (x)
                       (~>> x first-elt
                            (aref %input-data)
                            (funcall %key)))
                     %cluster-contents))
           (cluster-mutex (map-into (copy-array %cluster-contents)
                                    #'bt:make-lock)))
      (cl-data-structures.utils:with-rebind (cl-progress-bar:*progress-bar*)
        (lparallel:pmap
         nil
         (lambda (index)
           (cl-data-structures.utils:rebind
            (unless (medoid-p state index)
              (iterate
                (with some-data = (~>> index
                                       (aref %input-data)
                                       (funcall %key)))
                (with target = 0)
                (for j from 0)
                (for medoid in-vector medoids)
                (for distance = (funcall %metric-fn
                                         medoid
                                         some-data))
                (minimize distance into mini)
                (when (= distance mini)
                  (setf target j))
                (finally
                 (bt:with-lock-held ((aref cluster-mutex target))
                   (vector-push-extend index
                                       (aref %cluster-contents target))))))
            (cl-progress-bar:update 1)))
         %all-indexes)))))


(defgeneric draw-cluster-sample (state)
  (:method ((state clara-algorithm-state))
    (cl-ds.utils:with-slots-for (state clara-algorithm-state)
      (map 'vector
           (lambda (s)
             (map-into (make-array %cluster-sample-size)
                       (lambda ()
                         (~> (length s)
                             random
                             (aref s _)
                             (aref %input-data _)
                             (funcall %key _)))))
           %cluster-contents)))
  (:method ((state pam-algorithm-state))
    (cl-ds.utils:with-slots-for (state pam-algorithm-state)
      (map 'vector
           (lambda (s)
             (map-into (make-array %cluster-sample-size)
                       (lambda ()
                         (~> (length s)
                             random
                             (aref s _)))))
           %cluster-contents))))


;; Should be replaced by ESTIMATED (or expected) distance, estimator should be a callback
;; passed to the clustering function and it should default to mean.
;; Median can be also useful, though.
(defgeneric mean-distance-to-cluster (state element cluster)
  (:method ((state clara-algorithm-state) element cluster)
    (cl-ds.utils:with-slots-for (state clara-algorithm-state)
      (mean (map 'vector
                 (curry %metric-fn element)
                 cluster))))
  (:method ((state pam-algorithm-state) element cluster)
    (cl-ds.utils:with-slots-for (state pam-algorithm-state)
      (mean (map 'vector
                 (curry #'cl-ds.utils:mref %distance-matrix element)
                 cluster)))))


(defgeneric for-distance-calculation (state index)
  (:method ((state clara-algorithm-state) index)
    (cl-ds.utils:with-slots-for (state clara-algorithm-state)
      (funcall %key (aref %input-data index))))
  (:method ((state pam-algorithm-state) index)
    index))


(defun reassign-data-points-from-cluster (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (let ((last-cluster (cl-ds.utils:pop-last %cluster-contents)))
      (when (emptyp %cluster-contents)
        (error "Can't eleminate subminimal cluster, because it is the only one left."))
      (iterate
        (with sample = (draw-cluster-sample state))
        (for x in-vector last-cluster)
        (for elt = (for-distance-calculation state x))
        (iterate
          (with result = nil)
          (for cluster in-vector sample)
          (for real-cluster in-vector %cluster-contents)
          (for mean-distance = (mean-distance-to-cluster state
                                                         elt
                                                         cluster))
          (minimize mean-distance into mini)
          (when (= mean-distance mini)
            (setf result real-cluster))
          (finally (vector-push-extend x result)))))))


(defun reassign-data-points-from-subminimal-clusters (state)
  (cl-ds.utils:with-slots-for (state pam-algorithm-state)
    (iterate
      (for count = (cl-ds.utils:swap-if %cluster-contents
                                        (rcurry #'< %minimal-cluster-size)
                                        :key #'length))
      (until (zerop count))
      (reassign-data-points-from-cluster state))))


(defun build-clara-clusters (input-data
                             number-of-medoids
                             metric-fn
                             sample-size
                             sample-count
                             &key
                               (key #'identity)
                               (select-medoids-attempts-count 50)
                               (silhouette-sample-size 500)
                               (silhouette-sample-count 10)
                               (cluster-sample-size 1000)
                               (attempts 0)
                               (minimal-cluster-size 10)
                               split
                               merge)
  (when (emptyp input-data)
    (return-from build-clara-clusters
      (make 'cl-ds.utils.cluster:clustering-result
            :cluster-contents #()
            :silhouette (make-array 0 :element-type 'single-float))))
  (let ((state (make 'clara-algorithm-state
                     :number-of-medoids number-of-medoids
                     :input-data input-data
                     :split-merge-attempts-count attempts
                     :select-medoids-attempts-count select-medoids-attempts-count
                     :split-threshold split
                     :silhouette-sample-size silhouette-sample-size
                     :silhouette-sample-count silhouette-sample-count
                     :cluster-sample-size cluster-sample-size
                     :minimal-cluster-size minimal-cluster-size
                     :key key
                     :metric-fn metric-fn
                     :sample-count sample-count
                     :sample-size sample-size
                     :merge-threshold merge)))
    (cl-ds.utils:with-slots-for (state clara-algorithm-state)
      (iterate
        (repeat %sample-count)
        (draw-clara-sample state)
        (build-pam-clusters state t)
        (update-result-cluster state)
        (cl-progress-bar:update 1))
      (setf %cluster-contents %result-cluster-contents)
      (assert %silhouette)
      state)))
