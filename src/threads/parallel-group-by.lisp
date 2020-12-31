(cl:in-package #:cl-data-structures.threads)


(defclass parallel-group-by-proxy (cl-ds.alg:proxy-range)
  ((%groups :initarg :groups
            :type hash-table
            :reader read-groups)
   (%having :initarg :having
            :reader read-having)
   (%transform :initarg :transform
               :reader read-transform)
   (%chunk-size :initarg :chunk-size
                :reader read-chunk-size)
   (%maximum-queue-size :initarg :maximum-queue-size
                        :reader read-maximum-queue-size)
   (%key :initarg :key
         :reader read-key)))


(defmethod cl-ds.utils:cloning-information append
    ((range parallel-group-by-proxy))
  '((:groups read-groups)
    (:having read-having)
    (:transform read-transform)
    (:maximum-queue-size read-maximum-queue-size)
    (:chunk-size read-chunk-size)
    (:key read-key)))


(defclass forward-parallel-group-by-proxy (parallel-group-by-proxy
                                           cl-ds:fundamental-forward-range)
  ())


(defclass bidirectional-parallel-group-by-proxy (forward-parallel-group-by-proxy
                                                 cl-ds.alg:bidirectional-proxy-range)
  ())


(defclass random-access-parallel-group-by-proxy (bidirectional-parallel-group-by-proxy
                                                 cl-ds.alg:random-access-proxy-range)
  ())


(defmethod initialize-instance :before ((instance parallel-group-by-proxy)
                                        &key test groups key &allow-other-keys)
  (setf (slot-value instance '%groups) (if (null test)
                                           (copy-hash-table groups)
                                           (make-hash-table :test test))
        (slot-value instance '%key) key))


(defclass parallel-group-by-function (cl-ds.alg.meta:layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric parallel-group-by (range &key test key groups chunk-size maximum-queue-size
                                       having transform)
  (:generic-function-class parallel-group-by-function)
  (:method (range &key
                    (test 'eql) (key #'identity)
                    (chunk-size 16)
                    (having (constantly t))
                    (transform #'identity)
                    (maximum-queue-size 32)
                    (groups (make-hash-table :test test)))
    (cl-ds.alg.meta:apply-range-function range #'parallel-group-by
                                         (list range :test test
                                                     :key key
                                                     :maximum-queue-size maximum-queue-size
                                                     :chunk-size chunk-size
                                                     :having having
                                                     :transform transform
                                                     :groups groups))))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range parallel-group-by-proxy)
                                                  outer-constructor
                                                  (function cl-ds.alg.meta:aggregation-function)
                                                  (arguments list))
  (bind ((groups-prototype (read-groups range))
         (chunk-size (read-chunk-size range))
         (having (ensure-function (read-having range)))
         (transform (ensure-function (read-transform range)))
         (maximum-queue-size (read-maximum-queue-size range))
         (group-by-key (ensure-function (read-key range)))
         (queue (lparallel.queue:make-queue
                 :fixed-capacity maximum-queue-size))
         ((:flet scan-futures (&optional force))
          (iterate
            (until (if force
                       (lparallel.queue:queue-empty-p/no-lock queue)
                       (~> queue
                           lparallel.queue:queue-full-p/no-lock
                           not)))
            (for future = (lparallel.queue:pop-queue/no-lock queue))
            (for result = (lparallel:force future))
            (unless (null result)
              (error result))))
         (outer-fn (call-next-method)))
    (cl-ds.alg.meta:aggregator-constructor
     (cl-ds.alg:read-original-range range)
     (cl-ds.utils:cases ((:variant (eq group-by-key #'identity)))
       (cl-ds.alg.meta:let-aggregator
           ((groups (copy-hash-table groups-prototype)))

           ((element)
            (bind ((selected (~>> element (funcall group-by-key)))
                   (group (gethash selected groups)))
              (when (null group)
                (setf group (list (bt:make-lock)
                                  (vect)
                                  (cl-ds.alg.meta:call-constructor outer-fn))
                      (gethash selected groups) group))
              (bind (((lock buffer aggregator) group))
                (vector-push-extend element buffer)
                (unless (< (fill-pointer buffer) chunk-size)
                  (let ((chunk (copy-array buffer)))
                    (setf (fill-pointer buffer) 0)
                    (lparallel.queue:with-locked-queue queue
                      (scan-futures)
                      (lparallel.queue:push-queue/no-lock
                       (lparallel:future
                         (handler-case
                             (iterate
                               (with cl-ds.alg:*current-key* = selected)
                               (for elt in-vector chunk)
                               (bt:with-lock-held (lock)
                                 (cl-ds.alg.meta:pass-to-aggregation
                                  aggregator elt))
                               (finally (return nil)))
                           (error (e) e)))
                       queue)))))))

           ((let ((result (copy-hash-table groups-prototype)))
              (lparallel.queue:with-locked-queue queue
                (scan-futures t))
              (maphash (lambda (key group &aux (cl-ds.alg:*current-key* key))
                         (bind (((lock buffer aggregator) group))
                           (setf (gethash key result)
                                 (lparallel:future
                                   (handler-case
                                       (iterate
                                         (with cl-ds.alg:*current-key* = key)
                                         (for c in-vector buffer)
                                         (bt:with-lock-held (lock)
                                           (cl-ds.alg.meta:pass-to-aggregation aggregator c))
                                         (finally
                                          (bt:with-lock-held (lock)
                                            (let* ((result (cl-ds.alg.meta:extract-result aggregator))
                                                   (accepted (funcall having result))
                                                   (transformed
                                                     (when accepted
                                                       (funcall transform result))))
                                              (return (list t transformed accepted))))))
                                     (error (e) (list nil e nil)))))))
                       groups)
              (maphash (lambda (key aggregator)
                         (bind (((success value accepted) (lparallel:force aggregator)))
                           (unless success
                             (error value))
                           (if accepted
                               (setf (gethash key result) value)
                               (remhash key result))))
                       result)
              (make-instance 'cl-ds.alg:group-by-result-range
                             :hash-table result
                             :keys (~> result hash-table-keys (coerce 'vector))
                             :begin 0
                             :end (hash-table-count result))))

         (iterate
           (declare (ignorable buffer))
           (for (key group) in-hashtable groups)
           (for (lock buffer aggregator) = group)
           (bt:with-lock-held (lock)
             (cl-ds.alg.meta:cleanup aggregator)))))
     function
     arguments)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:traversable)
                                       (fn parallel-group-by-function)
                                       all)
  (cl-ds.alg:make-proxy range 'forward-parallel-group-by-proxy
                        :groups (getf (rest all) :groups)
                        :having (getf (rest all) :having)
                        :transform (getf (rest all) :transform)
                        :maximum-queue-size (getf (rest all) :maximum-queue-size)
                        :chunk-size (getf (rest all) :chunk-size)
                        :key (getf (rest all) :key)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-forward-range)
                                       (fn parallel-group-by-function)
                                       all)
  (cl-ds.alg:make-proxy range 'forward-parallel-group-by-proxy
                        :groups (getf (rest all) :groups)
                        :maximum-queue-size (getf (rest all) :maximum-queue-size)
                        :having (getf (rest all) :having)
                        :transform (getf (rest all) :transform)
                        :chunk-size (getf (rest all) :chunk-size)
                        :key (getf (rest all) :key)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-bidirectional-range)
                                       (fn parallel-group-by-function)
                                       all)
  (cl-ds.alg:make-proxy range 'bidirectional-parallel-group-by-proxy
                        :having (getf (rest all) :having)
                        :transform (getf (rest all) :transform)
                        :groups (getf (rest all) :groups)
                        :maximum-queue-size (getf (rest all) :maximum-queue-size)
                        :chunk-size (getf (rest all) :chunk-size)
                        :key (getf (rest all) :key)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-random-access-range)
                                       (fn parallel-group-by-function)
                                       all)
  (cl-ds.alg:make-proxy range 'random-access-parallel-group-by-proxy
                        :having (getf (rest all) :having)
                        :transform (getf (rest all) :transform)
                        :groups (getf (rest all) :groups)
                        :maximum-queue-size (getf (rest all) :maximum-queue-size)
                        :chunk-size (getf (rest all) :chunk-size)
                        :key (getf (rest all) :key)))
