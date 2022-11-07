(cl:in-package #:cl-data-structures.algorithms)


(defclass group-by-proxy (proxy-range)
  ((%groups :initarg :groups
            :type hash-table
            :reader read-groups)
   (%having :initarg :having
            :reader read-having)
   (%transform :initarg :transform
               :reader read-transform)
   (%key :initarg :key
         :reader read-key)))


(defclass group-by-result-range (hash-table-range)
  ())


(defmethod cl-ds.utils:cloning-information append
    ((range group-by-proxy))
  '((:groups read-groups)
    (:transform read-transform)
    (:having read-having)
    (:key read-key)))


(defclass forward-group-by-proxy (group-by-proxy
                                  fundamental-forward-range)
  ())


(defclass bidirectional-group-by-proxy (forward-group-by-proxy
                                        bidirectional-proxy-range)
  ())


(defclass random-access-group-by-proxy (bidirectional-group-by-proxy
                                        random-access-proxy-range)
  ())


(defmethod initialize-instance :before ((instance group-by-proxy)
                                        &key test groups key &allow-other-keys)
  (setf (slot-value instance '%groups) (if (null test)
                                           (copy-hash-table groups)
                                           (make-hash-table :test test))
        (slot-value instance '%key) key))


(defclass group-by-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric group-by (range &key test key groups transform having)
  (:generic-function-class group-by-function)
  (:method (range &key (test 'eql) (key #'identity) (groups (make-hash-table :test test))
                       (transform #'identity) (having (constantly t)))
    (apply-range-function range #'group-by
                          (list range :test test :key key :groups groups
                                :transform transform :having having))))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range group-by-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (declare (optimize (speed 3) (safety 0) (debug 0)
                     (space 0) (compilation-speed 0)))
  (bind ((groups-prototype (read-groups range))
         (group-by-key (ensure-function (read-key range)))
         (transform (ensure-function (read-transform range)))
         (accept (ensure-function (read-having range)))
         (outer-fn (call-next-method)))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.utils:cases ((:variant (eq group-by-key #'identity)))
       (cl-ds.alg.meta:let-aggregator
           ((groups (copy-hash-table groups-prototype)))

           ((element)
            (bind ((selected (~>> element (funcall group-by-key)))
                   (*current-key* selected)
                   (group (gethash selected groups)))
              (when (null group)
                (setf group (cl-ds.alg.meta:call-constructor outer-fn)
                      (gethash selected groups) group))
              (handler-case
                  (cl-ds.alg.meta:pass-to-aggregation group element)
                (cl-ds.alg.meta:early-aggregation-exit nil))))

           ((maphash (lambda (key aggregator &aux (*current-key* key))
                       (let ((result (cl-ds.alg.meta:extract-result aggregator)))
                         (if (funcall accept result)
                             (setf (gethash key groups) (funcall transform result))
                             (remhash key groups))))
                     groups)
             (make-instance 'group-by-result-range
                            :hash-table groups
                            :keys (~> groups hash-table-keys (coerce 'vector))
                            :begin 0
                            :end (hash-table-count groups)))

         (iterate
           (for (key group) in-hashtable groups)
           (cl-ds.alg.meta:cleanup group))))
     function
     arguments)))


(defmethod apply-layer ((range cl-ds:traversable)
                        (fn group-by-function)
                        all)
  (make-proxy range 'forward-group-by-proxy
              :groups (getf (rest all) :groups)
              :key (getf (rest all) :key)
              :having (getf (rest all) :having)
              :transform (getf (rest all) :transform)))


(defmethod apply-layer ((range fundamental-forward-range)
                        (fn group-by-function)
                        all)
  (make-proxy range 'forward-group-by-proxy
              :groups (getf (rest all) :groups)
              :transform (getf (rest all) :transform)
              :having (getf (rest all) :having)
              :key (getf (rest all) :key)))


(defmethod apply-layer ((range fundamental-bidirectional-range)
                        (fn group-by-function)
                        all)
  (make-proxy range 'bidirectional-group-by-proxy
              :groups (getf (rest all) :groups)
              :transform (getf (rest all) :transform)
              :having (getf (rest all) :having)
              :key (getf (rest all) :key)))


(defmethod apply-layer ((range fundamental-random-access-range)
                        (fn group-by-function)
                        all)
  (make-proxy range 'random-access-group-by-proxy
              :groups (getf (rest all) :groups)
              :transform (getf (rest all) :transform)
              :having (getf (rest all) :having)
              :key (getf (rest all) :key)))
