(cl:in-package #:cl-data-structures.streaming-algorithms)


(defclass approximated-set-cardinality (fundamental-data-sketch)
  ((%registers :initarg :registers
               :accessor access-registers)))


(defmethod cl-ds.utils:cloning-information append
    ((sketch approximated-set-cardinality))
  '((:registers access-registers)))


(defmethod cl-ds:clone ((object approximated-set-cardinality))
  (cl-ds.utils:quasi-clone* object
    :registers (~> object access-registers copy-array)))


(defmethod compatiblep ((first-sketch approximated-set-cardinality)
                         &rest more-sketches)
  (push first-sketch more-sketches)
  (cl-ds.utils:homogenousp more-sketches :key #'class-of))


(defmethod union ((first approximated-set-cardinality) &rest more)
  (cl-ds.utils:quasi-clone* first
    :registers (apply #'hll:union
                      (access-registers first)
                      (mapcar #'access-registers more))))


(defmethod initialize-instance :after ((object approximated-set-cardinality)
                                       &rest all)
  (declare (ignore all))
  (check-type (access-registers object) hll:sketch))


(defmethod cl-ds:value ((state approximated-set-cardinality))
  (~> state access-registers hll:cardinality))


(cl-ds.alg.meta:define-aggregation-function
  approximated-set-cardinality approximated-set-cardinality-function

  (:range &key hash-fn key data-sketch)
  (:range &key (hash-fn #'sxhash) (key #'identity)
        (data-sketch
         (clean-sketch #'approximated-set-cardinality
                       :hash-fn hash-fn)))

  (%data-sketch)

  ((check-type data-sketch approximated-set-cardinality)
   (setf %data-sketch (cl-ds:clone data-sketch)))

  ((element)
   (bind (((:slots %hash-fn %registers) %data-sketch)
          (hash-fn (ensure-function %hash-fn))
          (hash (ldb (byte 64 0) (funcall hash-fn element))))
     (declare (optimize (speed 3) (debug 0) (safety 1) (space 0)))
     (hll:add-hash %registers hash)))

  (%data-sketch))


(defmethod clean-sketch ((function approximated-set-cardinality-function)
                         &rest all &key hash-fn)
  (declare (ignore all))
  (ensure-functionf hash-fn)
  (make 'approximated-set-cardinality
        :registers (hll:new-sketch)
        :hash-fn hash-fn))


(defun hyperloglog-jaccard (a b)
  (check-type a approximated-set-cardinality)
  (check-type b approximated-set-cardinality)
  (hll:jaccard (access-registers a) (access-registers b)))

