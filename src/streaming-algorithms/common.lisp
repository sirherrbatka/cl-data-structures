(cl:in-package #:cl-data-structures.streaming-algorithms)


(defclass fundamental-data-sketch ()
  ((%hash-fn :initarg :hash-fn
             :accessor access-hash-fn)))


(defmethod cl-ds.utils:cloning-information append
    ((sketch fundamental-data-sketch))
  '((:hash-fn access-hash-fn)))


(defmethod initialize-instance :after ((sketch fundamental-data-sketch)
                                       &rest all)
  (declare (ignore all))
  (ensure-functionf (access-hash-fn sketch)))


(defgeneric compatiblep (first-sketch &rest more-sketches)
  (:method :around ((a fundamental-data-sketch) &rest more-sketches)
    (unless (every (curry #'eq (class-of a))
                   (mapcar #'class-of more-sketches))
      (return-from compatiblep nil))
    (unless (every (curry #'eq (access-hash-fn a))
                   (mapcar #'access-hash-fn more-sketches))
      (warn "Hashing function objects in the sketches mismatches. This may be a problem…"))
    (call-next-method)))


(defgeneric clean-sketch (function &rest arguments &key &allow-other-keys))


(defgeneric union (first-sketch &rest more-sketches)
  (:method :around ((sketch fundamental-data-sketch) &rest more-sketches)
    (unless (apply #'compatiblep sketch more-sketches)
      (error 'cl-ds:incompatible-arguments
             :parameters '(sketch more-sketches)
             :values `(,sketch ,more-sketches)
             :format-control "Sketches passed to the union are not compatible."))
    (call-next-method)))


(defgeneric internal-array (sketch))
