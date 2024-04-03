(cl:in-package #:cl-data-structures.streaming-algorithms)


(defclass bloom-filter (fundamental-data-sketch)
  ((%counters :initarg :counters
              :type simple-bit-vector
              :reader internal-array
              :accessor access-counters)
   (%hashes :initarg :hashes
            :type vector
            :accessor access-hashes)
   (%width :initarg :width
           :type fixnum
           :accessor access-width)
   (%depth :initarg :depth
           :type fixnum
           :accessor access-depth)))


(defmethod cl-ds.utils:cloning-information append
    ((sketch bloom-filter))
  '((:counters access-counters)
    (:hashes access-hashes)
    (:depth access-depth)
    (:width access-width)))


(defmethod compatiblep ((first bloom-filter) &rest more)
  (push first more)
  (and (cl-ds.utils:homogenousp more :key #'access-width)
       (cl-ds.utils:homogenousp more :key #'access-depth)
       (cl-ds.utils:homogenousp more :key (compose #'access-counters
                                                   #'array-total-size))
       (cl-ds.utils:homogenousp more :key #'access-hashes
                                     :test #'vector=)))


(defmethod initialize-instance :after ((sketch bloom-filter) &rest all)
  (declare (ignore all))
  (bind (((:slots %width %counters %depth %hashes) sketch))
    (check-type %width integer)
    (check-type %depth integer)
    (check-type %counters (simple-array bit (*)))
    (check-type %hashes ph:hash-array)
    (unless (eql (array-dimension (access-counters sketch) 0)
                 (access-width sketch))
      (error 'cl-ds:incompatible-arguments
             :parameters '(hashes width)
             :values `(,(access-hashes sketch) ,(access-width sketch))
             :format-control "First dimension of the COUNTERS is expected to be equal to WIDTH."))
    (unless (eql (array-dimension %hashes 1)
                 %width)
      (error 'cl-ds:incompatible-arguments
             :parameters '(hashes width)
             :values `(,%hashes ,%width)
             :format-control "First dimension of the hashes is expected to be equal to WIDTH."))))


(defmethod cl-ds:clone ((object bloom-filter))
  (cl-ds.utils:quasi-clone* object
    :counters (~> object access-counters copy-array)))


(defmethod union ((first bloom-filter) &rest more)
  (cl-ds.utils:quasi-clone* first
    :counters (apply #'cl-ds.utils:transform
                     #'max
                     (~> first access-counters copy-array)
                     (mapcar #'access-counters more))))


(defmethod cl-ds:at ((container bloom-filter) location &rest more-locations)
  (unless (endp more-locations)
    (error 'cl-ds:dimensionality-error
           :bounds '(1)
           :value (1+ (length more-locations))
           :format-control "Approximated-counts does not accept more-locations"))
  (iterate
    (with hash = (ldb (byte 32 0)
                      (funcall (access-hash-fn container)
                               location)))
    (with counts = (access-counters container))
    (with hashes = (access-hashes container))
    (with width = (access-width container))
    (with depth = (access-depth container))
    (for j from 0 below width)
    (for value = (aref counts (ph:hashval hashes depth j hash)))
    (when (zerop value)
      (leave (values nil t)))
    (finally (return (values t t)))))


(cl-ds.alg.meta:define-aggregation-function
    bloom-filter bloom-filter-function

  (:range &key hash-fn width depth key hashes data-sketch)
  (:range &key hash-fn width depth (key #'identity) hashes
          (data-sketch
           (clean-sketch
            #'bloom-filter
            :width width
            :depth depth
            :hashes hashes
            :hash-fn hash-fn)))

  (%data-sketch)

  ((check-type data-sketch bloom-filter)
   (setf %data-sketch (cl-ds:clone data-sketch)))

  ((element)
   (iterate
     (declare (type fixnum j width depth)
              (type ph:hash-array hashes))
     (with hash = (ldb (byte 32 0)
                       (funcall (access-hash-fn %data-sketch)
                                element)))
     (with counts = (access-counters %data-sketch))
     (with hashes = (access-hashes %data-sketch))
     (with width = (access-width %data-sketch))
     (with depth = (access-depth %data-sketch))
     (for j from 0 below width)
     (setf (aref counts (ph:hashval hashes depth j hash)) 1)))

  (%data-sketch))


(defmethod clean-sketch ((function bloom-filter-function)
                         &rest all &key hashes hash-fn depth width)
  (declare (ignore all))
  (ensure-functionf hash-fn)
  (check-type depth integer)
  (check-type width integer)
  (check-type hashes (or null ph:hash-array))
  (cl-ds:check-argument-bounds depth (<= 1 depth array-total-size-limit))
  (cl-ds:check-argument-bounds width (<= 1 width array-total-size-limit))
  (make 'bloom-filter
        :counters (make-array depth
                   :initial-element 0
                   :element-type 'bit)
        :hashes (or hashes (ph:make-hash-array width))
        :hash-fn (ensure-function hash-fn)
        :depth depth
        :width width))


(defun bloom-filter-jaccard (a b)
  (check-type a bloom-filter)
  (check-type b bloom-filter)
  (compatiblep a b)
  (let ((a-counters (access-counters a))
        (b-counters (access-counters b)))
    (iterate
      (for i from 0 below (array-total-size a-counters))
      (counting (= 1 (row-major-aref a-counters i) (row-major-aref b-counters i))
                into intersection)
      (counting (>= 1 (+ (row-major-aref a-counters i) (row-major-aref b-counters i)))
                into union)
      (finally (return (- 1.0 (/ intersection union)))))))
