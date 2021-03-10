(cl:in-package #:cl-data-structures.streaming-algorithms)


(defclass approximated-counts (fundamental-data-sketch)
  ((%counters :initarg :counters
              :type (simple-array non-negative-fixnum (* *))
              :accessor access-counters
              :reader internal-array)
   (%hashes :initarg :hashes
            :type vector
            :accessor access-hashes)
   (%depth :initarg :depth
           :accessor access-depth)
   (%width :initarg :width
           :accessor access-width)))


(defmethod union ((first approximated-counts) &rest more)
  (cl-ds.utils:quasi-clone* first
    :counters (apply #'cl-ds.utils:transform
                     #'max
                     (~> first access-counters copy-array)
                     (mapcar #'access-counters more))))


(defmethod compatiblep ((first approximated-counts) &rest more)
  (push first more)
  (and (cl-ds.utils:homogenousp more :key #'access-depth)
       (cl-ds.utils:homogenousp more :key #'access-width)
       (cl-ds.utils:homogenousp more :key #'access-hashes
                                     :test #'vector=)))


(defmethod initialize-instance :after ((object approximated-counts)
                                       &rest all)
  (declare (ignore all))
  (check-type (access-width object) integer)
  (check-type (access-depth object) integer)
  (check-type (access-hashes object) (simple-array non-negative-fixnum (* 2)))
  (check-type (access-counters object) (simple-array non-negative-fixnum (* *)))
  (unless (eql (array-dimension (access-counters object) 0)
               (access-width object))
    (error 'cl-ds:incompatible-arguments
           :parameters '(hashes space)
           :values `(,(access-hashes object) ,(access-width object))
           :format-control "First dimension of the COUNTERS is expected to be equal to WIDTH."))
  (unless (eql (array-dimension (access-counters object) 1)
               (access-depth object))
    (error 'cl-ds:incompatible-arguments
           :parameters '(hashes space)
           :values `(,(access-hashes object) ,(access-depth object))
           :format-control "Second dimension of the COUNTERS is expected to be equal to DEPTH.")))


(defmethod cl-ds.utils:cloning-information append
  ((sketch approximated-counts))
  '((:counters access-counters)
    (:hashes access-hashes)
    (:width access-width)
    (:depth access-depth)))


(defmethod cl-ds:clone ((sketch approximated-counts))
  (cl-ds.utils:quasi-clone* sketch
    :counters (~> sketch access-counters copy-array)))


(defmethod cl-ds:at ((container approximated-counts)
                     location &rest more-locations)
  (unless (endp more-locations)
    (error 'cl-ds:dimensionality-error :bounds '(1)
                                       :value (1+ (length more-locations))
                                       :format-control "Approximated-counts does not accept more-locations"))
  (iterate
    (with hash = (funcall (access-hash-fn container)
                          location))
    (with counts = (access-counters container))
    (with hashes = (access-hashes container))
    (with width = (access-width container))
    (with depth = (access-depth container))
    (for j from 0 below width)
    (minimize (aref counts j (ph:hashval hashes depth j hash))
              into min)
    (finally (return (values min t)))))


(defun update-count-min-sketch (location data-sketch &optional (count 1))
  (iterate
    (with hash = (funcall (access-hash-fn data-sketch)
                          location))
    (with counts = (access-counters data-sketch))
    (with hashes = (access-hashes data-sketch))
    (with width = (access-width data-sketch))
    (with depth = (access-depth data-sketch))
    (for j from 0 below width)
    (minimize (incf (aref counts j (ph:hashval hashes depth j hash))
                    count))))


(cl-ds.alg.meta:define-aggregation-function
    approximated-counts approximated-counts-function

    (:range &key hash-fn depth width key hashes data-sketch)
    (:range &key (hash-fn #'sxhash) depth width (key #'identity) hashes
          (data-sketch (clean-sketch
                        #'approximated-counts
                        :hashes hashes
                        :hash-fn hash-fn
                        :depth depth
                        :width width)))

    (%data-sketch)

    ((check-type data-sketch approximated-counts)
     (setf %data-sketch (cl-ds:clone data-sketch)))

    ((element)
     (update-count-min-sketch element %data-sketch))

    (%data-sketch))


(defmethod clean-sketch ((function approximated-counts-function)
                         &rest all &key hashes hash-fn depth width)
  (declare (ignore all))
  (check-type depth integer)
  (check-type width integer)
  (check-type hashes (or null (simple-array non-negative-fixnum (* 2))))
  (cl-ds:check-argument-bounds depth (> depth 0))
  (cl-ds:check-argument-bounds width (> width 0))
  (make 'approximated-counts
        :counters (make-array
                   `(,width ,depth)
                   :initial-element 0
                   :element-type 'non-negative-fixnum)
        :hashes (or hashes (ph:make-hash-array width))
        :hash-fn (ensure-function hash-fn)
        :depth depth
        :width width))


(defun approximated-counts-distance (a-sketch b-sketch)
  (check-type a-sketch approximated-counts)
  (check-type b-sketch approximated-counts)
  (assert (compatiblep a-sketch b-sketch)
          (a-sketch b-sketch)
          (make-condition 'cl-ds:incompatible-arguments
                          :parameters '(a-sketch b-sketch)
                          :values `(,a-sketch ,b-sketch)
                          :format-control "Sketches passed to the count-min-sketches-distance are not compatible."))
  (flet ((counter-mean (counter line)
           (iterate
             (with length = (array-dimension counter 1))
             (for i from 0 below length)
             (sum (aref counter line i) into total)
             (finally (return (/ total length))))))
    (iterate
      (with a-counters = (access-counters a-sketch))
      (with b-counters = (access-counters b-sketch))
      (with line-size = (array-dimension a-counters 1))
      (for i from 0 below (array-dimension a-counters 0))
      (for score = (iterate
                     (for j from 0 below line-size)
                     (for a = (aref a-counters i j))
                     (for b = (aref b-counters i j))
                     (sum (sqrt (* a b)))))
      (for a-mean = (counter-mean a-counters i))
      (for b-mean = (counter-mean b-counters i))
      (maximize (~>> (* a-mean b-mean line-size line-size)
                     sqrt (/ 1) (* score) (- 1) sqrt
                     (coerce _ 'single-float))))))
