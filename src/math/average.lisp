(cl:in-package #:cl-data-structures.math)


(cl-ds.alg.meta:define-aggregation-function
    average average-function
    (:range &key key sum count after weight)
    (:range &key (key #'identity) (sum 0) (count 0) (after #'identity) (weight (constantly 1)))
    ((%sum number) (%count integer))
    ((setf %sum sum
           %weight weight
           %count count))
    ((element)
     (check-type element number)
     (let ((weight (funcall %weight cl-ds.alg.meta:aggregated-element)))
       (incf %count weight)
       (incf %sum (* element weight))))
    ((/ %sum %count)))


(cl-ds.alg.meta:define-aggregation-function
    harmonic-average harmonic-average-function
    (:range &key key count sum after)
    (:range &key (key #'identity) (after #'identity) (count 0) (sum 0))
    ((%sum number) (%count integer) (%zero boolean))
    ((setf %sum sum
           %count count
           %zero nil))
    ((element)
     (check-type element number)
     (incf %count)
     (if (zerop element)
         (setf %zero t)
         (incf %sum (/ 1 element))))
    ((if %zero
         0.0
         (/ %count %sum))))


(cl-ds.alg.meta:define-aggregation-function
    geometric-average geometric-average-function
    (:range &key key count total after)
    (:range &key (key #'identity) (count 0) (total nil) (after #'identity))
    ((%total (or null number)) (%count integer))
    ((setf %total total
           %count count))
    ((element)
     (check-type element number)
     (unless (zerop element)
       (setf element (log element 10))
       (incf %count)
       (if (null %total)
           (setf %total element)
           (incf %total element))))
    ((if (zerop %count)
       0
       (expt 10 (/ %total %count)))))


(cl-ds.alg.meta:define-aggregation-function
    array-average array-average-function
    (:range &key key sum count after)
    (:range &key (key #'identity) (after #'identity) (sum nil) (count 0))
    ((%sum (or array null)) (%count integer))
    ((setf %count count
           %sum (if (null sum)
                    nil
                    (copy-array sum))))
    ((element)
     (check-type element array)
     (incf %count)
     (when (null %sum)
       (setf %sum (make-array (array-dimensions element)
                              :initial-element 0.0)))
     (iterate
       (for i from 0 below (array-total-size %sum))
       (incf (row-major-aref %sum i) (row-major-aref element i))))
    ((iterate
       (for i from 0 below (array-total-size %sum))
       (setf (row-major-aref %sum i) (/ (row-major-aref %sum i)
                                        %count))
       (finally (return %sum)))))


(cl-ds.alg.meta:define-aggregation-function
    array-geometric-average array-geometric-average-function
    (:range &key key total count after)
    (:range &key (key #'identity) (after #'identity) (total nil) (count 0))
    ((%total (or array null)) (%count integer))
    ((setf %count count
           %total (if (null total)
                    nil
                    (copy-array total))))
    ((element)
     (check-type element array)
     (incf %count)
     (if (null %total)
         (setf %total (copy-array element))
         (iterate
           (for i from 0 below (array-total-size %total))
           (setf (row-major-aref %total i)
                 (* (row-major-aref %total i)
                    (row-major-aref element i))))))
    ((iterate
       (for i from 0 below (array-total-size %total))
       (setf (row-major-aref %total i) (expt (row-major-aref %total i)
                                             (/ 1 %count)))
       (finally (return %total)))))


(cl-ds.alg.meta:define-aggregation-function
    array-harmonic-average array-harmonic-average-function
    (:range &key key sum count)
    (:range &key (key #'identity) (sum nil) (count 0))
    ((%sum (or array null))
     (%count integer)
     (%zeros (or simple-array null)))
    ((setf %count count
           %zeros (if (null sum)
                      nil
                      (make-array (array-dimensions sum)
                                  :element-type 'boolean
                                  :initial-element nil))
           %sum (if (null sum)
                    nil
                    (copy-array sum))))
    ((element)
     (check-type element array)
     (incf %count)
     (when (null %zeros)
       (setf %zeros (make-array (array-dimensions element)
                                :element-type 'boolean
                                :initial-element nil)))
     (when (null %sum)
       (setf %sum (setf %sum (make-array (array-dimensions element)
                                         :initial-element 0.0))))
     (iterate
       (for i from 0 below (array-total-size %sum))
       (if (zerop (row-major-aref element i))
           (setf (row-major-aref %zeros i) t)
           (incf (row-major-aref %sum i)
                 (/ 1 (row-major-aref element i))))))
    ((iterate
       (for i from 0 below (array-total-size %sum))
       (if (row-major-aref %zeros i)
           (setf (row-major-aref %sum i) 0)
           (setf (row-major-aref %sum i) (/ %count
                                            (row-major-aref %sum i))))
       (finally (return %sum)))))
