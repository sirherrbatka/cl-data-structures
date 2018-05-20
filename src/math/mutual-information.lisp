(in-package #:cl-data-structures.math)


(defclass mutual-information-fundamental-function ()
  ())


(defclass mutual-information-matrix-function (cl-ds.alg.meta:multi-aggregation-function
                                              mutual-information-fundamental-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass mutual-information-function (cl-ds.alg.meta:multi-aggregation-function
                                       mutual-information-fundamental-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass harmonic-average-mutual-information-function (cl-ds.alg.meta:multi-aggregation-function
                                                        mutual-information-fundamental-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass optimal-split-point-function (cl-ds.alg.meta:multi-aggregation-function
                                        mutual-information-fundamental-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric mutual-information (range field &rest comparative-fields)
  (:generic-function-class mutual-information-function)
  (:method (range field &rest comparative-fields)
    (cl-ds:validate-fields #'mutual-information (cons field comparative-fields))
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'mutual-information
                                               :field field
                                               :comparative-fields comparative-fields
                                               :key #'identity)))


(defgeneric mutual-information-matrix (range &rest fields)
  (:generic-function-class mutual-information-matrix-function)
  (:method (range &rest fields)
    (cl-ds:validate-fields #'mutual-information-matrix fields)
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'mutual-information-matrix
                                               :fields fields
                                               :key #'identity)))


(defgeneric harmonic-average-mutual-information (range field &rest fields)
  (:generic-function-class harmonic-average-mutual-information-function)
  (:method (range field &rest fields)
    (cl-ds:validate-fields #'harmonic-average-mutual-information (cons field fields))
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'harmonic-average-mutual-information
                                               :field field
                                               :comparative-fields fields
                                               :key #'identity)))


(defgeneric optimal-split-point (range reference-field &rest matched-fields)
  (:generic-function-class optimal-split-point-function)
  (:method (range reference-field &rest matched-fields)
    (cl-ds:validate-field #'optimal-split-point reference-field)
    (cl-ds:validate-fields #'optimal-split-point matched-fields)
    (cl-ds.alg.meta:apply-aggregation-function range
                                               #'optimal-split-point
                                               :key #'identity
                                               :reference-field reference-field
                                               :matched-fields matched-fields)))


(defclass info-field ()
  ((%name :initarg :name
          :reader read-name)
   (%data :initarg :data
          :reader read-data)
   (%original-data :initarg :original-data
                   :reader read-original-data)
   (%discrete :initarg :discrete
              :reader read-discrete)
   (%split-point-count :initarg :split-point-count
                       :reader read-split-point-count)
   (%test :initarg :test
          :reader read-test)
   (%selector-function :initarg :selector-function
                       :reader read-selector-function
                       :initform #'identity)))


(defclass split-point-field (info-field)
  ((%split-point :initarg :split-point
                 :accessor access-split-point)))


(defun continuesp (field)
  (eq (cl-ds:at field :type)
      :continues))


(defun initialize-mutual-information-hash-tables (field1 field2)
  (bind ((table1 (make-hash-table :test (read-test field1)))
         (table2 (make-hash-table :test (read-test field2)))
         (table3 (make-hash-table :test 'equal))
         (vector1 (read-data field1))
         (vector2 (read-data field2))
         (length (length vector1))
         ((:dflet normalize-table (table))
          (iterate
            (for (key value) in-hashtable table)
            (setf (gethash key table) (/ value length)))))
    (assert (eql length (length vector2)))
    (iterate
      (with function1 = (read-selector-function field1))
      (with function2 = (read-selector-function field2))
      (for v1 in-vector vector1)
      (for v2 in-vector vector2)
      (for value1 = (funcall function1 v1))
      (for value2 = (funcall function2 v2))
      (incf (gethash value1 table1 0))
      (incf (gethash value2 table2 0))
      (incf (gethash (cons value1 value2) table3 0)))
    (normalize-table table1)
    (normalize-table table2)
    (normalize-table table3)
    (values table1 table2 table3)))


(defun calculate-mutual-information-between (field1 field2)
  (bind (((:values table1 table2 table3)
          (initialize-mutual-information-hash-tables field1 field2)))
    (iterate
      (for (key p3) in-hashtable table3)
      (for p1 = (gethash (car key) table1))
      (for p2 = (gethash (cdr key) table2))
      (for p = (* p3 (log (/ p3 (* p1 p2)) 2)))
      (sum p))))


(defun calculate-mutual-information (fields)
  (iterate
    (with result = (cl-ds.utils:make-half-matrix
                    'single-float
                    (length fields)
                    :query-key (iterate
                                 (with table = (make-hash-table))
                                 (for field in fields)
                                 (for i from 0)
                                 (setf (gethash (read-name field) table)
                                       i)
                                 (finally (return (rcurry #'gethash table))))))
    (for field in fields)
    (iterate
      (for future-field in fields)
      (unless (eq field future-field)
        (setf (cl-ds.utils:mref result (read-name field) (read-name future-field))
              (calculate-mutual-information-between field future-field))))
    (finally (assert (>= result 0)) (return result))))


(defun partition-points (length split-points-count)
  (bind ((number-of-points (min length split-points-count))
         (shift (/ length number-of-points))
         (result (make-array split-points-count
                             :element-type 'fixnum
                             :adjustable t
                             :fill-pointer 0)))
    (iterate
      (for i from 0 below number-of-points)
      (for offset = (~> (* i shift) round (min length)))
      (vector-push-extend offset result)
      (finally (return result)))))


(defun discrete-form (field data)
  (bind ((split-points-count (cl-ds:at field :split-points-count))
         (sorted (~>> data
                      (map 'vector (or (cl-ds:at field :key) #'identity))
                      (sort _ #'<)))
         (partition-points (partition-points (length data)
                                             split-points-count))
         (key (or (cl-ds:at field :key) #'identity))
         (partitions (map-into (make-array (length partition-points)
                                           :adjustable t
                                           :fill-pointer 0)
                               (curry #'aref sorted)
                               partition-points)))
    (values (map '(vector fixnum)
                 (lambda (x) (cl-ds.utils:lower-bound partitions
                                                      (funcall key x)
                                                      #'<))
                 data)
            (length partition-points))))


(defun initialize-field (data field)
  (bind ((original-data data)
         ((:values data split-points-count)
          (if (continuesp field)
              (discrete-form field data)
              (values data (cl-ds:at field :split-points-count)))))
    (make 'info-field
          :name (cl-ds:at field :name)
          :test (if (continuesp field)
                    'eql
                    'equal)
          :original-data original-data
          :split-point-count split-points-count
          :discrete (not (continuesp field))
          :data data
          :selector-function (if (continuesp field)
                                 #'identity
                                 (cl-ds:at field :key)))))


(defun construct-split-point (sorted-data field i)
  (lret ((result (make 'split-point-field
                       :name (read-name field)
                       :test (read-test field)
                       :discrete (read-discrete field)
                       :original-data (read-original-data field)
                       :split-point-count (read-split-point-count field)
                       :split-point i
                       :data (copy-array sorted-data)
                       :selector-function #'identity)))
    (map-into (read-data result)
              (compose (curry #'<= i) (read-selector-function field))
              (read-data result))))


(defun initialize-fields (fields data)
  (mapcar (curry #'initialize-field data) fields))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function mutual-information-matrix-function)
     &rest all
     &key key fields
     &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :vector (range &rest all)
          (declare (ignore all))
          (cl-ds.alg:to-vector range :key key :force-copy nil))

        (lambda (&key vector &allow-other-keys)
          (declare (type vector vector))
          (~> (initialize-fields fields vector)
              calculate-mutual-information))))


(defun mutual-information-hash-table (field fields)
  (let ((result (make-hash-table :test 'eq)))
    (iterate
      (for f in fields)
      (setf (gethash (read-name f) result)
            (calculate-mutual-information-between field f))
      (assert (>= (gethash (read-name f) result) 0)))
    result))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function mutual-information-function)
     &rest all
     &key key field comparative-fields
     &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :vector (range &rest all)
          (declare (ignore all))
          (cl-ds.alg:to-vector range :key key :force-copy nil))

        (lambda (&key vector &allow-other-keys)
          (declare (type vector vector))
          (~> (mutual-information-hash-table
               (initialize-field vector field)
               (initialize-fields comparative-fields vector))
              cl-ds.alg:make-hash-table-range))))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function harmonic-average-mutual-information-function)
     &rest all
     &key key field comparative-fields
     &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :vector (range &rest all)
          (declare (ignore all))
          (cl-ds.alg:to-vector range :key key :force-copy nil))

        (lambda (&key vector &allow-other-keys)
          (declare (type vector vector))
          (let ((result (mutual-information-hash-table
                         (initialize-field vector field)
                         (initialize-fields comparative-fields vector)))
                (sum 0)
                (count 0))
            (maphash (lambda (key value)
                       (declare (ignore key))
                       (unless (zerop value)
                         (incf sum (/ 1 value))
                         (incf count)))
                     result)
            (/ count sum)))))


(defun calculate-split-point (reference-field matched-field)
  (iterate
    (with result = nil)
    (with sorted-data = (if (read-discrete matched-field)
                            (copy-array (read-data matched-field))
                            (sort-new (read-data matched-field)
                                      #'<
                                      :key (read-selector-function matched-field))))
    (for i from 0 below (read-split-point-count matched-field))
    (for split-point = (construct-split-point sorted-data matched-field i))
    (until (null split-point))
    (for table = (mutual-information-hash-table reference-field
                                                (list split-point)))
    (for mi = (gethash (read-name split-point) table))
    (maximize mi into maximum)
    (when (= mi maximum)
      (setf result (cons split-point mi)))
    (finally (return result))))


(defmethod cl-ds.alg.meta:multi-aggregation-stages
    ((function optimal-split-point-function)
     &rest all
     &key reference-field matched-fields key
     &allow-other-keys)
  (declare (ignore all))
  (list (cl-ds.alg.meta:stage :vector (range &rest all)
          (declare (ignore all))
          (cl-ds.alg:to-vector range :key key :force-copy nil))

        (lambda (&key vector &allow-other-keys)
          (declare (type vector vector))
          (iterate
            (with reference-field = (initialize-field vector reference-field))
            (with result = (make-hash-table :test 'eq))
            (for matched-field in matched-fields)
            (for initialized-field = (initialize-field vector matched-field))
            (for (field . mi) = (calculate-split-point reference-field initialized-field))
            (for sorted-data = (~> field
                                   read-original-data
                                   (sort #'<
                                         :key (read-selector-function initialized-field))))
            (for value = (aref sorted-data (~> (length sorted-data)
                                               (/ (read-split-point-count field))
                                               (* (access-split-point field))
                                               round
                                               (min (length sorted-data)))))
            (setf (gethash (read-name initialized-field) result)
                  (cons (~> initialized-field
                            read-selector-function
                            (funcall value))
                        mi))
            (finally (return (cl-ds.alg:make-hash-table-range result)))))))


(cl-ds:define-validation-for-fields
    (mutual-information-fundamental-function (:name :type :key))
  (:name :optional nil)
  (:key :optional t
        :default #'identity)
  (:split-points-count :optional t
                       :default 10
                       :type 'positive-integer)
  (:type :optional nil
         :member (:discrete :continues)))
