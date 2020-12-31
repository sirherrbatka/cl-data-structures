(cl:in-package #:cl-data-structures.math)


(defun initialize-fields (fields data)
  (mapcar (curry #'initialize-field data) fields))


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
      (until (eq field future-field))
      (setf (cl-ds.utils:mref result
                              (read-name field)
                              (read-name future-field))
            (calculate-mutual-information-between field
                                                  future-field)))
    (finally (assert (>= result 0)) (return result))))


(defun partition-points (length split-points-count)
  "How to divide vector into equal partitions?"
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
  "Can't calculate mutual information for continues variables, so we will divide whole range into segments."
  (bind ((split-points-count (cl-ds:at field :split-points-count))
         (sorted (~> (map 'vector (cl-ds:at field :key) data)
                     (sort #'<)))
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

(defun mutual-information-hash-table (field fields)
  (let ((result (make-hash-table :test 'eq)))
    (iterate
      (for f in fields)
      (setf (gethash (read-name f) result)
            (calculate-mutual-information-between field f))
      (assert (>= (gethash (read-name f) result) 0)))
    result))


(cl-ds.alg.meta:define-aggregation-function
    mutual-information mutual-information-function

    (:range fields &key key)
    (:range fields &key (key #'identity))

    (%data %fields)

    ((cl-ds:validate-fields #'mutual-information fields)
     (setf %data (vect)
           %fields fields))

    ((element)
     (vector-push-extend element %data))

    ((~> (mutual-information-hash-table
          (initialize-field %data (first %fields))
          (initialize-fields (rest %fields) %data))
         cl-ds.alg:make-hash-table-range)))


(cl-ds.alg.meta:define-aggregation-function
    mutual-information-matrix mutual-information-matrix-function

    (:range fields &key key)
    (:range fields &key (key #'identity))

    (%data %fields)

    ((cl-ds:validate-fields #'mutual-information-matrix fields)
     (setf %data (vect)
           %fields fields))

    ((element)
     (vector-push-extend element %data))

    ((~> (initialize-fields %fields %data)
         calculate-mutual-information)))


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
                 :accessor access-split-point)
   (%original-selector-function :initarg :original-selector-function
                                :reader read-original-selector-function)
   (%discrete-values-set :initarg :discrete-values-set
                         :reader read-discrete-values-set)))


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


(defun initialize-field (data field)
  (bind ((original-data data)
         ((:values data split-points-count)
          (if (continuesp field)
              (discrete-form field data)
              (values data nil)))) ; split-points-count is ignored. Maybe i should check if it was passed anyway and signal error in such case?
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


(defun initialize-split-point-field (data field)
  (bind ((original-data data)
         ((:values data split-points-count)
          (if (continuesp field)
              (discrete-form field data)
              (values data nil)))
         (result
          (make 'split-point-field
                :name (cl-ds:at field :name)
                :test (if (continuesp field)
                          'eql
                          'equal)
                :original-data original-data
                :original-selector-function (cl-ds:at field :key)
                :discrete (not (continuesp field))
                :data data))
         (selector-function (if (continuesp field)
                                #'identity
                                (cl-ds:at field :key))))
    (setf (slot-value result '%discrete-values-set)
          (if (continuesp field)
              (coerce (iota split-points-count) 'vector)
              (~> (map 'vector selector-function data)
                  (remove-duplicates :test #'equal)))

          (slot-value result '%selector-function)
          (lambda (x)
            (equal (aref (read-discrete-values-set result)
                         (access-split-point result))
                   (funcall selector-function x)))

          (slot-value result '%split-point-count)
          (length (read-discrete-values-set result)))
    result))


(cl-ds.alg.meta:define-aggregation-function
    harmonic-average-mutual-information
    harmonic-average-mutual-information-function

    (:range fields &key key)
    (:range fields &key (key #'identity))

    (%data %field %comparative-fields)

    ((cl-ds:validate-fields #'harmonic-average-mutual-information
                            fields)
     (setf %data (vect)
           %field (first fields)
           %comparative-fields (rest fields)))

    ((element)
     (vector-push-extend element %data))

    ((let* ((vector %data)
            (result (mutual-information-hash-table
                     (initialize-field vector %field)
                     (initialize-fields %comparative-fields vector)))
            (sum 0)
            (count 0))
       (declare (type (cl-ds.utils:extendable-vector t) vector))
       (maphash (lambda (key value)
                  (declare (ignore key))
                  (unless (zerop value)
                    (incf sum (/ 1 value))
                    (incf count)))
                result)
       (/ count sum))))


(defun calculate-split-point (reference-field matched-field)
  (iterate
    (with result = nil)
    (for i from 0 below (read-split-point-count matched-field))
    (setf (access-split-point matched-field) i)
    (for table = (mutual-information-hash-table reference-field
                                                (list matched-field)))
    (for mi = (gethash (read-name matched-field) table))
    (maximize mi into maximum)
    (when (= mi maximum)
      (setf result (cons (aref (read-discrete-values-set matched-field)
                               (access-split-point matched-field))
                         mi)))
    (finally
     (unless (read-discrete matched-field)
       (setf (car result)
             (~> (read-discrete-values-set matched-field)
                 (aref (car result))
                 (position (read-data matched-field))
                 (aref (read-original-data matched-field) _)
                 (funcall (read-original-selector-function matched-field) _))))
     (return result))))


(cl-ds.alg.meta:define-aggregation-function
    optimal-split-point optimal-split-point-function

    (:range fields &key key)
    (:range fields &key (key #'identity))

    (%data %matched-fields %reference-field)

    ((setf %data (vect)
           %matched-fields (rest fields)
           %reference-field (first fields))
     (cl-ds:validate-fields #'optimal-split-point %matched-fields)
     (cl-ds:validate-field #'optimal-split-point %reference-field))

    ((element)
     (vector-push-extend element %data))

    ((let ((vector %data))
       (declare (type (cl-ds.utils:extendable-vector t) vector))
       (iterate
         (with reference-field = (initialize-field vector %reference-field))
         (with result = (make-hash-table :test 'eq))
         (for matched-field in %matched-fields)
         (for initialized-field =
              (initialize-split-point-field vector matched-field))
         (for (value . mi)  = (calculate-split-point reference-field
                                                     initialized-field))
         (setf (gethash (read-name initialized-field) result)
               (cons value mi))
         (finally (return (cl-ds.alg:make-hash-table-range result)))))))


(defmacro define-all-validation-for-fields (&rest class-list)
  `(progn
     ,@(mapcar (lambda (class)
                 `(cl-ds:define-validation-for-fields
                      (,class (:name :type :key :split-points-count))
                    (:name :optional nil)
                    (:key :optional t
                          :default #'identity)
                    (:split-points-count :optional t
                                         :default 10
                                         :type 'positive-integer)
                    (:type :optional nil
                           :member (:discrete :continues))))
               class-list)))


(define-all-validation-for-fields
    optimal-split-point-function
    mutual-information-function
    mutual-information-matrix-function
    harmonic-average-mutual-information-function)
