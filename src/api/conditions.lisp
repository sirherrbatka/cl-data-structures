(cl:in-package #:cl-data-structures)


(defgeneric print-condition (object stream))


(define-condition textual-error (simple-error
                                 more-conditions:chainable-condition)
  ()
  (:report print-condition))


(defmethod print-condition ((condition textual-error) stream)
  (when-let ((control (simple-condition-format-control condition)))
    (apply #'format stream control
           (simple-condition-format-arguments condition)))
  (more-conditions:maybe-print-cause stream condition t t))


(define-condition initialization-error (more-conditions:reference-condition
                                        textual-error)
  ((%class :initarg :class
           :reader read-class)))


(define-condition operation-not-allowed (more-conditions:reference-condition
                                         textual-error)
  ())


(defmethod print-condition ((condition initialization-error) stream)
  (format stream "During initialization of ~a:~%"
          (read-class condition))
  (call-next-method))


(define-condition invalid-value (more-conditions:reference-condition
                                 textual-error)
  ((%value :initarg :value
           :reader read-value)))


(define-condition argument-error (textual-error)
  ((%argument :type symbol
              :initarg :parameter
              :initarg :argument
              :reader read-argument)))


(define-condition invalid-argument (more-conditions:reference-condition
                                    argument-error)
  ())



(define-condition unexpected-argument (invalid-argument)
  ())


(define-condition incompatible-arguments
    (textual-error more-conditions:incompatible-arguments)
  ())


(define-condition out-of-bounds (invalid-value)
  ((%bounds :initarg :bounds
            :reader read-bounds)))


(define-condition invalid-argument-value (invalid-value
                                          argument-error)
  ())


(define-condition argument-value-out-of-bounds (invalid-argument-value
                                                out-of-bounds)
  ())


(define-condition not-in-allowed-set (out-of-bounds)
  ())


(define-condition argument-value-not-in-allowed-set (invalid-argument-value
                                                     not-in-allowed-set)
  ())


(define-condition empty-container (textual-error)
  ())


(define-condition dimensionality-error (out-of-bounds)
  ())


(define-condition too-many-dimensions (dimensionality-error)
  ())


(define-condition file-releated-error (textual-error)
  ((%path :initarg :path
          :initform nil
          :reader path)))


(defmethod print-condition ((condition file-releated-error) stream)
  (unless (null (path condition))
    (format stream "Error when processing file ~a:~%" (path condition)))
  (call-next-method))


(defmethod print-condition ((condition dimensionality-error) stream)
  (format stream "Dimensionality is ~a.~%" (read-bounds condition))
  (call-next-method))


(defmethod print-condition ((condition argument-value-out-of-bounds) stream)
  (if (slot-boundp condition '%argument)
      (format stream "Argument ~a has value ~a which is out of bounds ~a.~%"
              (read-argument condition)
              (read-value condition)
              (read-bounds condition))
      (format stream "Value ~a is out of bounds ~a.~%"
              (read-value condition)
              (read-bounds condition)))
  (call-next-method))


(defmethod print-condition ((condition not-in-allowed-set) stream)
  (let ((value (read-value condition))
        (bounds (read-bounds condition))
        (i 20))
    (block out
      (format stream "Value ~a is not in the set: " value)
      (map nil
           (lambda (x)
             (if (zerop (decf i))
                 (return-from out nil)
                 (format stream "~A " x)))
           bounds))
    (unless (zerop i)
      (format stream "(...)"))
    (format stream "~%"))
  (call-next-method))


(defmethod print-condition ((condition argument-error) stream)
  (format stream "Problem with the argument: ~a.~%"
          (read-argument condition))
  (call-next-method))


(defmethod print-condition ((condition unexpected-argument) stream)
  (format stream "Argument ~A was not expected.~%"
          (read-argument condition))
  (call-next-method))


(define-condition initialization-out-of-bounds (initialization-error
                                                argument-value-out-of-bounds)
  ())


(define-condition not-implemented (simple-error)
  ())


(defmethod print-condition ((condition invalid-value) stream)
  (format stream "Invalid value: ~a.~%" (read-value condition))
  (call-next-method))
