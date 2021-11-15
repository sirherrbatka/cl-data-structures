(cl:in-package #:cl-data-structures.math)


(cl-ds:define-validation-for-fields
    (chi-squared-function (:test :classes))
  (:test :optional t
         :type '(or symbol function)
         :default 'eql)
  (:classes :optional nil
            :type 'sequence))


(defun path-list (fields &aux (result (vect)))
  (labels ((impl (path &optional (fl fields))
             (if (endp fl)
                 (let ((rpath (reverse path)))
                   (vector-push-extend rpath result))
                 (iterate
                   (for class in-sequence (cl-ds:at (first fl) :classes))
                   (impl (cons class path) (rest fl))))))
    (impl nil)
    result))


(defun sum-plane (counts axis position)
  (iterate
    (with generator = (cl-ds.utils:cycle-over-address (array-dimensions counts)
                                                      axis
                                                      position))
    (for adr = (funcall generator))
    (until (null adr))
    (sum (apply #'aref counts adr))))


(defun chi-square-marginal-counts (counts)
  (iterate
    (with dimensions = (array-dimensions counts))
    (with result = (make-array (length dimensions)))
    (for dim in dimensions)
    (for axis from 0)
    (for marginal = (make-array dim :element-type 'fixnum))
    (iterate
      (for position from 0 below dim)
      (for sum = (sum-plane counts axis position))
      (setf (aref marginal position) sum))
    (setf (aref result axis) marginal)
    (finally (return result))))


(defun independent-counts (marginal-counts)
  (let* ((total-counts (map 'vector (curry #'reduce #'+) marginal-counts))
         (frequencies (map 'vector
                           (lambda (marginal-count total-count)
                             (map 'vector
                                  (lambda (x) (/ x total-count))
                                  marginal-count))
                           marginal-counts
                           total-counts))
         (expected-counts (cl-ds.utils:cartesian-table frequencies #'*))
         (unfolded-counts (cl-ds.utils:unfold-table expected-counts)))
    (assert (every (curry #'= (first-elt total-counts)) total-counts))
    (map-into unfolded-counts
              (curry #'* (first-elt total-counts))
              unfolded-counts)
    expected-counts))


(eval-always
  (define-constant +epsilon+ 1.19e-07 :test '=))


(defun gamma-function-incomplete (a x)
  (labels ((gamma*aux (a z)
             (do ((n    0.0 (1+ n))
                  (term 1.0
                        (/ (* term (- z))
                           (+ n 1.0)))
                  (sum  0.0))
                 ((progn (setq sum (+ sum (/ term (+ a n))))
                         (< (abs (/ term (+ a n))) +epsilon+))
                  sum)))
           (impl (a x)
             (if (< a 1.0)
                 (/ (+ (impl (+ a 1.0) x)
                       (* (expt x a) (exp (- x))))
                    a)
                 (* (expt x a) (gamma*aux a x)))))
    (impl a x)))


(defun chi-squared-pval (deegress-of-freedom chi-squared)
  (- 1.0 (gamma-function-incomplete (/ deegress-of-freedom 2)
                                    (/ chi-squared 2))))


(defun chi-squared-on-table (&key fields class-counts &allow-other-keys)
  (let* ((marginal-counts (chi-square-marginal-counts class-counts))
         (independent-counts (independent-counts marginal-counts))
         (generator (cl-ds.utils:cycle-over-address (array-dimensions class-counts))))
    (assert (equal (array-dimensions class-counts)
                   (array-dimensions independent-counts)))
    (iterate
      (for adr = (funcall generator))
      (until (null adr))
      (for expected = (apply #'aref independent-counts adr))
      (for existing = (apply #'aref class-counts adr))
      (sum (/ (expt (- expected existing) 2)
              expected)
           into result)
      (finally
       (return (chi-squared-pval
                (reduce #'*
                        fields
                        :initial-value 1.0
                        :key (compose #'1-
                                      #'length
                                      (rcurry #'cl-ds:at :classes)))
                result))))))


(cl-ds.alg.meta:define-aggregation-function
    chi-squared
    chi-squared-function

    (:range cell-value fields &key key)
    (:range cell-value fields &key (key #'identity))

    (%fields-counts %fields %field-mapping %full-path-vector %full-address-vector %state
     %cell-value)

    ((&key key fields cell-value &allow-other-keys)
     (setf %field-mapping (map 'vector
                               (lambda (x)
                                 (make-hash-table :test (cl-ds:at x :test)
                                                  :size (~> x (cl-ds:at :classes) length)))
                               fields))
     (iterate
       (for f in fields)
       (for mapping in-vector %field-mapping)
       (iterate
         (for class in-sequence (cl-ds:at f :classes))
         (for class-number from 0)
         (setf (gethash class mapping) class-number)))
     (setf %fields-counts (mapcar (compose #'length
                                           (rcurry #'cl-ds:at :classes))
                                  fields)
           %cell-value cell-value
           %fields fields
           %full-path-vector (path-list fields)
           %state (make-array %fields-counts :element-type 'fixnum
                                            :initial-element 0)
           %full-address-vector (map 'vector
                                     (lambda (x)
                                       (map 'list (flip #'gethash) %field-mapping x))
                                     %full-path-vector)))

    ((element)
     (iterate
       (for path in-vector %full-path-vector)
       (for address in-vector %full-address-vector)
       (for old-value = (apply #'aref %state address))
       (apply #'(setf aref)
              (+ old-value (apply %cell-value element path))
              %state
              address)))

    ((chi-squared-on-table :fields %fields :class-counts %state)))
