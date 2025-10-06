(cl:in-package #:cl-data-structures.streaming-algorithms)


(cl-ds.alg.meta:define-aggregation-function
    simhash simhash-function

  (:range &key hash-fn)
  (:range &key hash-fn)

  (%counters %hash-fn)

  ((setf %hash-fn hash-fn
         %counters (make-array 64 :element-type '(unsigned-byte 32))))

  ((element)
   (iterate
     (declare (type (unsigned-byte 64) hash)
              (type fixnum i)
              (type (simple-array fixnum (64)) %counters))
     (with hash = (ldb (byte 64 0) (funcall %hash-fn element)))
     (for i from 0 below 64)
     (if (ldb-test (byte 1 i) hash)
         (incf (aref %counters i))
         (decf (aref %counters i)))))

  ((iterate
     (declare (type fixnum i)
              (type (unsigned-byte 32) counter)
              (type (unsigned-byte 64) result)
              (type integer result))
     (with result = 0)
     (for i from 0 below 64)
     (for counter = (aref %counters i))
     (setf (ldb (byte 1 i) result) (the bit (clamp counter 0 1)))
     (finally (return result)))))
