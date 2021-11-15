(cl:defpackage #:cl-data-structures.streaming-algorithms.polynomial-hashing
  (:use #:cl #:cl-data-structures.aux-package)
  (:export
   #:hashval-no-depth
   #:hashval
   #:hash-array
   #:hash
   #:+max-64-bits+
   #:make-hash-array))

(cl:in-package #:cl-data-structures.streaming-algorithms.polynomial-hashing)


(eval-always
  (define-constant +long-prime+ 4294967311)
  (define-constant +max-64-bits+ #xFFFFFFFFFFFFFFFF))


(deftype hash ()
  `(integer 0 ,+long-prime+))


(deftype hash-array ()
  `(simple-array hash (* 2)))


(-> hashval-no-depth (hash-array fixnum (unsigned-byte 64)) hash)
(defun hashval-no-depth (hashes j hash)
  (declare (optimize (speed 3) (safety 0))
           (type hash-array hashes)
           (type non-negative-fixnum j hash))
  (~> (aref hashes j 0)
      (* hash)
      (ldb (byte 32 0) _)
      (+ (aref hashes j 1))
      (ldb (byte 32 0) _)
      (rem +long-prime+)))


(-> hashval (hash-array positive-fixnum non-negative-fixnum (unsigned-byte 64)) hash)
(defun hashval (hashes depth j hash)
  (declare (type hash-array hashes)
           (type non-negative-fixnum depth j hash))
  (~> (hashval-no-depth hashes j hash)
      (rem depth)))


(defun make-hash-array (count)
  (iterate
    (with result = (~> (list count 2)
                       (make-array :element-type 'hash)))
    (for i from 0 below count)
    (setf (aref result i 0) (random-in-range 1 +long-prime+)
          (aref result i 1) (random-in-range 0 +long-prime+))
    (finally (return result))))
