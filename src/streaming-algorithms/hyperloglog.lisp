(cl:defpackage #:cl-data-structures.streaming-algorithms.hyperloglog
  (:use #:cl #:cl-data-structures.aux-package)
  (:shadow cl:union)
  (:export #:intersection-cardinality
           #:cardinality
           #:jaccard
           #:add-hash
           #:new-sketch
           #:register
           #:sketch
           #:union))

(cl:in-package #:cl-data-structures.streaming-algorithms.hyperloglog)

#|
This is loglog-beta to be specific.
|#

(eval-always
  (define-constant +p+ 14)
  (define-constant +m+ (ash 1 +p+))
  (define-constant +max+ (- 64 +p+))
  (define-constant +maxx+ (ldb (byte +p+ 0) #xFFFFFFFFFFFFFFFF))
  (define-constant +alpha+ (/ 0.7213d0 (1+ (/ 1.079d0 +m+))))
  (define-constant +q+ 6)
  (define-constant +r+ 10)
  (define-constant +2q+ (ash 1 +q+))
  (define-constant +2r+ (ash 1 +r+))
  (define-constant +c+  0.169919487159739093975315012348d0))


(deftype register ()
  `(unsigned-byte 16))


(deftype sketch ()
  `(simple-array register (,+m+)))


(-> new-sketch () sketch)
(defun new-sketch ()
  (make-array +m+ :element-type 'register))


(-> beta ((double-float 0.0d0)) double-float)
(declaim (inline beta))
(defun beta (ez)
  (declare (optimize (speed 3) (safety 0)))
  (let ((zl (log (1+ ez))))
    (declare (type double-float ez)
             (type (double-float 0.0d0 *) zl))
    (+ (* -0.370393911d0 ez)          (* 0.070471823d0 zl)
       (* 0.17393686d0   (expt zl 2)) (* 0.16339839d0  (expt zl 3))
       (* -0.09237745d0  (expt zl 4)) (* 0.03738027d0  (expt zl 5))
       (* -0.005384159d0 (expt zl 6)) (* 0.00042419d0  (expt zl 7)))))


(-> lz (register) (unsigned-byte 8))
(defun lz (register)
  (declare (optimize (speed 3)))
  (ash register #.(- +q+ 16)))


(-> new-register ((unsigned-byte 8) (unsigned-byte 16)) register)
(defun new-register (rank sig)
  (declare (optimize (speed 3)))
  (logior (ash rank +r+) sig))


(-> reg-sum-and-zeroes ((simple-array register (*))) (values double-float double-float))
(declaim (inline reg-sum-and-zeroes))
(defun reg-sum-and-zeroes (registers)
  (declare (optimize (speed 3) (safety 0)))
  (iterate
    (declare (type fixnum i)
             (type double-float sum ez))
    (with sum = 0.0d0)
    (with ez = 0.0d0)
    (for i from 0 below (length registers))
    (for val = (aref registers i))
    (for lz = (lz val))
    (when (zerop lz) (incf ez))
    (incf sum (/ 1.0d0 (expt 2.0d0 lz)))
    (finally (return (values sum ez)))))


(-> hll-rank ((unsigned-byte 64)) (unsigned-byte 8))
(declaim (inline hll-rank))
(defun hll-rank (value)
  (declare (type (unsigned-byte 64) value)
           (optimize (speed 3)))
  (- 65
     (~>> (ash value +p+) (logxor +maxx+)
          (ldb (byte 64 0))
          integer-length)))


(-> hash-shifts ((unsigned-byte 64)) (unsigned-byte 16))
(declaim (inline hash-shifts))
(defun hash-shifts (y)
  (declare (optimize (speed 3)))
  (ldb (byte 16 #.(- 64 +r+)) y))


(-> add-hash (sketch (unsigned-byte 64)) sketch)
(defun add-hash (sketch x)
  (declare (optimize (speed 3)))
  (maxf (aref sketch (ash x #.(- +max+)))
        (new-register (hll-rank x) (hash-shifts x)))
  sketch)


(-> cardinality (sketch) double-float)
(declaim (inline cardinality))
(defun cardinality (sketch)
  (declare (optimize (speed 3)))
  (bind (((:values sum ez) (reg-sum-and-zeroes sketch)))
    (declare (type double-float sum ez))
    (/ (* #.(* +alpha+ +m+) (- +m+ ez))
       (+ (beta ez) sum))))


(defun union (sketch &rest more-sketches)
  (iterate
    (with result = (new-sketch))
    (for s in (cons sketch more-sketches))
    (iterate
      (declare (type fixnum i))
      (for i from 0 below +m+)
      (maxf (aref result i) (aref s i)))
    (finally (return result))))


(-> expected-collisions ((double-float 0.0d0) (double-float 0.0d0)) double-float)
(declaim (inline expected-collisions))
(defun expected-collisions (n m)
  (iterate
    (declare (type (double-float 0.0d0) x b1 b2 power den b2d
                   b1d prx pry)
             (type fixnum i)
             (optimize (speed 3) (safety 0)))
    (for i from 1 to +2Q+)
    (with den = 0.0d0)
    (with power = 0.0d0)
    (with b2d = 0.0d0)
    (with b1d = 0.0d0)
    (with prx = 0.0d0)
    (with pry = 0.0d0)
    (with x = 0.0d0)
    (with b1 = 0.0d0)
    (with b2 = 0.0d0)
    (setf power (+ (if (= i +2Q+) (+ +p+ +r+ i -1) (+ +p+ +r+ i)) 0.0d0)
          den (expt 2.0d0 power))
    (iterate
      (declare (type fixnum j)
               (type double-float prx pry))
      (for j from 1 to +2R+)
      (if (not (= i +2Q+))
          (setf b1 (/ (+ +2R+ j) den)
                b2 (/ (+ +2R+ j 1) den))
          (setf b1 (/ j den)
                b2 (/ (1+ j) den)))
      (setf b2d (- 1.0d0 b2)
            b1d (- 1.0d0 b1)
            prx (- (expt b2d n)
                   (expt b1d n))
            pry (- (expt b2d m)
                   (expt b1d m)))
      (incf x (* prx pry)))
    (finally (return (+ 0.5d0 (* x +p+))))))


(-> approximated-expected-collisions (double-float double-float) double-float)
(declaim (inline approximated-expected-collisions))
(defun approximated-expected-collisions (first second)
  (bind (((:values n m) (if (< first second)
                            (values second first)
                            (values first second))))
    (cond ((> n #.(expt 2 (+ +r+ (expt 2 +q+))))
           most-positive-double-float)
          ((> n #.(expt 2 (+ +p+ 5)))
           (let* ((d (/ (* 4 (/ n m))
                        (expt (/ (1+ n) m) 2))))
             (+ 0.5d0 (* d #.(* +c+ (expt 2 (- +p+ +r+)))))))
          (t (/ (expected-collisions n m)
                +p+)))))


(-> jaccard (sketch sketch) double-float)
(defun jaccard (a b)
  (declare (optimize (speed 3) (safety 0)))
  (let ((c 0) (n 0))
    (declare (type fixnum c n))
    (iterate
      (declare (type fixnum i)
               (type register ea eb))
      (for i from 0 below +m+)
      (for ea = (aref a i))
      (for eb = (aref b i))
      (if (= ea eb)
          (unless (zerop ea)
            (incf c))
          (incf n)))
    (when (= c 0)
      (return-from jaccard 1.0d0))
    (let* ((c1 (cardinality a))
           (c2 (cardinality b))
           (ec (approximated-expected-collisions c1 c2)))
      (declare (type double-float c1 c2 ec))
      (if (< c ec)
          1.0d0
          (- 1.0d0 (/ (- c ec) n))))))


(-> intersection-cardinality (sketch sketch) double-float)
(defun intersection-cardinality (a b)
  (~> (union a b) cardinality (* (jaccard a b)) (+ 0.5d0)))
