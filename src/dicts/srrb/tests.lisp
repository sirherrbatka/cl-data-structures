(cl:in-package :cl-user)
(defpackage sparse-rrb-vector-tests
  (:use :cl :prove :cl-data-structures.aux-package)
  (:shadowing-import-from :iterate :collecting :summing :in))
(cl:in-package :sparse-rrb-vector-tests)

(plan 383297)

(bind ((vector (make-instance 'cl-ds.dicts.srrb::mutable-sparse-rrb-vector
                              :tail nil))
       ((:values structure status)
        (cl-ds.dicts.srrb::set-in-tail! vector #'cl-ds:add! vector 5 5 nil))
       (tail (cl-ds.dicts.srrb::access-tail vector))
       (tail-mask (cl-ds.dicts.srrb::access-tail-mask vector)))
  (is structure vector)
  (ok (cl-ds:changed status))
  (is (aref tail 5) 5)
  (is tail-mask (ash 1 5)))

(let* ((count 500)
       (container (make-instance 'cl-ds.dicts.srrb::mutable-sparse-rrb-vector))
       (input-data (~>> (cl-ds:iota-range :to count)
                        (cl-ds.alg:zip #'list* (cl-ds:iota-range :to count))
                        cl-ds.alg:to-vector)))
  (iterate
    (for (position . point) in-vector input-data)
    (cl-ds.meta:position-modification
     #'(setf cl-ds:at) container container position :value point))
  (iterate
    (for (position . point) in-vector input-data)
    (is (cl-ds:at container position) point))
  (setf input-data (~>> (cl-ds:iota-range :to count)
                        (cl-ds.alg:zip #'list* (cl-ds.alg:shuffled-range 0 count))
                        cl-ds.alg:to-vector))
  (iterate
    (for (position . point) in-vector input-data)
    (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                      position :value point))
  (iterate
    (for (position . point) in-vector input-data)
    (is (cl-ds:at container position) point))
  (is (cl-ds.dicts.srrb::access-tree-index-bound container)
      (cl-ds.dicts.srrb::scan-index-bound container)))


(let ((shift (cl-ds.dicts.srrb::shift-for-position 47)))
  (is shift 1))


(let ((shift (cl-ds.dicts.srrb::shift-for-position 308)))
  (is shift 1))

(let* ((count 500)
       (input-data (~>> (cl-ds:iota-range :to count)
                        (cl-ds.alg:zip #'list*
                                       (cl-ds.alg:shuffled-range 0
                                                                 count))
                        cl-ds.alg:to-vector))
       (container (cl-ds.dicts.srrb:make-transactional-sparse-rrb-vector)))
  (iterate
    (for (position . point) in-vector input-data)
    (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                      position :value point))
  (iterate
    (for (position . point) in-vector input-data)
    (is (cl-ds:at container position) point))
  (iterate
    (repeat (length input-data))
    (for position = (car (aref input-data 0)))
    (for (values structure status) = (cl-ds.meta:position-modification
                                      #'cl-ds:erase! container container position))
    (is structure container)
    (is (nth-value 1 (cl-ds:at container position)) nil)
    (cl-ds.utils:swapop input-data 0)
    (iterate
      (for (position . point) in-vector input-data)
      (is (cl-ds:at container position) point))))


(let* ((container (make-instance 'cl-ds.dicts.srrb::mutable-sparse-rrb-vector)))
  (declare (optimize (debug 3)))
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    0 :value 1)
  (is (cl-ds:at container 0) 1)
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    456 :value 2)
  (is (cl-ds:at container 0) 1)
  (is (cl-ds:at container 456) 2))

(let* ((count 500)
       (input-data (~>> (cl-ds:iota-range :to count)
                        (cl-ds.alg:zip #'list*
                                       (cl-ds.alg:shuffled-range 0
                                                                 count))
                        cl-ds.alg:to-vector))
       (container (make-instance 'cl-ds.dicts.srrb::mutable-sparse-rrb-vector)))
  (declare (optimize (debug 3)))
  (iterate
    (for (position . point) in-vector input-data)
    (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                      position :value point))
  (iterate
    (for (position . point) in-vector input-data)
    (is (cl-ds:at container position) point))
  (iterate
    (repeat (length input-data))
    (for position = (car (aref input-data 0)))
    (for (values structure status) = (cl-ds.meta:position-modification
                                      #'cl-ds:erase! container container position))
    (is structure container)
    (ok (cl-ds:changed status))
    (is (nth-value 1 (cl-ds:at container position)) nil)
    (cl-ds.utils:swapop input-data 0)
    (iterate
      (for (position . point) in-vector input-data)
      (is (cl-ds:at container position) point))))

(let ((container (make-instance 'cl-ds.dicts.srrb::mutable-sparse-rrb-vector)))
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    800 :value 5)
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    1024 :value 0)
  (is (cl-ds:at container 800) 5)
  (is (cl-ds:at container 1024) 0)
  (is (cl-ds:size container) 2)
  (cl-ds.meta:position-modification
   #'cl-ds:erase! container container 800)
  (is (cl-ds:at container 800) nil)
  (is (cl-ds:size container) 1)
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    800 :value 5)
  (is (cl-ds:size container) 2)
  (is (cl-ds:at container 1024) 0))


(let ((container (make-instance 'cl-ds.dicts.srrb::mutable-sparse-rrb-vector)))
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    82 :value 1)
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    53 :value 2)
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    417 :value 3)
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    114 :value 4)
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    0 :value 5)
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    486 :value 6)
  (is (cl-ds:at container 82) 1)
  (is (cl-ds:at container 53) 2)
  (is (cl-ds:at container 417) 3)
  (is (cl-ds:at container 114) 4)
  (is (cl-ds:at container 0) 5)
  (is (cl-ds:at container 486) 6)
  (cl-ds.meta:position-modification #'cl-ds:erase! container container
                                    82)
  (is (cl-ds:at container 114) 4))

(let ((container (make-instance 'cl-ds.dicts.srrb::mutable-sparse-rrb-vector)))
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    1024 :value 5)
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    5 :value 0)
  (is (cl-ds:size container) 2)
  (is (cl-ds:at container 1024) 5)
  (is (cl-ds:at container 5) 0)
  (cl-ds.meta:position-modification
   #'cl-ds:erase! container container 5)
  (is (cl-ds:at container 5) nil)
  (is (cl-ds:size container) 1)
  (is (cl-ds:at container 1024) 5)
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    2000 :value 10)
  (is (cl-ds:size container) 2)
  (is (cl-ds:at container 2000) 10)
  (is (cl-ds:at container 5) nil))

(let ((container (make-instance 'cl-ds.dicts.srrb::mutable-sparse-rrb-vector)))
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    1024 :value 5)
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    1023 :value 0)
  (is (cl-ds:size container) 2)
  (is (cl-ds:at container 1024) 5)
  (is (cl-ds:at container 1023) 0)
  (cl-ds.meta:position-modification
   #'cl-ds:erase! container container 1023)
  (is (cl-ds:at container 1023) nil)
  (is (cl-ds:size container) 1)
  (is (cl-ds:at container 1024) 5)
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    1023 :value 5)
  (is (cl-ds:size container) 2)
  (is (cl-ds:at container 1023) 5)
  (is (cl-ds:at container 1024) 5))

(let* ((count 500)
       (input-data (~>> (cl-ds:iota-range :to count)
                        (cl-ds.alg:zip #'list*
                                       (cl-ds.alg:shuffled-range 0
                                                                 count))
                        cl-ds.alg:to-vector))
       (container (make-instance 'cl-ds.dicts.srrb::transactional-sparse-rrb-vector
                                 :ownership-tag (cl-ds.common.abstract:make-ownership-tag))))
  (iterate
    (for (position . point) in-vector input-data)
    (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                      position :value point))
  (iterate
    (for (position . point) in-vector input-data)
    (is (cl-ds:at container position) point)))

(let* ((count 500)
       (input-data (~>> (cl-ds:iota-range :to count)
                        (cl-ds.alg:zip #'list* (cl-ds:iota-range))
                        cl-ds.alg:to-vector))
       (container (make-instance 'cl-ds.dicts.srrb::functional-sparse-rrb-vector)))
  (iterate
    (for (position . point) in-vector input-data)
    (setf container (cl-ds.meta:position-modification #'cl-ds:insert
                                                      container container
                                                      position :value point)))
  (iterate
    (for (position . point) in-vector input-data)
    (is (cl-ds:at container position) point))
  (setf input-data (~>> (cl-ds.alg:shuffled-range 0 count)
                        (cl-ds.alg:zip #'list*
                                       (cl-ds.alg:shuffled-range 0
                                                                 count))
                        cl-ds.alg:to-vector))
  (iterate
    (for (position . point) in-vector input-data)
    (setf container (cl-ds.meta:position-modification #'cl-ds:insert
                                                      container container
                                                      position :value point))
    (is (cl-ds.dicts.srrb::access-tree-index-bound container)
        (cl-ds.dicts.srrb::scan-index-bound container)))
  (iterate
    (for (position . point) in-vector input-data)
    (is (cl-ds:at container position) point)))

(let* ((count 500)
       (input-data (~>> (cl-ds:iota-range :to count)
                        (cl-ds.alg:zip #'list*
                                       (cl-ds.alg:shuffled-range 0
                                                                 count))
                        cl-ds.alg:to-vector))
       (container (make-instance 'cl-ds.dicts.srrb::functional-sparse-rrb-vector)))
  (declare (optimize (debug 3)))
  (iterate
    (for (position . point) in-vector input-data)
    (setf container (cl-ds.meta:position-modification #'cl-ds:insert
                                                      container container
                                                      position :value point)))
  (iterate
    (for (position . point) in-vector input-data)
    (is (cl-ds:at container position) point))
  (cl-ds:traverse container
                  (lambda (index.value)
                    (is (cl-ds:at container (car index.value))
                        (cdr index.value)))))

(let* ((count 500)
       (input-data (~>> (cl-ds:iota-range :to count)
                        (cl-ds.alg:zip #'list*
                                       (cl-ds.alg:shuffled-range 0
                                                                 count))
                        cl-ds.alg:to-vector))
       (container (make-instance 'cl-ds.dicts.srrb::mutable-sparse-rrb-vector)))
  (declare (optimize (debug 3)))
  (diag "Testing insert.")
  (iterate
    (for (position . point) in-vector input-data)
    (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                      position :value point))
  (setf container (cl-ds:become-functional container))
  (iterate
    (for (position . point) in-vector input-data)
    (is (cl-ds:at container position) point))
  (diag "Testing erasing.")
  (iterate
    (repeat (length input-data))
    (for position = (car (aref input-data 0)))
    (for (values structure status) = (cl-ds.meta:position-modification
                                      #'cl-ds:erase container container position))
    (setf container structure)
    (ok (cl-ds:changed status))
    (is (nth-value 1 (cl-ds:at container position)) nil)
    (cl-ds.utils:swapop input-data 0)
    (iterate
      (for (position . point) in-vector input-data)
      (is (cl-ds:at container position) point))))

(let* ((container (make-instance 'cl-ds.dicts.srrb::mutable-sparse-rrb-vector)))
  (declare (optimize (debug 3)))
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    32 :value 32)
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    64 :value 64)
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    512 :value 512)
  (is (cl-ds:at container 32) 32)
  (is (cl-ds:at container 64) 64)
  (is (cl-ds:at container 512) 512)
  )

(let* ((container (make-instance 'cl-ds.dicts.srrb::mutable-sparse-rrb-vector)))
  (declare (optimize (debug 3)))
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    1 :value 1)
  (cl-ds.meta:position-modification #'(setf cl-ds:at) container container
                                    64 :value 64)
  (is (cl-ds:at container 1) 1)
  (is (cl-ds:at container 64) 64)
  )

(finalize)
