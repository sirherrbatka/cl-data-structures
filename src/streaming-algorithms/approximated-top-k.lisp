(cl:in-package #:cl-ds.sa)


(defun move-updated (vector position)
  (declare (type (cl-ds.utils:extendable-vector list) vector)
           (type fixnum position)
           (optimize (speed 3) (safety 0) (debug 0)))
  (iterate
    (declare (type fixnum i j))
    (for i from position downto 0)
    (for j from (1- position) downto 0)
    (for front-count = (cdr (aref vector j)))
    (for back-count = (cdr (aref vector i)))
    (if (> (the positive-fixnum back-count) (the positive-fixnum front-count))
        (rotatef (aref vector i) (aref vector j))
        (finish))
    (finally (return vector))))


(cl-ds.alg.meta:define-aggregation-function
    approximated-top-k approximated-top-k-function

    (:range k &key hash-fn width depth key hashes test data-sketch)
    (:range k &key (hash-fn #'sxhash)
     width depth (key #'identity)
     (hashes (ph:make-hash-array width))
     (test 'equal)
     data-sketch)

    (%data-sketch
     (%prototype-data-sketch list)
     (%heap (cl-ds.utils:extendable-vector list))
     (%k fixnum) (%test function))

    ((check-type k integer)
     (cl-ds:check-argument-bounds k (<= 1 k array-total-size-limit))
     (setf %prototype-data-sketch `(:hashes ,hashes
                                    :hash-fn ,hash-fn
                                    :width ,width
                                    :depth ,depth)
           %data-sketch (if (null data-sketch)
                            nil
                            (cl-ds:clone data-sketch))
           %k k
           %test (ensure-function test)
           %heap (make-array k :fill-pointer 0
                               :initial-element '()
                               :element-type 'list)))

    ((element)
     (bind ((fill-pointer (fill-pointer %heap))
            (position (position element %heap :test %test
                                              :key #'car)))
       (declare (type (or null fixnum) position))
       (if (null %data-sketch)
           (cond ((not (null position))
                  (incf (the non-negative-fixnum (cdr (aref %heap position))))
                  (move-updated %heap position))
                 ((< fill-pointer %k)
                  (vector-push-extend (cons element 1) %heap))
                 (t
                  (setf %data-sketch (apply #'clean-sketch
                                            #'approximated-counts
                                            %prototype-data-sketch))
                  (iterate
                    (for (top . count) in-vector %heap)
                    (update-count-min-sketch top %data-sketch count))
                  (update-count-min-sketch element %data-sketch)))
           (let ((count (update-count-min-sketch element %data-sketch)))
             (declare (type non-negative-fixnum count))
             (cond ((not (null position))
                    ;; count is overestimated, so simply incf value in the %heap is likely to be wrong.
                    (setf (cdr (aref %heap position)) count)
                    (move-updated %heap position))
                   ((< fill-pointer %k)
                    (vector-push-extend (cons element count)
                                        %heap)
                    (move-updated %heap fill-pointer))
                   (t
                    (let ((split-point -1)
                          (needs-sort nil))
                      (iterate
                        (declare (type fixnum i))
                        (for i from (1- fill-pointer) downto 0)
                        (for top.old-estimate = (aref %heap i))
                        (for (top . old-estimate) = top.old-estimate)
                        (when (> old-estimate count) (leave))
                        (setf needs-sort t)
                        (for new-estimate = (cl-ds:at %data-sketch top))
                        (setf (cdr top.old-estimate) new-estimate)
                        (when (> count new-estimate)
                          (setf split-point i)
                          (leave)))
                      (iterate
                        (declare (type fixnum i))
                        (for i from 0 below split-point)
                        (for top.old-estimate = (aref %heap i))
                        (for top = (car top.old-estimate))
                        (setf (cdr top.old-estimate) (cl-ds:at %data-sketch top)))
                      (cond ((< split-point 0)
                             (setf %heap (sort %heap #'> :key #'cdr)
                                   (aref %heap (1- fill-pointer)) (cons element count))
                             (move-updated %heap (1- fill-pointer)))
                            (needs-sort
                             (setf %heap (sort %heap #'> :key #'cdr)))))))))))

    ((unless (null %data-sketch)
       (iterate
         (for i from 0 below (length %heap))
         (for top.old-estimate = (aref %heap i))
         (for top = (car top.old-estimate))
         (setf (cdr top.old-estimate) (cl-ds:at %data-sketch top))))
     (cl-ds:whole-range (sort %heap #'> :key #'cdr))))
