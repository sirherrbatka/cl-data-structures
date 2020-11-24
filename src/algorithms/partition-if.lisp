(cl:in-package #:cl-data-structures.algorithms)


(defclass abstract-partition-if-proxy (proxy-range)
  ((%key :initarg :key
         :reader read-key)
   (%on-first :initarg :on-first
              :reader read-on-first)
   (%test :initarg :test
          :reader read-test)))


(defclass forward-abstract-partition-if-proxy
    (abstract-partition-if-proxy
     fundamental-forward-range)
  ())


(defclass partition-if-proxy ()
  ((%chunks :initform (vect)
            :reader read-chunks)))


(defclass forward-partition-if-proxy
    (partition-if-proxy forward-abstract-partition-if-proxy)
  ((%collected :initarg :collected
               :accessor access-collected)
   (%orginal-collected :initarg :collected
                       :reader read-original-collected))
  (:default-initargs :collected (cl-ds.seqs.rrb:make-functional-rrb-vector)))


(defmethod cl-ds:reset! ((range forward-partition-if-proxy))
  (setf (access-collected range) (read-original-collected range))
  (call-next-method))



(defmethod clone ((range abstract-partition-if-proxy))
  (make (class-of range)
        :original-range (~> range read-original-range clone)
        :collected (access-collected range)
        :on-first (read-on-first range)
        :key (read-key range)
        :test (read-test range)))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range partition-if-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (declare (optimize (speed 3) (safety 0)))
  (bind ((on-first (read-on-first range))
         (test (ensure-function (read-test range)))
         (partition-key (ensure-function (read-key range)))
         (collected (access-collected range))
         (outer-fn (call-next-method)))
    (assert (functionp outer-fn))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.utils:cases ((:variant (eq partition-key #'identity))
                         (:variant (eq test #'eq)
                                   (eq test #'eql)
                                   (eq test #'equal)
                                   (eq test #'string=)
                                   (eq test #'=)
                                   (eq test #'equalp)))
       (cl-ds.alg.meta:let-aggregator
           ((chunks (cl-ds.alg:to-vector collected))
            (inner (cl-ds.alg.meta:call-constructor outer-fn)))

           ((element)
             (let* ((chunks-length (fill-pointer chunks))
                    (last-chunk (the fixnum (1- chunks-length)))
                    (key (funcall partition-key element))
                    (empty (zerop chunks-length)))
               (if empty
                   (vector-push-extend element chunks)
                   (bind ((old (~>> (if on-first 0 last-chunk)
                                    (aref chunks)))
                          (old-key (funcall partition-key old)))
                     (if (funcall test old-key key)
                         (vector-push-extend element chunks)
                         (let ((old-chunks chunks)
                               (*current-key* old-key))
                           (setf chunks (vect element))
                           (~>> old-chunks cl-ds:whole-range
                                (cl-ds.alg.meta:pass-to-aggregation inner))))))))

           ((unless (emptyp chunks)
              (let* ((length (length chunks))
                     (*current-key* (~>> (if on-first 0 (1- length))
                                         (aref chunks)
                                         (funcall partition-key))))
                (~>> chunks cl-ds:whole-range
                     (cl-ds.alg.meta:pass-to-aggregation inner))))
            (cl-ds.alg.meta:extract-result inner))))

     function
     arguments)))


(defclass partition-if-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric partition-if (range test &key key on-first)
  (:generic-function-class partition-if-function)
  (:method (range test &key (key #'identity) (on-first nil))
    (ensure-functionf test key)
    (apply-range-function range #'partition-if
                          (list range test
                                :key key
                                :on-first on-first))))


(defmethod apply-layer ((range traversable)
                        (fn partition-if-function)
                        all)
  (make-proxy range 'forward-partition-if-proxy
              :key (getf (cddr all) :key)
              :on-first (getf (cddr all) :on-first)
              :test (second all)))


(defun partition-if-consume-implementation (collected key
                                            on-first test
                                            value)
  (bind ((collected-size (cl-ds:size collected)))
    (if (zerop collected-size)
        (progn
          (cl-ds:put! collected value)
          collected)
        (let ((key-value (funcall key value))
              (current-key
                (~>> (if on-first 0 (1- collected-size))
                     (cl-ds:at collected)
                     (funcall key))))
          (if (funcall test key-value current-key)
              (progn
                (cl-ds:put! collected value)
                collected)
              (let ((old-collected (cl-ds:replica collected t)))
                (cl-ds:put! old-collected value)
                (cl-ds:reset! collected)
                old-collected))))))


(defmethod cl-ds:consume-front ((range forward-partition-if-proxy))
  (bind ((key (read-key range))
         (on-first (read-on-first range))
         (original-range (read-original-range range))
         (collected (~> range access-collected
                        become-transactional))
         (test (read-test range))
         ((:values result more)
          (iterate
            (for (values value more) = (cl-ds:consume-front original-range))
            (unless more
              (let ((data-size (cl-ds:size collected)))
                (if (zerop data-size)
                    (leave (values nil nil))
                    (let ((result (~> collected
                                      (cl-ds:replica t)
                                      cl-ds:whole-range)))
                      (cl-ds:reset! collected)
                      (leave (values result t))))))
            (for data =
                 (partition-if-consume-implementation collected
                                                      key
                                                      on-first
                                                      test
                                                      value))
            (unless (eq data collected)
              (leave (values (cl-ds:whole-range data) t))))))
    (setf (access-collected range) (become-functional collected))
    (values result more)))


(defmethod cl-ds:peek-front ((range forward-partition-if-proxy))
  (bind ((key (read-key range))
         (on-first (read-on-first range))
         (collected (~> range access-collected
                        become-transactional))
         (test (read-test range)))
    (cl-ds:across (read-original-range range)
                  (lambda (value)
                    (let ((data (partition-if-consume-implementation collected
                                                                     key
                                                                     on-first
                                                                     test
                                                                     value)))
                      (unless (eq data collected)
                        (return-from cl-ds:peek-front
                          (values (cl-ds:whole-range data) t))))))
    (if (~> collected cl-ds:size (= 0))
        (values nil nil)
        (values (cl-ds:whole-range collected) t))))


(defmethod cl-ds:traverse ((range forward-partition-if-proxy)
                           function)
  (ensure-functionf function)
  (iterate
    (for (values value more) = (cl-ds:consume-front range))
    (while more)
    (funcall function value)
    (finally (return range))))


(defmethod cl-ds:across ((range forward-partition-if-proxy)
                         function)
  (ensure-functionf function)
  (bind ((key (read-key range))
         (on-first (read-on-first range))
         (collected (~> range access-collected
                        become-transactional))
         (test (read-test range)))
    (cl-ds:across (read-original-range range)
                  (lambda (value)
                    (let ((data (partition-if-consume-implementation collected
                                                                     key
                                                                     on-first
                                                                     test
                                                                     value)))
                      (unless (eq data collected)
                        (funcall function (cl-ds:whole-range data))))))
    (unless (~> collected cl-ds:size (= 0))
      (funcall function (cl-ds:whole-range collected)))
    range))
