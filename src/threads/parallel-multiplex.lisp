(cl:in-package #:cl-data-structures.threads)


(defclass parallel-forward-multiplex-proxy (cl-ds.alg:forward-multiplex-proxy)
  ((%maximum-queue-size :initarg :maximum-queue-size
                        :reader read-maximum-queue-size)
   (%chunk-size :initarg :chunk-size
                :reader read-chunk-size)))


(defmethod cl-ds.utils:cloning-information append
    ((range parallel-forward-multiplex-proxy))
  '((:chunk-size read-chunk-size)
    (:maximum-queue-size read-maximum-queue-size)))


(defclass parallel-multiplex-function (cl-ds.alg.meta:layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric parallel-multiplex (range &key key function chunk-size
                                        maximum-queue-size)
  (:generic-function-class parallel-multiplex-function)
  (:method (range &key
                    (function #'cl-ds:whole-range)
                    (key #'identity)
                    (chunk-size 32)
                    (maximum-queue-size 512))
    (check-type maximum-queue-size integer)
    (cl-ds:check-argument-bounds maximum-queue-size
                                 (<= 16 maximum-queue-size))
    (ensure-functionf function)
    (ensure-functionf key)
    (cl-ds.alg.meta:apply-range-function
     range #'parallel-multiplex
     (list range :key key :function function
                 :chunk-size chunk-size
                 :maximum-queue-size maximum-queue-size))))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:traversable)
                                       (fn parallel-multiplex-function)
                                       all)
  (let ((keys (rest all)))
    (make 'parallel-forward-multiplex-proxy
          :original-range range
          :key (getf keys :key)
          :chunk-size (getf keys :chunk-size)
          :maximum-queue-size (getf keys :maximum-queue-size)
          :function (getf keys :function))))


(defmethod cl-ds.alg.meta:aggregator-constructor
    ((range parallel-forward-multiplex-proxy)
     outer-constructor
     function
     (arguments list))
  (declare (optimize (speed 1) (safety 2) (debug 1)
                     (compilation-speed 0) (space 0)))
  (bind ((outer-fn (or outer-constructor
                       (cl-ds.alg.meta:aggregator-constructor
                        '() nil function arguments)))
         (maximum-queue-size (read-maximum-queue-size range))
         (fn (ensure-function (cl-ds.alg:read-function range)))
         (key (ensure-function (cl-ds.alg:read-key range)))
         (chunk-size (read-chunk-size range))
         (queue (lparallel.queue:make-queue
                 :fixed-capacity maximum-queue-size))
         (result-queue (lparallel.queue:make-queue))
         ((:flet read-results (&optional block))
          (iterate
            (until (and (not block)
                        (lparallel.queue:queue-empty-p result-queue)))
            (setf block nil)
            (for (vector . inner) = (lparallel.queue:pop-queue result-queue))
            (iterate
              (for elt in-vector vector)
              (cl-ds.alg.meta:pass-to-aggregation inner elt))))
         ((:flet push-queue (new inner))
          (read-results)
          (lparallel.queue:with-locked-queue queue
            (when (lparallel.queue:queue-full-p/no-lock queue)
              (iterate
                (with future = (lparallel.queue:pop-queue/no-lock queue))
                (until (lparallel:fulfilledp future))
                (read-results t)))
            (lparallel.queue:push-queue/no-lock
             (lparallel:future
               (let ((result (vect)))
                 (unwind-protect
                      (~>> new (funcall key) (funcall fn)
                           (cl-ds:traverse
                            _
                            (lambda (element)
                              (vector-push-extend element result)
                              (unless (< (fill-pointer result) chunk-size)
                                (lparallel.queue:push-queue (cons (copy-array result)
                                                                  inner)
                                                            result-queue)
                                (setf (fill-pointer result) 0)))))
                   (lparallel.queue:push-queue (cons result inner)
                                               result-queue))))
             queue))))
    (cl-ds.alg.meta:aggregator-constructor
     (cl-ds.alg:read-original-range range)
     (cl-ds.utils:cases ((:variant (eq key #'identity))
                         (:variant (eq fn #'identity)))
       (cl-ds.alg.meta:let-aggregator
           ((inner (cl-ds.alg.meta:call-constructor outer-fn)))

           ((element)
            (push-queue element inner))

           ((iterate
              (until (lparallel.queue:queue-empty-p queue))
              (for future = (lparallel.queue:pop-queue queue))
              (iterate
                (until (lparallel:fulfilledp future))
                (read-results t)))
            (read-results)
            (cl-ds.alg.meta:extract-result inner))

         (cl-ds.alg.meta:cleanup inner)))
     function
     arguments)))
