(cl:in-package #:cl-data-structures.threads)


(defclass parallel-on-each-proxy (cl-ds.alg:forward-proxy-box-range)
  ((%maximum-queue-size :initarg :maximum-queue-size
                        :reader read-maximum-queue-size)
   (%chunk-size :initarg :chunk-size
                :reader read-chunk-size)))


(defmethod cl-ds.utils:cloning-information append
    ((range parallel-on-each-proxy))
  '((:maximum-queue-size read-maximum-queue-size)
    (:chunk-size read-chunk-size)))


(defclass parallel-on-each-function (cl-ds.alg.meta:layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric parallel-on-each (range function
                              &key key maximum-queue-size chunk-size
                                functor-constructor)
  (:generic-function-class parallel-on-each-function)
  (:method (range function
            &key (key #'identity) (maximum-queue-size 512) (chunk-size 128)
              (functor-constructor #'identity))
    (check-type maximum-queue-size integer)
    (check-type chunk-size integer)
    (cl-ds:check-argument-bounds maximum-queue-size
                                 (<= 16 maximum-queue-size))
    (cl-ds:check-argument-bounds chunk-size (<= 1 chunk-size))
    (ensure-functionf function)
    (ensure-functionf key)
    (ensure-functionf functor-constructor)
    (cl-ds.alg.meta:apply-range-function
     range #'parallel-on-each
     (list range function
           :key key
           :functor-constructor functor-constructor
           :maximum-queue-size maximum-queue-size
           :chunk-size chunk-size))))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:traversable)
                                       (fn parallel-on-each-function)
                                       all)
  (let ((keys (cddr all)))
    (make-instance 'parallel-on-each-proxy
                   :original-range range
                   :chunk-size (getf keys :chunk-size)
                   :key (getf keys :key)
                   :maximum-queue-size (getf keys :maximum-queue-size)
                   :functor-constructor (getf keys :functor-constructor)
                   :functor-prototype (second all))))


(defmethod cl-ds.alg.meta:aggregator-constructor
    ((range parallel-on-each-proxy)
     outer-constructor
     (function cl-ds.alg.meta:aggregation-function)
     (arguments list))
  (declare (optimize (speed 1) (safety 2) (debug 2)
                     (compilation-speed 0) (space 0)))
  (bind ((outer-fn (or outer-constructor
                       (cl-ds.alg.meta:aggregator-constructor
                        '() nil function arguments)))
         (maximum-queue-size (read-maximum-queue-size range))
         (chunk-size (the fixnum (read-chunk-size range)))
         (key (ensure-function (cl-ds.alg:read-key range)))
         (queue (lparallel.queue:make-queue
                 :fixed-capacity maximum-queue-size))
         (functor (cl-ds.alg:access-functor range))
         (functor-constructor (ensure-function (cl-ds.alg:read-functor-constructor range)))
         (functor-prototype (cl-ds.alg:read-functor-prototype range))
         ((:flet handle-result (elt inner))
          (setf elt (lparallel:force elt))
          (iterate
            (with length = (length elt))
            (for i from 0 below length)
            (for e = (aref elt i))
            (cl-ds.alg.meta:pass-to-aggregation inner e)))
         ((:flet push-chunk (fn chunk inner))
          (declare (type (cl-ds.utils:extendable-vector t) chunk))
          (let ((chunk (copy-array chunk)))
            (lparallel.queue:with-locked-queue queue
              (when (lparallel.queue:queue-full-p/no-lock queue)
                (bind (((elt . inner) (lparallel.queue:pop-queue/no-lock queue)))
                  (handle-result elt inner)))
              (lparallel.queue:push-queue/no-lock
               (cons (lparallel:future
                       (assert (array-has-fill-pointer-p chunk))
                       (cl-ds.utils:transform fn chunk))
                     inner)
               queue)))
          (setf (fill-pointer chunk) 0)))
    (cl-ds.alg.meta:aggregator-constructor
     (cl-ds.alg:read-original-range range)
     (cl-ds.utils:cases ((:variant (eq key #'identity)))
       (cl-ds.alg.meta:let-aggregator
           ((inner (cl-ds.alg.meta:call-constructor outer-fn))
            (chunk (make-array chunk-size :fill-pointer 0))
            (range-function (ensure-function
                             (or functor (funcall functor-constructor
                                                  functor-prototype)))))

           ((element)
             (unless (< (fill-pointer chunk) chunk-size)
               (push-chunk range-function chunk inner))
             (vector-push-extend element chunk))

           ((unless (zerop (fill-pointer chunk))
              (push-chunk range-function chunk inner))
             (lparallel.queue:with-locked-queue queue
               (iterate
                 (until (lparallel.queue:queue-empty-p/no-lock queue))
                 (for (elt . inner) = (lparallel.queue:pop-queue/no-lock queue))
                 (handle-result elt inner)))
             (cl-ds.alg.meta:extract-result inner))

         (cl-ds.alg.meta:cleanup inner)))
     function
     arguments)))
