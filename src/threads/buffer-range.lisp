(cl:in-package #:cl-data-structures.threads)


(defclass buffer-range (cl-ds.alg:proxy-range)
  ((%maximum-queue-size :initarg :maximum-queue-size
                        :reader read-maximum-queue-size)
   (%chunk-size :initarg :chunk-size
                :reader read-chunk-size)))


(defmethod cl-ds.utils:cloning-information append
    ((range buffer-range))
  '((:maximum-queue-size read-maximum-queue-size)
    (:chunk-size read-chunk-size)))


(defclass forward-buffer-range (buffer-range cl-ds.alg:forward-proxy-range)
  ())


(defclass bidirectional-buffer-range (buffer-range cl-ds.alg:bidirectional-proxy-range)
  ())


(defclass random-access-buffer-range (buffer-range cl-ds.alg:random-access-proxy-range)
  ())


(defclass thread-buffer-function (cl-ds.alg.meta:layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric thread-buffer (range &key maximum-queue-size chunk-size)
  (:generic-function-class thread-buffer-function)
  (:method (range &key (maximum-queue-size 512) (chunk-size 128))
    (check-type maximum-queue-size integer chunk-size)
    (cl-ds:check-argument-bounds maximum-queue-size
                                 (<= 16 maximum-queue-size))
    (cl-ds:check-argument-bounds chunk-size
                                 (<= 1 chunk-size))
    (cl-ds.alg.meta:apply-range-function
     range #'thread-buffer
     (list range
           :maximum-queue-size maximum-queue-size
           :chunk-size chunk-size))))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-forward-range)
                                       (fn thread-buffer-function)
                                       arguments)
  (cl-ds.alg:make-proxy range 'forward-buffer-range
                        :chunk-size (getf (rest arguments) :chunk-size)
                        :maximum-queue-size (getf (rest arguments) :maximum-queue-size)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-random-access-range)
                                       (fn thread-buffer-function)
                                       arguments)
  (cl-ds.alg:make-proxy range 'random-access-buffer-range
                        :chunk-size (getf (rest arguments) :chunk-size)
                        :maximum-queue-size (getf (rest arguments) :maximum-queue-size)))


(defmethod cl-ds.alg.meta:apply-layer ((range cl-ds:fundamental-bidirectional-range)
                                       (fn thread-buffer-function)
                                       arguments)
  (cl-ds.alg:make-proxy range 'bidirectional-buffer-range
                        :chunk-size (getf (rest arguments) :chunk-size)
                        :maximum-queue-size (getf (rest arguments) :maximum-queue-size)))


(defmethod cl-ds.alg.meta:aggregator-constructor
    ((range buffer-range)
     outer-constructor
     (function cl-ds.alg.meta:aggregation-function)
     (arguments list))
  (declare (optimize (speed 3) (safety 0)
                     (compilation-speed 0) (space 0)))
  (bind ((outer-fn (call-next-method))
         (chunk-size (the fixnum (read-chunk-size range)))
         (maximum-queue-size (read-maximum-queue-size range)))
    (cl-ds.alg.meta:aggregator-constructor
     (cl-ds.alg:read-original-range range)
     (cl-ds.alg.meta:let-aggregator
         ((inner (cl-ds.alg.meta:call-constructor outer-fn))
          (queue (lparallel.queue:make-queue
                  :fixed-capacity maximum-queue-size))
          (error-lock (bt:make-lock "error lock"))
          (stored-error nil)
          (chunk (make-array chunk-size :fill-pointer 0))
          ((:flet push-chunk ())
           (lparallel.queue:push-queue (copy-array chunk) queue)
           (setf (fill-pointer chunk) 0))
          ((:flet thread-function ())
           (iterate
             (for elt = (lparallel.queue:pop-queue queue))
             (until (null elt))
             (handler-case (iterate
                             (for e in-vector elt)
                             (cl-ds.alg.meta:pass-to-aggregation inner e))
               (error (e) (bt:with-lock-held (error-lock)
                            (setf stored-error e)
                            (leave))))))
          (aggregate-thread (bt:make-thread #'thread-function
                                            :name "Aggregation Thread")))

         ((element)
           (when (= chunk-size (fill-pointer chunk))
             (push-chunk))
           (vector-push-extend element chunk))

         ((bt:with-lock-held (error-lock)
            (unless (null stored-error)
              (error stored-error)))
          (unless (zerop (fill-pointer chunk))
            (push-chunk))
           (lparallel.queue:push-queue nil queue)
           (bt:join-thread aggregate-thread)
           (bt:with-lock-held (error-lock)
             (unless (null stored-error)
               (error stored-error)))
           (cl-ds.alg.meta:extract-result inner))

       (cl-ds.alg.meta:cleanup inner))
     function
     arguments)))
