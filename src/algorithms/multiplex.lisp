(cl:in-package #:cl-data-structures.algorithms)


(defclass multiplex-proxy (cl-ds:chunking-mixin
                           proxy-range)
  ((%key :initarg :key
         :reader read-key)
   (%function :initarg :function
              :reader read-function)
   (%current :initform nil
             :initarg :current
             :accessor access-current)
   (%initial-current :initform nil
                     :initarg :initial-current
                     :reader read-initial-current)))


(defmethod cl-ds.utils:cloning-information append
    ((range multiplex-proxy))
  '((:key read-key)
    (:current access-current)
    (:function read-function)))


(defmethod cl-ds:clone ((range multiplex-proxy))
  (bind (((:slots %current) range))
    (apply #'make (class-of range)
           :current #1=(if (null %current)
                           nil
                           (cl-ds:clone %current))
           :initial-current #1#
           :original-range (~> range
                               read-original-range
                               cl-ds:clone)
           (iterate
             (for (initarg reader) in (cl-ds.utils:cloning-information range))
             (collect initarg)
             (collect (funcall reader range))))))


(defmethod cl-ds:traverse ((range multiplex-proxy) function)
  (ensure-functionf function)
  (bind ((key (read-key range))
         (range-function (read-function range))
         ((:slots %current) range))
    (cl-ds:traverse (read-original-range range)
                    (lambda (x &aux (elt (funcall key x)))
                      (cl-ds:traverse %current function)
                      (setf %current (funcall range-function elt))
                      (cl-ds:traverse %current function)
                      (setf %current nil)))
    range)
  range)


(defmethod cl-ds:across ((range multiplex-proxy) function)
  (ensure-functionf function)
  (bind ((key (read-key range))
         (range-function (read-function range))
         ((:slots %current) range)
         (current %current))
    (cl-ds:across (read-original-range range)
                    (lambda (x &aux (elt (funcall key x)))
                      (cl-ds:traverse current function)
                      (setf current (funcall range-function elt))
                      (cl-ds:traverse current function)
                      (setf current nil)))
    range)
  range)


(defmethod cl-ds.alg.meta:across-aggregate ((range multiplex-proxy) function)
  (cl-ds:across range function))


(defmethod cl-ds:reset! ((range multiplex-proxy))
  (setf (access-current range) (let ((current (read-initial-current range)))
                                 (if (null current)
                                     nil
                                     (cl-ds:clone current))))
  (call-next-method)
  range)


(defmethod cl-ds:consume-front ((range multiplex-proxy))
  (bind (((:slots %current) range)
         (function (read-function range))
         (key (read-key range))
         (inner-range (read-original-range range)))
    (iterate
      (for (values element more) = (if (null %current)
                                       (values nil nil)
                                       (cl-ds:consume-front %current)))
      (when more
        (return-from cl-ds:consume-front (values element more)))
      (setf %current nil)
      (for (values next even-more) = (cl-ds:consume-front inner-range))
      (unless even-more
        (return-from cl-ds:consume-front (values nil nil)))
      (setf %current (~>> next (funcall key) (funcall function))))
    range))


(defmethod cl-ds:peek-front ((range multiplex-proxy))
  (~> range cl-ds:clone cl-ds:consume-front))


(defclass multiplex-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric multiplex (range &key key function)
  (:generic-function-class multiplex-function)
  (:method (range &key (key #'identity) (function #'cl-ds:whole-range))
    (ensure-functionf key)
    (apply-range-function range #'multiplex
                          (list range :key key :function function))))


(defclass forward-multiplex-proxy (multiplex-proxy
                                   cl-ds:fundamental-forward-range)
  ())


(defmethod apply-layer ((range traversable)
                        (fn multiplex-function)
                        all)
  (make 'forward-multiplex-proxy
        :original-range range
        :key (getf (rest all) :key)
        :function (getf (rest all) :function)))


(defmethod cl-ds.alg.meta:aggregator-constructor ((range multiplex-proxy)
                                                  outer-constructor
                                                  (function aggregation-function)
                                                  (arguments list))
  (declare (optimize (speed 3) (safety 0) (compilation-speed 0) (space 0)))
  (bind ((outer-fn (call-next-method))
         (fn (ensure-function (read-function range)))
         (key (ensure-function (read-key range))))
    (cl-ds.alg.meta:aggregator-constructor
     (read-original-range range)
     (cl-ds.utils:cases ((:variant (eq key #'identity)))
       (cl-ds.alg.meta:let-aggregator
           ((inner (cl-ds.alg.meta:call-constructor outer-fn)))

           ((element)
            (~>> element (funcall key) (funcall fn)
                 (cl-ds:traverse
                  _
                  (lambda (x)
                    (cl-ds.alg.meta:pass-to-aggregation inner x)))))

           ((cl-ds.alg.meta:extract-result inner))

         (cl-ds.alg.meta:cleanup inner)))
     function
     arguments)))
