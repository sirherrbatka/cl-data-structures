(cl:in-package #:cl-data-structures)


(defgeneric lower-bound (container location))

(defgeneric at (container location &rest more-locations))

(defgeneric (setf at) (new-value container location &rest more-locations)
  (:generic-function-class cl-ds.meta:insert!-function))

(defgeneric insert! (container location value)
  (:generic-function-class cl-ds.meta:insert!-function))

(defgeneric add (container location new-value)
  (:generic-function-class cl-ds.meta:functional-add-function))

(defgeneric put (container item)
  (:generic-function-class cl-ds.meta:functional-put-function))

(defgeneric put-back (container item)
  (:generic-function-class cl-ds.meta:functional-put-back-function))

(defgeneric put-front (container item)
  (:generic-function-class cl-ds.meta:functional-put-front-function))

(defgeneric put-back! (container item)
  (:generic-function-class cl-ds.meta:put-back!-function))

(defgeneric put-front! (container item)
  (:generic-function-class cl-ds.meta:put-front!-function))

(defgeneric take-out! (container)
  (:generic-function-class cl-ds.meta:take-out!-function))

(defgeneric take-out (container)
  (:generic-function-class cl-ds.meta:functional-take-out-function))

(defgeneric take-out-back (container)
  (:generic-function-class cl-ds.meta:functional-take-out-back-function))

(defgeneric take-out-front (container)
  (:generic-function-class cl-ds.meta:functional-take-out-front-function))

(defgeneric take-out-back! (container)
  (:generic-function-class cl-ds.meta:take-out-back!-function))

(defgeneric take-out-front! (container)
  (:generic-function-class cl-ds.meta:take-out-front!-function))

(defgeneric near (container item maximal-distance))

(defgeneric between* (container &key low high))

(defgeneric add! (container location new-value)
  (:generic-function-class cl-ds.meta:add!-function))

(defgeneric insert (container location new-value)
  (:generic-function-class cl-ds.meta:functional-insert-function))

(defgeneric erase*! (container range)
  (:generic-function-class cl-ds.meta:erase*!-function))

(defgeneric erase* (container range)
  (:generic-function-class cl-ds.meta:erase*-function))

(defgeneric erase (container location)
  (:generic-function-class cl-ds.meta:functional-erase-function))

(defgeneric erase-if (container location condition-fn)
  (:generic-function-class cl-ds.meta:functional-erase-if-function))

(defgeneric erase-if! (container location condition-fn)
  (:generic-function-class cl-ds.meta:erase-if!-function))

(defgeneric erase! (container location)
  (:generic-function-class cl-ds.meta:erase!-function))

(defgeneric put! (container item)
  (:generic-function-class cl-ds.meta:put!-function))

(defgeneric size (container))

(defgeneric update (container location new-value)
  (:generic-function-class cl-ds.meta:functional-update-function))

(defgeneric update-if (container location new-value condition-fn)
  (:generic-function-class cl-ds.meta:functional-update-if-function))

(defgeneric update! (container location new-value)
  (:generic-function-class cl-ds.meta:update!-function))

(defgeneric update-if! (container location new-value condition-fn)
  (:generic-function-class cl-ds.meta:update-if!-function))

(defgeneric become-functional (container)
  (:method :around ((container functional))
    (if (typep container 'lazy)
        (call-next-method)
        container)))

(defgeneric become-mutable (container)
  (:method :around ((container mutable))
    (if (transactionalp container)
        (call-next-method)
        container)))

(defgeneric replica (container &optional isolate))

(defgeneric become-transactional (container))

(defgeneric become-lazy (container))

(defgeneric mutablep (container)
  (:method ((container mutable)) t)
  (:method ((container fundamental-container)) nil))

(defgeneric functionalp (container)
  (:method ((container functional)) t)
  (:method ((container fundamental-container)) nil))

(defgeneric transactionalp (container)
  (:method ((container transactional)) t)
  (:method ((container fundamental-container)) nil))

(defgeneric value (status))

(defgeneric changed (status))

(defgeneric found (status))

(defgeneric empty-clone (container))

(defgeneric traverse (object function)
  (:method ((object sequence) function)
    (map nil function object))
  (:method ((object fundamental-range) function)
    (iterate
      (for (values val more) = (cl-ds:consume-front object))
      (while more)
      (funcall function val))))

(defgeneric across (object function)
  (:method ((object sequence) function)
    (map nil function object)
    object)
  (:method ((object fundamental-container) function)
    (traverse object function)
    object)
  (:method ((object fundamental-range) function)
    (traverse (clone object) function)
    object))

(defgeneric make-from-traversable (traversable class &rest arguments))

(defgeneric make-of-size (class size &rest more))

(defgeneric forward-call (object function))

(defgeneric type-specialization (container)
  (:method ((container fundamental-container))
    t))

#|

Range releated functions.

|#

(defgeneric consume-front (range))

(defgeneric peek-front (range))

(defgeneric (setf peek-front) (new-value range))

(defgeneric consume-back (range))

(defgeneric peek-back (range))

(defgeneric (setf peek-back) (new-value range))

(defgeneric chunked (range &optional chunk-size-hint)
  (:method :before ((range fundamental-range) &optional chunk-size-hint)
    (check-type chunk-size-hint (or null positive-integer)))
  (:method ((range fundamental-range) &optional chunk-size-hint)
    (declare (ignore chunk-size-hint))
    nil))

(defgeneric dimensionality (object)
  (:method ((object fundamental-container))
    1)
  (:method ((object fundamental-range))
    1))

(defgeneric drop-front (range count)
  (:method ((range fundamental-forward-range) count)
    (check-type count non-negative-fixnum)
    (iterate
      (repeat count)
      (for i from 0)
      (for (values value more) = (consume-front range))
      (while more)
      (finally (return (values range i))))))

(defgeneric drop-back (range count)
  (:method ((range fundamental-bidirectional-range) count)
    (check-type count non-negative-fixnum)
    (iterate
      (repeat count)
      (for i from 0)
      (for (values value more) = (consume-back range))
      (while more)
      (finally (return (values range i))))))

(defgeneric clone (range))

(defgeneric whole-range (container)
  (:method ((range fundamental-range))
    range))

(defgeneric reset! (obj))


(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:functional-function))
  operation)


(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:erase!-function))
  #'erase)


(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:erase*!-function))
  #'erase*)


(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:update-if!-function))
  #'update-if)


(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:erase-if!-function))
  #'erase-if)


(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:put!-function))
  #'put)


(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:add!-function))
  #'add)


(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:insert!-function))
  #'insert)


(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:take-out!-function))
  #'take-out)


(defmethod cl-ds.meta:functional-counterpart ((operation cl-ds.meta:update!-function))
  #'update)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:take-out-back!-function))
  #'take-out-back)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:erase*-function))
  #'erase*!)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:take-out-front!-function))
  #'take-out-front)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:put-front!-function))
  #'put-front)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:put-back!-function))
  #'put-back)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:destructive-function))
  operation)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-erase-function))
  #'erase!)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-erase-if-function))
  #'erase-if!)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-update-if-function))
  #'update-if!)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-add-function))
  #'add!)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-put-function))
  #'put!)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-insert-function))
  #'insert!)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-take-out-function))
  #'take-out!)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-take-out-back-function))
  #'take-out-back!)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-take-out-front-function))
  #'take-out-front!)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-put-front-function))
  #'put-front!)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-put-back-function))
  #'put-back!)


(defmethod cl-ds.meta:destructive-counterpart ((operation cl-ds.meta:functional-update-function))
  #'update!)


(defmethod clone ((range chunked-range))
  (make 'chunked-range
        :original-range (~> range read-original-range clone)
        :chunk-size (read-chunk-size range)))


(defmethod chunked ((range chunking-mixin) &optional chunk-size-hint)
  (make 'chunked-range
        :original-range (cl-ds:clone range)
        :chunk-size (or chunk-size-hint 512)))


(defmethod consume-front ((range chunked-range))
  (bind ((og-range (read-original-range range))
         ((:values item more) (consume-front og-range)))
    (if more
        (let* ((chunk-size (read-chunk-size range))
               (result (make-array chunk-size
                                   :adjustable t
                                   :fill-pointer 1)))
          (setf (aref result 0) item)
          (iterate
            (for i from 1 below chunk-size)
            (for (values elt m) = (consume-front og-range))
            (while m)
            (vector-push-extend elt result))
          (values (whole-range result)
                  t))
        (values nil nil))))


(defmethod cl-ds:across ((range chunked-range) function)
  (bind ((og-range (read-original-range range))
         (chunk-size (read-chunk-size range))
         (vector #1=(make-array chunk-size :fill-pointer 0)))
    (cl-ds:across og-range
                  (lambda (x)
                    (vector-push-extend x vector)
                    (unless (< (length vector) chunk-size)
                      (funcall function (cl-ds:whole-range vector))
                      (setf vector #1#))))
    (unless (emptyp vector)
      (funcall function (cl-ds:whole-range vector)))
    range))


(defmethod cl-ds:traverse ((range chunked-range) function)
  (bind ((og-range (read-original-range range))
         (chunk-size (read-chunk-size range))
         (vector #1=(make-array chunk-size :fill-pointer 0)))
    (cl-ds:traverse og-range
                    (lambda (x)
                      (vector-push-extend x vector)
                      (unless (< (length vector) chunk-size)
                        (funcall function (cl-ds:whole-range vector))
                        (setf vector #1#))))
    (unless (emptyp vector)
      (funcall function (cl-ds:whole-range vector)))
    range))


(defmethod peek-front ((range chunked-range))
  (bind ((og-range (~> range read-original-range cl-ds:clone))
         ((:values item more) (consume-front og-range)))
    (if more
        (let* ((chunk-size (read-chunk-size range))
               (result (make-array chunk-size
                                   :adjustable t
                                   :fill-pointer 1)))
          (setf (aref result 0) item)
          (iterate
            (for i from 1 below chunk-size)
            (for (values elt m) = (consume-front og-range))
            (while m)
            (vector-push-extend elt result))
          (values (whole-range result)
                  t))
        (values nil nil))))


(defmethod forward-call ((object fundamental-forward-range)
                         function)
  (funcall function object))
