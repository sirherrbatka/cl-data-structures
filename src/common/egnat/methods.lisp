(cl:in-package #:cl-data-structures.common.egnat)


(declaim (inline position-to-value))
(defun position-to-value (position)
  (bind (((node results) position))
    (declare (ignore node))
    (if (consp results)
        (values t (first results))
        (values nil nil))))


(defun add-children-to-stack (range node stack)
  (when (typep node 'egnat-node)
    (return-from add-children-to-stack stack))
  (bind ((children (read-children node))
         (children-mask (select-children range node)))
    (iterate
      (for child in-vector children)
      (for present-bit in-vector children-mask)
      (for present = (eql 1 present-bit))
      (when present
        (push (list child t) stack))
      (finally (return stack)))))


(defmacro front-body (range stack)
  `(iterate
     (until (endp ,stack))
     (for (values found next) = (position-to-value (first ,stack)))
     (setf stack (next-elements ,range ,stack))
     (when found
       (leave (values next t)))
     (finally (return (values nil nil)))))


(defmacro traverse-body (range stack function)
  `(iterate
     (until (endp ,stack))
     (for (values found next) = (position-to-value (first ,stack)))
     (setf stack (next-elements ,range ,stack))
     (when found
       (funcall ,function next))
     (finally (return ,range))))


(cl-ds.common:defmethod-with-stack
    (cl-ds:consume-front
     ((range egnat-range))
     stack
     (access-stack range))
  (front-body range stack))


(cl-ds.common:defmethod-with-peek-stack
    (cl-ds:peek-front
     ((range egnat-range))
     stack (access-stack range))
  (front-body range stack))


(cl-ds.common:defmethod-with-peek-stack
    (cl-ds:across
     ((range egnat-range) function)
     stack (access-stack range))
  (traverse-body range stack function))


(cl-ds.common:defmethod-with-stack
    (cl-ds:traverse
     ((range egnat-range) function)
     stack (access-stack range))
  (traverse-body range stack function))


(defmethod cl-ds:reset! ((range egnat-range))
  (bind (((:slots %stack %initial-stack) range))
    (setf %stack %initial-stack))
  range)


(defmethod cl-ds:clone ((range egnat-range))
  (make-instance (type-of range)
                 :stack (access-stack range)
                 :container (read-container range)))


(defmethod cl-ds:clone ((range egnat-range-around))
  (make-instance (type-of range)
                 :stack (access-stack range)
                 :container (read-container range)
                 :near (read-near range)
                 :margin (read-margin range)))


(defmethod cl-ds:clone ((container fundamental-egnat-container))
  (bind (((:slots %branching-factor %metric-fn %metric-type
                  %content-count-in-node %size %root)
          container))
    (make (type-of container)
          :branching-factor %branching-factor
          :metric-fn %metric-fn
          :metric-type %metric-type
          :content-count-in-node %content-count-in-node
          :size %size
          :root %root)))


(defmethod cl-ds:near ((container fundamental-egnat-container)
                       item
                       maximal-distance)
  (let* ((root (access-root container))
         (result (make 'egnat-range-around
                       :near item
                       :stack (list (list root t))
                       :margin maximal-distance
                       :container container)))
    result))


(defmethod cl-ds:whole-range ((container fundamental-egnat-container))
  (let* ((root (access-root container))
         (stack (list (list root t)))
         (result (make 'egnat-range
                       :stack stack
                       :container container)))
    result))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:grow-function)
                                             container
                                             (structure mutable-egnat-container)
                                             location
                                             &rest all)
  (egnat-grow! structure container operation location all))


(defmethod cl-ds:traverse ((container fundamental-egnat-container) function)
  (traverse-impl container function))


(defmethod cl-ds:across ((container fundamental-egnat-container) function)
  (traverse-impl container function))
