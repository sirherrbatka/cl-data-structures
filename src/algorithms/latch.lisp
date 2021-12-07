(cl:in-package #:cl-ds.alg)


(defclass latch-proxy (cl-ds:chunking-mixin
                       proxy-range)
  ((%latches :initarg :latches
             :accessor read-latches)))


(defclass forward-latch-proxy (latch-proxy
                               fundamental-forward-range)
  ())


(defmethod cl-ds:clone ((range latch-proxy))
  (make (type-of range)
        :original-range (cl-ds:clone (read-original-range range))
        :latches (map 'vector #'cl-ds:clone (read-latches range))))


(defmethod cl-ds:traverse ((range latch-proxy) function)
  (let ((current (read-original-range range))
        (latches (read-latches range)))
    (iterate
      (for (values value more) = (cl-ds:consume-front current))
      (while more)
      (iterate
        (for latch in-vector latches)
        (for (values open more) = (cl-ds:consume-front latch))
        (unless more
          (return-from cl-ds:traverse range))
        (unless open
          (leave nil))
        (finally (funcall function value)))
      (finally (return range)))))


(defmethod cl-ds.alg.meta:across-aggregate ((range latch-proxy) function)
  (cl-ds:across range function))


(defmethod cl-ds:across ((range latch-proxy) function)
  (let ((current (read-original-range range))
        (latches (map 'vector #'cl-ds:clone (read-latches range))))
    (cl-ds:across current
                  (lambda (elt)
                    (iterate
                      (for latch in-vector latches)
                      (for (values open more) =
                           (cl-ds:consume-front latch))
                      (unless more
                        (return-from cl-ds:across range))
                      (unless open
                        (leave nil))
                      (finally (funcall function elt)))))
    range))


(defmethod cl-ds:reset! ((range latch-proxy))
  (map nil #'cl-ds:reset! (read-latches range))
  (call-next-method)
  range)


(defun peek-or-consume (current latches)
  (iterate
    (for (values value more) = (cl-ds:consume-front current))
    (while more)
    (iterate
      (for latch in-vector latches)
      (for (values open more) = (cl-ds:consume-front latch))
      (unless more
        (return-from peek-or-consume (values nil nil)))
      (unless open
        (leave))
      (finally (return-from peek-or-consume (values value t))))
    (finally (return (values nil nil)))))


(defmethod cl-ds:peek-front ((range forward-latch-proxy))
  (let ((current (cl-ds:clone (read-original-range range)))
        (latches (map 'vector #'cl-ds:clone (read-latches range))))
    (peek-or-consume current latches)))


(defmethod cl-ds:consume-front ((range forward-latch-proxy))
  (let ((current (read-original-range range))
        (latches (read-latches range)))
    (peek-or-consume current latches)))


(defclass latch-function (layer-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric latch (range latch &rest more-latches)
  (:generic-function-class latch-function)
  (:method (range latch &rest more-latches)
    (apply-range-function range #'latch
                          (list range latch more-latches))))


(defmethod apply-layer ((range fundamental-forward-range)
                        (function latch-function)
                        all
                        &aux
                          (latch (second all))
                          (more-latches (third all)))
  (make 'forward-latch-proxy
        :original-range range
        :latches (mapcar (lambda (object)
                           (if (typep object 'cl-ds:fundamental-forward-range)
                               (cl-ds:clone object)
                               (cl-ds:whole-range object)))
                         (cons latch more-latches))))
