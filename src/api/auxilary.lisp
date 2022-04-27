(cl:in-package #:cl-data-structures)


(defclass empty-range (fundamental-forward-range)
  ())


(defmethod cl-ds:consume-front ((range empty-range))
  (values nil nil))


(defmethod cl-ds:peek-front ((range empty-range))
  (values nil nil))


(defmethod cl-ds:reset! ((range empty-range))
  range)


(defmethod cl-ds:clone ((range empty-range))
  (make (class-of range)))


(defmethod cl-ds:traverse ((range empty-range)
                           function)
  (ensure-functionf function)
  range)


(defmethod cl-ds:across ((range empty-range)
                         function)
  (ensure-functionf function)
  range)
