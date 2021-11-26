(cl:in-package #:cl-data-structures.adapters)


(defmethod cl-ds:at ((container sequence) index &rest more-locations)
  (cl-ds:assert-one-dimension more-locations)
  (elt container index))


(defmethod cl-ds:size ((seq list))
  (length seq))


(defclass list-range (cl-ds:fundamental-forward-range)
  ((%content :initarg :content
             :accessor access-content)
   (%original-content :initarg :original-content
                      :reader read-original-content)))


(defmethod cl-ds:clone ((obj list-range))
  (make 'list-range
        :content (access-content obj)
        :original-content (access-content obj)))


(defmethod cl-ds:reset! ((obj list-range))
  (setf (access-content obj) (read-original-content obj))
  obj)


(defmethod cl-ds:peek-front ((obj list-range))
  (if (endp (access-content obj))
      (values nil nil)
      (values (first (access-content obj))
              t)))


(defmethod cl-ds:consume-front ((obj list-range))
  (if (endp (access-content obj))
      (values nil nil)
      (values (pop (access-content obj))
              t)))


(defmethod cl-ds:traverse ((obj list-range) function)
  (ensure-functionf function)
  (map nil function (access-content obj))
  (setf (access-content obj) nil)
  obj)


(defmethod cl-ds:across ((obj list-range) function)
  (ensure-functionf function)
  (map nil function (access-content obj))
  obj)


(defmethod cl-ds:whole-range ((container cl:list))
  (make 'list-range
        :content container
        :original-content container))
