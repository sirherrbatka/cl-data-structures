(cl:in-package #:cl-data-structures.common)


(defclass sequence-window (cl-ds:fundamental-random-access-range)
  ((%from :initarg :from
          :reader read-from)
   (%to :initarg :to
        :reader read-to)
   (%content :initarg :content
             :reader read-content)
   (%current-index :initarg :current-index
                   :accessor access-current-index
                   :initform 0)
   (%initial-to :accessor access-initial-to)))


(defmethod initialize-instance :after ((instance sequence-window)
                                       &key &allow-other-keys)
  (bind (((:slots %from %current-index
                  %initial-to %to)
          instance))
    (setf %current-index %from
          %initial-to %to)))


(defmethod reinitialize-instance ((instance sequence-window)
                                  &key &allow-other-keys)
  (bind (((:slots %from %current-index
                  %initial-to %to)
          instance))
    (setf %current-index %from
          %to %initial-to)))


(defmethod cl-ds:reset! ((instance sequence-window))
  (reinitialize-instance instance))


(defclass random-access-sequence-window (sequence-window)
  ())


(defclass vector-sequence-window (sequence-window)
  ())


(defclass list-sequence-window (sequence-window)
  ())


(defgeneric sequence-window (sequence from to))


(defmethod sequence-window ((sequence cl-ds:fundamental-random-access-range)
                            from to)
  (unless (< from to)
    (error 'cl-ds:argument-value-out-of-bounds
           :argument 'from
           :bounds (list 0 to)
           :value from
           :format-control "FROM argument is out of bounds."))
  (unless (<= to (cl-ds:size sequence))
    (error 'cl-ds:argument-value-out-of-bounds
           :argument 'to
           :bounds (list from (cl-ds:size sequence))
           :value to
           :format-control "TO argument is out of bounds."))
  (make 'random-access-sequence-window
        :from from :to to
        :content sequence))


(defmethod sequence-window :before ((sequence cl:sequence)
                                    from to)
  (unless (< from to)
    (error 'cl-ds:argument-value-out-of-bounds
           :argument 'from
           :bounds (list 0 to)
           :value from
           :format-control "FROM argument is out of bounds."))
  (unless (<= to (length sequence))
    (error 'cl-ds:argument-value-out-of-bounds
           :argument 'to
           :bounds (list from (length sequence))
           :value to
           :format-control "TO argument is out of bounds.")))


(defmethod sequence-window ((sequence sequence-window)
                            from to)
  (make (type-of sequence)
        :from (+ (access-current-index sequence) from)
        :to (+ (read-from sequence) to)
        :content (read-content sequence)))


(defmethod sequence-window ((sequence vector)
                            from to)
  (make 'vector-sequence-window
        :from from :to to
        :content sequence))


(defmethod sequence-window ((sequence list)
                            from to)
  (make 'list-sequence-window
        :from from :to to
        :content sequence))


(defgeneric effective-at (window index)
  (:method ((window vector-sequence-window) index)
    (values (aref (read-content window) index)
            t))
  (:method ((window random-access-sequence-window) index)
    (cl-ds:at (read-content window) index))
  (:method ((window list-sequence-window) index)
    (values (elt (read-content window) index)
            t)))


(defmethod cl-ds:at ((window sequence-window) index &rest more)
  (cl-ds:assert-one-dimension more)
  (bind (((:slots %from %to %current-index) window)
         (effective-index (+ %from index)))
    (if (< (1- %current-index) effective-index %to)
        (effective-at window effective-index)
        (values nil nil))))


(defmethod cl-ds:peek-front ((container sequence-window))
  (bind (((:slots %from %to %current-index) container))
    (if (< %current-index %to)
        (effective-at container %current-index)
        (values nil nil))))


(defmethod cl-ds:consume-front ((container sequence-window))
  (bind (((:slots %from %to %current-index) container))
    (if (< %current-index %to)
        (bind (((:values result value)
                (effective-at container %current-index)))
          (when value (incf %current-index))
          (values result value))
        (values nil nil))))


(defmethod cl-ds:peek-back ((container sequence-window))
  (bind (((:slots %from %to %current-index) container))
    (if (< %current-index %to)
        (bind (((:values result value) (effective-at container (1- %to))))
          (values result value))
        (values nil nil))))


(defmethod cl-ds:consume-back ((container sequence-window))
  (bind (((:values result value) (cl-ds:peek-back container)))
    (when value (decf (slot-value container '%to)))
    (values result value)))


(defmethod cl-ds:across ((container sequence-window) function)
  (bind (((:slots %current-index %to %content) container))
    (iterate
      (for i from %current-index below %to)
      (funcall function (effective-at container i)))
    container))


(defmethod cl-ds:traverse ((container sequence-window) function)
  (cl-ds:across container function))


(defmethod cl-ds:clone ((container sequence-window))
  (make (type-of container)
        :from (read-from container)
        :to (read-to container)
        :content (read-content container)
        :current-index (access-current-index container)
        :initial-to (access-initial-to container)))


(defmethod cl-ds:size ((range sequence-window))
  (bind (((:slots %to %current-index) range))
    (- %to %current-index)))
