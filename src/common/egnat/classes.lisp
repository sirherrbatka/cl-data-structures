(cl:in-package #:cl-data-structures.common.egnat)


(defclass fundamental-egnat-container (cl-ds:fundamental-container)
  ((%branching-factor
    :type integer
    :initarg :branching-factor
    :reader read-branching-factor)
   (%samples-count
    :type integer
    :initarg :samples-count
    :reader read-samples-count)
   (%metric-type
    :reader read-metric-type
    :initform :single-float
    :initarg :metric-type)
   (%content-count-in-node
    :type integer
    :reader read-content-count-in-node
    :initarg :content-count-in-node)
   (%size
    :accessor access-size
    :reader cl-ds:size
    :type integer
    :initform 0)
   (%root
    :accessor access-root
    :initform nil
    :initarg :root)))


(defclass mutable-egnat-container (fundamental-egnat-container
                                   cl-ds:mutable)
  ())


(defclass egnat-node ()
  ((%content
    :initarg :content
    :reader read-content)))


(defmethod initialize-instance :after ((instance egnat-node)
                                       &rest all)
  (declare (ignore all))
  (assert (vectorp (read-content instance))))


(defmethod cl-ds.utils:cloning-information append ((object egnat-node))
  '((:content read-content)))


(defclass egnat-subtree ()
  ((%content
    :initarg :content
    :reader read-content)
   (%close-range
    :initform nil
    :initarg :close-range
    :accessor access-close-range
    :reader read-close-range)
   (%distant-range
    :initarg :distant-range
    :initform nil
    :accessor access-distant-range
    :reader read-distant-range)
   (%children
    :type vector
    :initform #()
    :initarg :children
    :reader read-children)))


(defmethod cl-ds.utils:cloning-information append ((object egnat-subtree))
  '((:close-range read-close-range)
    (:content read-content)
    (:distant-range read-distant-range)
    (:children read-children)))


(defclass egnat-range (cl-ds:fundamental-forward-range)
  ((%stack :initform '()
           :initarg :stack
           :accessor access-stack)
   (%initial-stack :reader read-initial-stack
                   :initarg :stack)
   (%initial-results :initform '()
                     :initarg :results
                     :reader read-initial-stack)
   (%container :initarg :container
               :reader read-container)))


(defclass egnat-range-around (egnat-range)
  ((%near :initarg :near
          :reader read-near)
   (%margin :initarg :margin
            :reader read-margin)))


(defmethod initialize-instance
    :after ((container fundamental-egnat-container) &rest all)
  (declare (ignore all))
  (bind (((:slots %samples-count %branching-factor %content-count-in-node) container))
    (check-type %branching-factor integer)
    (check-type %samples-count fixnum)
    (check-type %content-count-in-node integer)
    (when (zerop %content-count-in-node)
      (error 'cl-ds:initialization-out-of-bounds
             :format-control "Content count in node should be at least 1"
             :value %content-count-in-node
             :class (type-of container)
             :argument :content-count-in-node
             :bounds '(>= 1)))
    (when (< %samples-count 1)
      (error 'cl-ds:initialization-out-of-bounds
             :format-control "Samples count must be positive"
             :value %samples-count
             :class (type-of container)
             :argument :samples-count
             :bounds '(>= 1)))
    (when (< %branching-factor 2)
      (error 'cl-ds:initialization-out-of-bounds
             :format-control "Branching factor should be at least 2"
             :value %branching-factor
             :class (type-of container)
             :argument :branching-factor
             :bounds '(>= 2)))))
