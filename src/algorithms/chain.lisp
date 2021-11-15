(cl:in-package #:cl-data-structures.algorithms)


(defclass forward-chain-of-ranges (cl-ds:chunking-mixin
                                   cl-ds:fundamental-forward-range)
  ((%content :initarg :content
             :reader read-content)
   (%original-content :initarg :original-content
                      :type list
                      :reader read-original-content)))


(defclass bidirectional-chain-of-ranges (forward-chain-of-ranges
                                         fundamental-bidirectional-range)
  ())


(defclass random-access-chain-of-ranges (bidirectional-chain-of-ranges
                                         fundamental-random-access-range)
  ())


(defmethod cl-ds:forward-call ((range forward-chain-of-ranges)
                               function)
  (map nil function (read-content range))
  (map nil function (read-original-content range)))


(defmethod cl-ds:clone ((range forward-chain-of-ranges))
  (make (type-of range)
        :content (iterate
                   (with result = (make 'flexichain:standard-flexichain))
                   (with content = (read-content range))
                   (for i from 0 below (flexichain:nb-elements content))
                   (flexichain:push-end result (clone (flexichain:element* content i)))
                   (finally (return result)))
        :original-content (iterate
                            (with content = (read-content range))
                            (for i from 0 below (flexichain:nb-elements content))
                            (collecting (clone (flexichain:element* content i))))))


(defun init-chain-of-range (obj)
  (bind (((:slots %content %original-content) obj))
    (setf %content (make 'flexichain:standard-flexichain))
    (map nil
         (lambda (x) (~>> x cl-ds:clone (flexichain:push-end %content)))
         %original-content)))


(defmethod initialize-instance :after ((obj forward-chain-of-ranges)
                                       &key &allow-other-keys)
  (init-chain-of-range obj))


(defun chain (&rest ranges)
  (map nil
       (lambda (x) (check-type x cl-ds:fundamental-forward-range))
       ranges)
  (let ((fundamental-type (common-fundamental-range-class ranges)))
    (assert fundamental-type)
    (make (eswitch (fundamental-type)
            ('fundamental-forward-range 'forward-chain-of-ranges)
            ('fundamental-bidirectional-range 'bidirectional-chain-of-ranges)
            ('fundamental-random-access-range 'random-access-chain-of-ranges))
          :original-content ranges)))


(defmethod cl-ds:reset! ((range forward-chain-of-ranges))
  (init-chain-of-range range)
  range)


(defmethod cl-ds:at ((range random-access-chain-of-ranges) location &rest more-locations)
  (cl-ds:assert-one-dimension more-locations)
  (bind (((:slots %content) range))
    (iterate
      (for c in-vector %content)
      (sum (cl-ds:size c) into sum)
      (while (< sum location))
      (for total = sum)
      (for p-total previous total initially 0)
      (finally (cl-ds:at c (- location p-total))))))


(defmethod cl-ds:size ((range random-access-chain-of-ranges))
  (bind (((:slots %content) range)
         (count (flexichain:nb-elements %content)))
    (iterate
      (for i below count)
      (sum (~> %content (flexichain:element* i) cl-ds:size)))))


(defmethod cl-ds:consume-front ((range forward-chain-of-ranges))
  (bind (((:slots %content) range))
    (iterate
      (if (zerop (flexichain:nb-elements %content))
          (leave (values nil nil))
          (bind ((front (flexichain:element* %content 0))
                 ((:values value more) (cl-ds:consume-front front)))
            (if more
                (leave (values value t))
                (flexichain:pop-start %content)))))))


(defmethod cl-ds:peek-front ((range forward-chain-of-ranges))
  (bind (((:slots %content) range))
    (iterate
      (if (zerop (flexichain:nb-elements %content))
          (leave (values nil nil))
          (bind ((front (flexichain:element* %content 0))
                 ((:values value more) (cl-ds:peek-front front)))
            (if more
                (leave (values value t))
                (flexichain:pop-start %content)))))))


(defmethod cl-ds:consume-back ((range bidirectional-chain-of-ranges))
  (bind (((:slots %content) range))
    (iterate
      (for count = (flexichain:nb-elements %content))
      (if (zerop count)
          (leave (values nil nil))
          (bind ((back (flexichain:element* %content (1- count)))
                 ((:values value more) (cl-ds:consume-back back)))
            (if more
                (leave (values value t))
                (flexichain:pop-end %content)))))))


(defmethod cl-ds:peek-back ((range bidirectional-chain-of-ranges))
  (bind (((:slots %content) range))
    (iterate
      (for count = (flexichain:nb-elements %content))
      (if (zerop count)
          (leave (values nil nil))
          (bind ((back (flexichain:element* %content (1- count)))
                 ((:values value more) (cl-ds:peek-back back)))
            (if more
                (leave (values value t))
                (flexichain:pop-end %content)))))))


(defmethod cl-ds:empty-clone ((range forward-chain-of-ranges))
  (make (type-of range)
        :original-content (read-original-content range)))


(defmethod cl-ds:traverse ((range forward-chain-of-ranges) function)
  (ensure-functionf function)
  (bind (((:slots %content) range))
    (iterate
      (for i from 0 below (flexichain:nb-elements %content))
      (cl-ds:traverse (flexichain:element* %content i) function))
    range))


(defmethod cl-ds:across ((range forward-chain-of-ranges) function)
  (ensure-functionf function)
  (bind (((:slots %content range) range))
    (iterate
      (for i from 0 below (flexichain:nb-elements %content))
      (cl-ds:across (flexichain:element* %content i) function)))
  range)
