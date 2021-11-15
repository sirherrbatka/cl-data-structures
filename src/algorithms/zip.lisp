(cl:in-package #:cl-data-structures.algorithms)


(defclass forward-connecting-range (cl-ds:fundamental-forward-range)
  ((%ranges :initarg :ranges
            :reader read-ranges)))


(defclass bidirectional-connecting-range (forward-connecting-range
                                          cl-ds:fundamental-bidirectional-range)
  ())


(defclass random-access-connecting-range (bidirectional-connecting-range
                                          cl-ds:fundamental-random-access-range)
  ())


(defclass chunkable-forward-connecting-range (cl-ds:chunking-mixin
                                              forward-connecting-range)
  ())


(defclass chunkable-bidirectional-connecting-range (cl-ds:chunking-mixin
                                                    bidirectional-connecting-range)
  ())


(defclass chunkable-random-access-connecting-range (cl-ds:chunking-mixin
                                                    random-access-connecting-range)
  ())


(defmethod cl-ds.utils:cloning-information append
    ((range forward-connecting-range))
  '((:ranges read-ranges)))


(defclass zipping-mixin (cl-ds:chunking-mixin)
  ((%function :initarg :function
              :reader read-function)))


(defclass forward-zipped-ranges (zipping-mixin
                                 forward-connecting-range)
  ())


(defmethod cl-ds.utils:cloning-information append
    ((range zipping-mixin))
  '((:function read-function)))


(defclass bidirectional-zipped-ranges (zipping-mixin
                                       bidirectional-connecting-range)
  ())


(defclass random-access-zipped-ranges (zipping-mixin
                                       random-access-connecting-range)
  ())


(defmethod cl-ds:clone ((range forward-connecting-range))
  (cl-ds.utils:quasi-clone
   range
   :ranges (mapcar #'cl-ds:clone (read-ranges range))))


(defun init-connected-ranges (obj)
  (bind (((:slots %ranges) obj))
    (map nil #'cl-ds:reset! %ranges)
    obj))


(defmethod cl-ds:reset! ((range forward-connecting-range))
  (init-connected-ranges range)
  range)


(defmethod initialize-instance :after ((range forward-connecting-range)
                                       &key &allow-other-keys)
  (init-connected-ranges range))


(defun zip-traversable (function ranges)
  (let* ((ranges (~>> (cl-ds.utils:if-else
                       (rcurry #'typep 'cl-ds:fundamental-forward-range)
                       #'cl-ds:clone #'cl-ds:whole-range)
                      (cl-ds.alg:on-each ranges)
                      to-list))
         (type (common-fundamental-range-class ranges)))
    (assert type)
    (make (eswitch (type)
            ('fundamental-forward-range 'forward-zipped-ranges)
            ('fundamental-bidirectional-range 'bidirectional-zipped-ranges)
            ('fundamental-random-access-range 'random-access-zipped-ranges))
          :ranges ranges
          :function function)))


(defun zip (function range &rest ranges)
  (zip-traversable function (cons range ranges)))


(defun connect-traversable (ranges)
  (let* ((ranges (~>> (cl-ds.utils:if-else
                       (rcurry #'typep 'cl-ds:fundamental-forward-range)
                       #'cl-ds:clone #'cl-ds:whole-range)
                      (cl-ds.alg:on-each ranges)
                      to-list))
         (type (common-fundamental-range-class ranges)))
    (assert type)
    (make (eswitch (type)
            ('fundamental-forward-range 'chunkable-forward-connecting-range)
            ('fundamental-bidirectional-range 'chunkable-bidirectional-connecting-range)
            ('fundamental-random-access-range 'chunkable-random-access-connecting-range))
          :ranges ranges)))


(defun connect (range &rest ranges)
  (connect-traversable (cons range ranges)))


(defmethod cl-ds:consume-front ((range forward-connecting-range))
  (block nil
    (bind (((:slots %ranges) range))
      (values (mapcar (lambda (x)
                        (multiple-value-bind (value more)
                            (cl-ds:consume-front x)
                          (when (null more)
                            (return-from nil (values nil nil)))
                          value))
                      %ranges)
              t))))


(defmethod cl-ds:consume-front ((range zipping-mixin))
  (bind (((:slots %function) range)
         ((:values values more) (call-next-method)))
    (if more
        (values (apply %function values) t)
        (values nil nil))))


(defmethod cl-ds:consume-back ((range bidirectional-connecting-range))
  (block nil
    (bind (((:slots %ranges) range))
      (values (mapcar (lambda (x)
                        (multiple-value-bind (value more)
                            (cl-ds:consume-back x)
                          (when (null more)
                            (return-from nil (values nil nil)))
                          value))
                      %ranges)
              t))))


(defmethod cl-ds:consume-back ((range zipping-mixin))
  (bind (((:slots %function) range)
         ((:values values more) (call-next-method)))
    (if more
        (values (apply %function values) t)
        (values nil nil))))


(defmethod cl-ds:peek-front ((range forward-connecting-range))
  (block nil
    (bind (((:slots %ranges) range))
      (values (mapcar (lambda (x)
                        (multiple-value-bind (value more)
                            (cl-ds:peek-front x)
                          (when (null more)
                            (return-from nil (values nil nil)))
                          value))
                      %ranges)
              t))))


(defmethod cl-ds:peek-front ((range zipping-mixin))
  (bind (((:slots %function) range)
         ((:values values more) (call-next-method)))
    (if more
        (values (apply %function values) t)
        (values nil nil))))


(defmethod cl-ds:peek-back ((range bidirectional-connecting-range))
  (block nil
    (bind (((:slots %ranges) range))
      (values (mapcar (lambda (x)
                        (multiple-value-bind (value more)
                            (cl-ds:peek-back x)
                          (when (null more)
                            (return-from nil (values nil nil)))
                          value))
                      %ranges)
              t))))


(defmethod cl-ds:peek-back ((range zipping-mixin))
  (bind (((:slots %function) range)
         ((:values values more) (call-next-method)))
    (if more
        (values (apply %function values) t)
        (values nil nil))))


(defmethod cl-ds:at ((range random-access-connecting-range) location
                     &rest more-locations)
  (cl-ds:assert-one-dimension more-locations)
  (block nil
    (bind (((:slots %ranges) range))
      (values (mapcar (lambda (x)
                        (multiple-value-bind (value more)
                            (cl-ds:at x location)
                          (when (null more)
                            (return-from nil (values nil nil)))
                          value))
                      %ranges)
              t))))


(defmethod cl-ds:at ((range zipping-mixin) location &rest more-locations)
  (declare (ignore more-locations))
  (bind (((:slots %function) range)
         ((:values values more) (call-next-method)))
    (if more
        (values (apply %function values) t)
        (values nil nil))))


(defmethod cl-ds:size ((range random-access-connecting-range))
  (bind (((:slots %ranges) range))
    (iterate
      (for range in %ranges)
      (minimize (cl-ds:size range)))))
