(cl:in-package #:cl-data-structures.algorithms)


(defclass transparent-to-chunking-mixin ()
  ())


(defclass proxy-range ()
  ((%original-range :initarg :original-range
                    :reader read-original-range)))


(defmethod cl-ds.alg.meta:across-aggregate ((range proxy-range) function)
  (~> range
      read-original-range
      (cl-ds.alg.meta:across-aggregate function)))


(defalias clone-of-original-range (compose
                                   #'cl-ds:clone
                                   #'read-original-range))


(defmethod cl-ds.utils:cloning-information append
    ((range proxy-range))
  '((:original-range clone-of-original-range)))


(defclass chunked-proxy-range (cl-ds:chunking-mixin
                               proxy-range
                               cl-ds:fundamental-forward-range)
  ((%chunked-range :initarg :chunked-range
                   :reader read-chunked-range)))


(defmethod cl-ds:forward-call ((range proxy-range) function)
  (funcall function (read-original-range range)))


(defmethod cl-ds:reset! ((range proxy-range))
  (cl-ds:reset! (read-original-range range))
  range)


(defmethod cl-ds:clone ((range proxy-range))
  (cl-ds.utils:clone range))


(defmethod cl-ds:traverse ((range proxy-range) function)
  (cl-ds:traverse (read-original-range range) function))


(defmethod cl-ds:across ((range proxy-range) function)
  (cl-ds:across (read-original-range range) function))


(defgeneric wrap-chunk (range chunk))


(defmethod cl-ds:consume-front ((range chunked-proxy-range))
  (if-let ((chunk (~> range read-chunked-range cl-ds:consume-front)))
    (values (wrap-chunk (read-original-range range) chunk) t)
    (values nil nil)))


(defmethod cl-ds:chunked ((range proxy-range) &optional chunk-size-hint)
  (if-let ((method (c2mop:compute-applicable-methods-using-classes
                    #'wrap-chunk
                    (list (class-of range)
                          (find-class 'cl-ds:fundamental-forward-range))))
           (chunked (~> range read-original-range
                        (cl-ds:chunked chunk-size-hint))))
    (make 'chunked-proxy-range
          :original-range range
          :chunked-range chunked)
    nil))


(defmethod cl-ds:chunked ((range transparent-to-chunking-mixin) &optional chunk-size-hint)
  (~> range read-original-range (cl-ds:chunked chunk-size-hint)))


(defclass forward-proxy-range (proxy-range fundamental-forward-range)
  ())


(defclass bidirectional-proxy-range (proxy-range fundamental-bidirectional-range)
  ())


(defclass random-access-proxy-range (proxy-range fundamental-random-access-range)
  ())


(defgeneric make-proxy (range class
                        &rest all
                        &key &allow-other-keys)
  (:method ((range cl-ds:traversable)
            class &rest all &key &allow-other-keys)
    (apply #'make-instance class :original-range range all)))


(defclass hash-table-range (fundamental-random-access-range
                            key-value-range)
  ((%hash-table :initarg :hash-table
                :reader read-hash-table)
   (%keys :initarg :keys
          :reader read-keys)
   (%begin :initarg :begin
           :type fixnum
           :accessor access-begin)
   (%initial-begin :initarg :begin
                   :type fixnum
                   :reader read-initial-begin)
   (%end :initarg :end
         :type fixnum
         :accessor access-end)
   (%initial-end :initarg :end
                 :type fixnum
                 :reader read-initial-end)))


(defmethod print-object ((obj hash-table-range) stream)
  (print-unreadable-object (obj stream :type t)
    (let ((count 5)
          (current 0)
          (total-size (hash-table-count (read-hash-table obj))))
      (block map-block
        (maphash (lambda (key value) (declare (ignore value))
                   (format stream "{~a}" key)
                   (incf current)
                   (when (eql current count)
                     (return-from map-block)))
                 (read-hash-table obj)))
      (when (> total-size count)
        (format stream "..."))
      obj)))


(defmethod cl-ds:traverse ((range hash-table-range) function)
  (let ((keys (read-keys range))
        (table (read-hash-table range)))
    (iterate
      (for i from (access-begin range) below (access-end range))
      (for key = (aref keys i))
      (funcall function (list* key (gethash key table))))))


(defmethod cl-ds:reset! ((range hash-table-range))
  (setf (access-begin range) (read-initial-begin range)
        (access-end range) (read-initial-end range))
  range)


(defmethod cl-ds.utils:cloning-information append
    ((range hash-table-range))
  '((:keys read-keys)
    (:begin access-begin)
    (:end access-end)
    (:hash-table read-hash-table)))


(defmethod clone ((range hash-table-range))
  (cl-ds.utils:clone range))


(defmethod consume-front ((range forward-proxy-range))
  (consume-front (read-original-range range)))


(defmethod consume-back ((range bidirectional-proxy-range))
  (consume-back (read-original-range range)))


(defmethod peek-front ((range forward-proxy-range))
  (peek-front (read-original-range range)))


(defmethod peek-back ((range bidirectional-proxy-range))
  (peek-back (read-original-range range)))


(defmethod at ((range random-access-proxy-range) location &rest more)
  (apply #'at (read-original-range range) location more))


(defmethod consume-front ((range hash-table-range))
  (bind (((:slots (begin %begin) (end %end) (ht %hash-table) (keys %keys))
          range))
    (if (eql begin end)
        (values nil nil)
        (let* ((key (aref keys begin))
               (value (gethash key ht)))
          (incf begin)
          (values (list* key value) t)))))


(defmethod consume-back ((range hash-table-range))
  (bind (((:slots (begin %begin) (end %end) (ht %hash-table) (keys %keys))
          range))
    (if (eql begin end)
        (values nil nil)
        (let* ((key (aref keys end))
               (value (gethash key ht)))
          (decf end)
          (values (list* key value) t)))))


(defmethod peek-front ((range hash-table-range))
  (bind (((:slots (begin %begin) (end %end) (ht %hash-table) (keys %keys))
          range))
    (if (eql begin end)
        (values nil nil)
        (let* ((key (aref keys begin))
               (value (gethash key ht)))
          (values (list* key value) t)))))


(defmethod peek-back ((range hash-table-range))
  (bind (((:slots (begin %begin) (end %end) (ht %hash-table) (keys %keys))
          range))
    (if (eql begin end)
        (values nil nil)
        (let* ((key (aref keys end))
               (value (gethash key ht)))
          (values (list* key value) t)))))


(defmethod drop-front ((range hash-table-range) count)
  (bind (((:slots (begin %begin) (end %end)) range)
         (count (min end (+ begin count))))
    (setf begin count))
  range)


(defmethod drop-back ((range hash-table-range) count)
  (with-slots ((begin %begin) (end %end)) range
    (setf end (max begin (- end count))))
  range)


(defmethod at ((range hash-table-range) location &rest more)
  (assert (null more))
  (bind (((:slots (ht %hash-table)) range)
         ((:values result found) (gethash location ht))
         (test (hash-table-test ht))
         (begin (access-begin range))
         (end (access-end range))
         (keys (read-keys range)))
    (cond
      ((not found)
       (values nil nil))
      ((and (zerop begin)
            (eql (length keys) end))
       (values result t))
      ((iterate
         (for i from begin below end)
         (for l = (aref keys i))
         (finding t such-that (funcall test l location)))
       (values result t))
      (t (values nil nil)))))


(defmethod size ((range hash-table-range))
  (bind (((:slots %end %begin) range))
    (- %end %begin)))


(defun make-hash-table-range (hash-table)
  (let ((keys (~> hash-table hash-table-keys (coerce 'vector))))
    (make-instance 'hash-table-range
                   :hash-table hash-table
                   :keys keys
                   :begin 0
                   :end (length keys))))


(defvar *current-key*)
