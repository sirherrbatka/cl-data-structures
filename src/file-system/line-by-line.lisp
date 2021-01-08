(cl:in-package #:cl-data-structures.file-system)


(defclass line-by-line-range (cl-ds:chunking-mixin
                              file-range-mixin
                              cl-ds:fundamental-forward-range)
  ((%lines-read :initarg :lines-read
                :accessor access-lines-read)
   (%initial-lines-read :initarg :lines-read
                        :reader read-initial-lines-read))
  (:default-initargs :initial-position 0 :lines-read 0))


(defmethod cl-ds:clone ((range line-by-line-range))
  (declare (optimize (debug 3)))
  (let ((current-position (access-current-position range)))
    (close-stream range)
    (make 'line-by-line-range
          :path (read-path range)
          :lines-read (access-lines-read range)
          :reached-end (access-reached-end range)
          :initial-position current-position)))


(defmethod cl-ds:peek-front ((range line-by-line-range))
  (if (access-reached-end range)
      (values nil nil)
      (let* ((stream (ensure-stream range))
             (file-position (file-position stream))
             (line (read-line stream :eof-value nil)))
        (if (null line)
            (values nil nil)
            (progn
              (unless (file-position (read-stream range)
                                     file-position)
                (error 'cl-ds:file-releated-error
                       :format-control "Can't set position in the stream."
                       :path (read-path range)))
              (values line t))))))


(defmethod set-stream-position ((range line-by-line-range) stream)
  (or (ignore-errors (call-next-method range stream))
      (iterate
        (repeat (access-lines-read range))
        (read-line stream))))


(defmethod cl-ds:reset! ((range line-by-line-range))
  (setf (access-lines-read range) (read-initial-lines-read range))
  (call-next-method)
  range)


(defmethod cl-ds:consume-front ((range line-by-line-range))
  (if (access-reached-end range)
      (values nil nil)
      (let* ((stream (ensure-stream range))
             (line (read-line stream nil nil)))
        (call-next-method range)
        (if (null line)
            (values nil nil)
            (progn
              (incf (access-lines-read range))
              (values line t))))))


(defmethod cl-ds:traverse ((range line-by-line-range) function)
  (unless (~> range access-reached-end)
    (ensure-stream range)
    (unwind-protect
         (iterate
           (with stream = (read-stream range))
           (for line = (read-line stream nil nil))
           (until (null line))
           (incf (access-lines-read range))
           (funcall function line))
      (setf (access-current-position range) (~> range
                                                read-stream
                                                file-position))
      (close-stream range)))
  range)


(defmethod cl-ds:across ((range line-by-line-range) function)
  (unless (~> range access-reached-end)
    (with-stream-input (stream range)
      (iterate
        (for line = (read-line stream nil nil))
        (until (null line))
        (funcall function line))))
  range)


(defun line-by-line (path)
  (check-type path stream-designator)
  (make 'line-by-line-range
        :path path))
