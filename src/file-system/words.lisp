(cl:in-package #:cl-data-structures.file-system)


(defconstant +buffer-size+ 128)


(defclass words-range (cl-ds:chunking-mixin
                       file-range-mixin
                       cl-ds:fundamental-forward-range)
  ((%path :initarg :path
          :type (or string pathname)
          :reader read-path)
   (%fptr :initarg :fptr
           :accessor access-fptr)
   (%p :initarg :p
       :accessor access-p)
   (%buffer :initarg :buffer
            :type simple-string
            :accessor access-buffer))
  (:default-initargs
   :buffer (make-string (* 2 +buffer-size+))
   :initial-position 0
   :p -1
   :fptr 0))


(defmethod cl-ds:clone ((range words-range))
  (make 'words-range
        :path (read-path range)
        :fptr (access-fptr range)
        :initial-position (access-current-position range)
        :p (access-p range)
        :reached-end (access-reached-end range)
        :buffer (copy-array (access-buffer range))))


(defun read-word (stream range)
  (bind (((:accessors (buffer access-buffer)
                      (fptr access-fptr)
                      (current-position access-current-position)
                      (p access-p))
          range)
         (minimum-room (length buffer))
         (start 0)
         (c nil))
    (labels ((underflow ()
               (replace buffer buffer :start2 start :end2 p)
               (let ((length (length buffer)))
                 (when (<= (- length p) minimum-room)
                   (setf buffer (adjust-array buffer (+ length minimum-room))
                         minimum-room (* minimum-room 2))))
               (decf p start)
               (setf start 0)
               (setf fptr (read-sequence buffer stream :start p))
               (incf current-position fptr))
             (char-space-p ()
               (or (char-equal c #\Space)
                   (char-equal c #\Tab)
                   (char-equal c #\-)
                   (char-equal c #\–)
                   (char-equal c #\/)
                   (char-equal c #\—)))
             (report-word (end)
               (return-from read-word (values buffer start end)))
             (new-line-p ()
               (member c '(#.#\Return #.#\newline #.#\Linefeed) :test 'char-equal))
             (consume ()
               (when c
                 (incf p)
                 (when (= p fptr) (underflow)))
               (setf c (if (= p fptr) nil (char buffer p))))
             (consume-whitespace ()
               (loop
                 :while (and c (not (alpha-char-p c))) ; words are supposed to start with alpha-char
                 :do (consume)))
             (read-data ()
               (consume-whitespace)
               (unless (null c)
                 (let ((count 0)
                       (trailing-non-alpha-char-count 0))
                   (setf start p)
                   (loop
                     :until (or (null c) (char-space-p) (new-line-p))
                     :do (progn
                           (if (alpha-char-p c)
                               (setf trailing-non-alpha-char-count 0)
                               (incf trailing-non-alpha-char-count))
                           (consume)
                           (incf count)))
                   (report-word (+ start count (- trailing-non-alpha-char-count)))))))
      (declare (inline underflow report-word new-line-p consume consume-whitespace read-data))
      (when (< p 0)
        (setf p 0)
        (underflow))
      (setf c (if (= p fptr) nil (char buffer p)))
      (loop :until (null c) :do (read-data))
      (setf (access-reached-end range) t)
      (values nil nil nil))))


(defmethod cl-ds:consume-front ((range words-range))
  (if (access-reached-end range)
      (values nil nil)
      (bind (((:values buffer start end) (read-word (ensure-stream range)
                                                    range)))
        (if (null buffer)
            (values nil nil)
            (values (subseq buffer start end) t)))))


(defmethod cl-ds:traverse ((range words-range) function)
  (cl-ds.fs:with-file-ranges ((r range))
    (declare (ignore r))
    (iterate
      (with stream = (ensure-stream range))
      (for (values buffer start end) = (read-word stream range))
      (while buffer)
      (funcall function (subseq buffer start end))))
  range)


(defun words (path)
  (make 'words-range :path path))

