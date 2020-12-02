(cl:in-package #:cl-data-structures.file-system)


(defmethod open-stream-designator ((designator pathname))
  (open designator))


(defmethod open-stream-designator ((designator string))
  (open designator))


(defgeneric stream-designator-p (designator))


(defmethod stream-designator-p ((designator pathname))
  t)


(defmethod stream-designator-p ((designator string))
  t)


(deftype stream-designator ()
  '(satisfies stream-designator-p))


(defclass file-range-mixin ()
  ((%reached-end :initarg :reached-end
                 :type boolean
                 :accessor access-reached-end
                 :initform nil)
   (%current-position :initarg :initial-position
                      :accessor access-current-position
                      :type non-negative-integer)
   (%initial-position :initarg :initial-position
                      :type non-negative-integer
                      :reader read-initial-position)
   (%stream :initarg :stream
            :initform (list nil)
            :type list)
   (%path :initarg :path
          :reader read-path)
   (%mutex :initform (bt:make-lock)
           :reader read-mutex)))


(defgeneric set-stream-position (range stream))
(defmethod set-stream-position ((range file-range-mixin) stream)
  (let ((position (access-current-position range)))
    (unless (or (zerop position)
                (file-position stream position))
      (error 'cl-ds:file-releated-error
             :format-control "Can't set position in the stream."
             :path (read-path range)))))


(defmacro with-stream-input ((stream range) &body body)
  (with-gensyms (!stream)
    (once-only (range)
      `(let* ((,stream (open-stream-designator (read-path ,range)))
              (,!stream ,stream))
         (unwind-protect
              (progn
                (set-stream-position ,range ,stream)
                ,@body)
           (close ,!stream))))))


(defmethod cl-ds.utils:cloning-information append ((object file-range-mixin))
  '((:reached-end access-reached-end)
    (:initial-position access-current-position)
    (:path read-path)))


(defmethod cl-ds:reset! ((range file-range-mixin))
  (setf (access-current-position range) (read-initial-position range))
  (ensure-stream range)
  (unless (file-position (read-stream range)
                         (read-initial-position range))
    (close-stream range)
    (error 'cl-ds:file-releated-error
           :format-control "Can't change position in the stream."))
  (setf (access-reached-end range) nil)
  range)


(defmethod cl-ds:consume-front ((range file-range-mixin))
  (let ((stream (read-stream range)))
    (setf (access-current-position range) (file-position stream))
    (when (eq :eof (peek-char t stream nil :eof))
      (setf (access-reached-end range) t)
      (close-stream range))))


(defun read-stream (object)
  (check-type object file-range-mixin)
  (car (slot-value object '%stream)))


(defun ensure-stream (range)
  (bt:with-lock-held ((read-mutex range))
    (when (~> range read-stream null)
      (let ((current-position (access-current-position range))
            (file (~> range read-path open-stream-designator)))
        (unless (or (zerop current-position)
                    (file-position file (access-current-position range)))
          (close file)
          (error 'cl-ds:file-releated-error
                 :path (read-path range)
                 :format-control "Can't change position in the stream."))
        (setf (car (slot-value range '%stream)) file
              (access-reached-end range) nil)))
    (read-stream range)))


(defun close-silence-errors (stream) ; in case if closing already close streams produces error
  (handler-case (close stream)
    (stream-error (e) (declare (ignore e)))))


(more-conditions:define-condition-translating-method close-inner-stream
    ((error cl-ds:file-releated-error)))



(defgeneric close-inner-stream (range)
  (:method ((range cl-ds:fundamental-forward-range))
    (cl-ds:forward-call range #'close-inner-stream))
  (:method ((range file-range-mixin))
    (when-let ((stream (read-stream range)))
      (close-silence-errors stream)
      (setf (car (slot-value range '%stream)) nil))
    range))


(defmacro with-file-ranges (bindings &body body)
  (let ((extra-vars (mapcar (lambda (x) (gensym)) bindings))
        (prime-vars (mapcar #'first bindings)))
    `(let (,@extra-vars)
       (declare (ignorable ,@extra-vars))
       (unwind-protect
            (progn
              ,@(mapcar (lambda (extra x)
                          `(setf ,extra ,(second x)))
                        extra-vars
                        bindings)
              (let ,(mapcar #'list prime-vars extra-vars)
                ,@body))
         (progn
           ,@(mapcar (lambda (x)
                       `(unless (null ,x)
                          (close-inner-stream ,x)))
                     extra-vars))))))


(defun enclose-finalizer (stream-cons)
  (lambda ()
    (unless (null (car stream-cons))
      (close-silence-errors (car stream-cons)))))


(defmethod initialize-instance :after ((range file-range-mixin)
                                       &rest all)
  (declare (ignore all))
  (trivial-garbage:finalize range
                            (enclose-finalizer (slot-value range '%stream))))


(defun close-stream (range)
  (bt:with-lock-held ((read-mutex range))
    (unless (~> range read-stream null)
      (~> range read-stream close-silence-errors)
      (setf (car (slot-value range '%stream)) nil))))


(defmethod cl-ds:across ((range file-range-mixin) function)
  (~> range cl-ds:clone (cl-ds:traverse function))
  range)
