(cl:in-package #:cl-data-structures)


(defclass expression (chunking-mixin
                      fundamental-forward-range)
  ((%construct-function :initarg :construct-function
                        :type function
                        :reader read-body)
   (%arguments :initarg :arguments
               :initform nil
               :reader read-arguments)
   (%arguments-closure :accessor access-arguments-closure
                       :initarg :arguments-closure)
   (%finished-closure :accessor access-finished-closure
                      :initarg :finished-closure)
   (%closure :accessor access-closure
             :initarg :closure)))


(defmethod clone ((obj expression))
  (bind (((:slots %construct-function %arguments
                  %closure %arguments-closure)
          obj)
         ((:values result-closure arguments-closure finished-closure)
          (apply %construct-function
                 (funcall %arguments-closure)))
         (result (make 'expression
                       :construct-function %construct-function
                       :arguments-closure arguments-closure
                       :finished-closure finished-closure
                       :arguments %arguments
                       :closure result-closure)))
    result))


(defmethod initialize-instance :after ((obj expression) &rest all
                                       &key &allow-other-keys)
  (declare (ignore all))
  (unless (slot-boundp obj '%closure)
    (reset! obj)))


(defmacro xpr (arguments &body body)
  (let ((keys (plist-keys arguments)))
    (with-gensyms (!fn)
      `(cl-ds.utils:let-generator
           ((,!fn ,(mapcar (lambda (x) (intern (symbol-name x))) keys)
                  ,@body))
           cl-ds:send-finish
           cl-ds:finish
           cl-ds:recur
           cl-ds:send-recur
         (make 'cl-ds:expression
               :construct-function (function ,!fn)
               :arguments (list ,@arguments))))))


(defmethod traverse ((obj expression) function)
  (declare (optimize (speed 3) (debug 0) (space 0)))
  (let ((fn (ensure-function (access-closure obj)))
        (function (ensure-function function)))
    (declare (type (-> (t) t) function)
             (type (-> (&optional boolean) t) fn))
    (iterate
      (for (values value not-finished) = (funcall fn))
      (while not-finished)
      (funcall function value)))
  obj)


(defmethod across ((obj expression) function)
  (declare (optimize (speed 3) (debug 0) (space 0)))
  (let ((function (ensure-function (ensure-function function))))
    (declare (type (-> (t) t) function))
    (bind (((:slots %construct-function %closure %arguments-closure) obj)
           (fn (ensure-function (apply %construct-function
                                       (funcall %arguments-closure)))))
      (declare (type (-> (&optional boolean) t) fn))
      (iterate
        (for (values value not-finished) = (funcall fn))
        (while not-finished)
        (funcall function value))
      obj)))


(defmethod consume-front ((obj expression))
  (funcall (access-closure obj)))


(defmethod peek-front ((obj expression))
  (bind (((:slots %closure %construct-function %arguments-closure %finished-closure)
          obj))
    (if (funcall %finished-closure)
        (values nil nil)
        (funcall (apply %construct-function (funcall %arguments-closure))))))


(defmethod reset! ((obj expression))
  (bind (((:slots %construct-function %arguments %closure
                  %arguments-closure %finished-closure)
          obj)
         ((:values function arguments-closure finished-closure)
          (apply %construct-function %arguments)))
    (setf %closure function
          %finished-closure finished-closure
          %arguments-closure arguments-closure))
  obj)


(defmethod drop-front ((obj expression) count)
  (check-type count non-negative-fixnum)
  (iterate
    (with function = (access-closure obj))
    (repeat count)
    (for i from 0)
    (for (values value more) = (funcall function))
    (while more)
    (finally (return i))))


(defun path-scaner (path &key flatten-lists flatten-vectors)
  (lambda (object)
    (cl-ds:xpr (:stack (list (list t path (list object))))
      (when (endp stack)
        (cl-ds:finish))
      (bind ((((descend (first-path-element . rest-path) object-path) . rest) stack))
        (if descend
            (if (listp first-path-element)
                (iterate
                  (for element in first-path-element)
                  (push (list t (cons element rest-path) object-path)
                        rest)
                  (finally (cl-ds:recur :stack rest)))
                (bind (((:values next-object found)
                        (funcall first-path-element (first object-path)))
                       (new-path-to-object (cons next-object object-path)))
                  (cl-ds.utils:cond+ ((endp rest-path) found)
                    ((t t)
                     (cond ((and flatten-vectors
                                 (vectorp next-object))
                            (cl-ds:recur
                                   :stack (iterate
                                            (for i from (~> next-object length 1-) downto 0)
                                            (for obj = (aref next-object i))
                                            (push (list nil
                                                        (list first-path-element)
                                                        (cons obj object-path))
                                                  rest)
                                            (finally (return rest)))))
                           ((and flatten-lists
                                 (listp next-object))
                            (cl-ds:recur
                             :stack (iterate
                                      (for obj in next-object)
                                      (push (list nil
                                                  (list first-path-element)
                                                  (cons obj object-path))
                                            rest)
                                      (finally (return rest)))))
                       (t (cl-ds:send-recur new-path-to-object :stack rest))))
                    ((t nil)
                     (cl-ds:recur :stack rest))
                    ((nil t)
                     (cond ((and flatten-vectors
                                  (vectorp next-object))
                             (cl-ds:recur
                                :stack (iterate
                                         (for i from (~> next-object length 1-) downto 0)
                                         (for obj = (aref next-object i))
                                         (push (list t
                                                     rest-path
                                                     (cons obj object-path))
                                               rest)
                                         (finally (return rest)))))
                            ((and flatten-lists
                                  (listp next-object))
                             (cl-ds:recur
                              :stack (iterate
                                       (for obj in next-object)
                                       (push (list t
                                                   rest-path
                                                   (cons obj object-path))
                                             rest)
                                       (finally (return rest)))))
                            (t (cl-ds:recur :stack (cons (list t rest-path (cons next-object object-path))
                                                         rest)))))
                    ((nil nil)
                     (cl-ds:recur :stack rest)))))
            (cl-ds:send-recur object-path :stack rest))))))
