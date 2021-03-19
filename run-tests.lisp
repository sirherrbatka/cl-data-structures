(ql:quickload :cl-data-structures)
(ql:quickload :cl-data-structures-tests)

(setf prove:*enable-colors* nil)

(unwind-protect
     (let ((*error-output* (make-broadcast-stream)))
       (handler-bind
           ((lparallel.kernel:no-kernel-error
              (lambda (c)
                (declare (ignore c))
                (invoke-restart 'lparallel.kernel:make-kernel 4))))
         (prove:run :cl-data-structures-tests)))
  (cl-user::quit))
