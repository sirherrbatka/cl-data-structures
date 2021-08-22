(cl:in-package #:cl-data-structures.meta)


(defclass functional-function ()
  ())


(defclass destructive-function ()
  ())


(defclass grow-function ()
  ())


(defclass shrink-function ()
  ())


(defclass insert-function (grow-function)
  ())


(defclass update-function (grow-function)
  ())


(defclass update-if-function (grow-function)
  ())


(defclass add-function (grow-function)
  ())


(defclass erase-function (shrink-function)
  ())


(defclass erase-if-function (shrink-function)
  ())


(defclass put-function (grow-function)
  ())


(defclass take-out-function (shrink-function)
  ())


(defclass take-out-back-function (shrink-function)
  ())


(defclass take-out-front-function (shrink-function)
  ())


(defclass put-back-function (grow-function)
  ())


(defclass put-front-function (grow-function)
  ())


(defclass take-out!-function (closer-mop:standard-generic-function
                              destructive-function
                              take-out-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass take-out-back!-function (closer-mop:standard-generic-function
                                   destructive-function
                                   take-out-back-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass take-out-front!-function (closer-mop:standard-generic-function
                                    destructive-function
                                    take-out-front-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass put-back!-function (closer-mop:standard-generic-function
                              destructive-function
                              put-back-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass put-front!-function (closer-mop:standard-generic-function
                               destructive-function
                               put-front-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass functional-update-if-function (closer-mop:standard-generic-function
                                         functional-function
                                         update-if-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass functional-insert-function (closer-mop:standard-generic-function
                                      functional-function
                                      insert-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass functional-update-function (closer-mop:standard-generic-function
                                      functional-function
                                      update-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass functional-add-function (closer-mop:standard-generic-function
                                   functional-function
                                   add-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass functional-erase-function (closer-mop:standard-generic-function
                                     functional-function
                                     erase-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass functional-erase-if-function (closer-mop:standard-generic-function
                                        functional-function
                                        erase-if-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass functional-put-function (closer-mop:standard-generic-function
                                   functional-function
                                   put-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass functional-take-out-function (closer-mop:standard-generic-function
                                        functional-function
                                        take-out-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass functional-take-out-back-function (closer-mop:standard-generic-function
                                             functional-function
                                             take-out-back-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass functional-take-out-front-function (closer-mop:standard-generic-function
                                              functional-function
                                              take-out-front-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass functional-put-back-function (closer-mop:standard-generic-function
                                        functional-function
                                        put-back-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass functional-put-front-function (closer-mop:standard-generic-function
                                         functional-function
                                         put-front-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass insert!-function (closer-mop:standard-generic-function
                            destructive-function
                            insert-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass update!-function (closer-mop:standard-generic-function
                            destructive-function
                            update-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass update-if!-function (closer-mop:standard-generic-function
                               destructive-function
                               update-if-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass add!-function (closer-mop:standard-generic-function
                         destructive-function
                         add-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass erase-if!-function (closer-mop:standard-generic-function
                              destructive-function
                              erase-if-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass erase*-function (closer-mop:standard-generic-function
                           functional-function
                           erase-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass erase*!-function (closer-mop:standard-generic-function
                            destructive-function
                            erase-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass erase!-function (closer-mop:standard-generic-function
                           destructive-function
                           erase-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass put!-function (closer-mop:standard-generic-function
                         destructive-function
                         put-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defgeneric pass-bucket-operation (operation container &rest arguments))


(defgeneric pass-bucket-query (container &rest arguments))


(defgeneric make-bucket-from-multiple (operation container data
                                       &rest all
                                       &key &allow-other-keys)
  (:method (operation container data &rest all)
    (apply #'pass-bucket-operation container operation data all)))


(define-constant null-bucket 'null-bucket)


(defgeneric map-bucket (container bucket function)
  (:method (container (bucket (eql null-bucket)) function)
    nil)
  (:method (container (bucket t) function)
    (funcall function bucket))
  (:method (container (bucket sequence) function)
    (map nil function bucket)))


(defgeneric position-modification (operation
                                   structure
                                   container
                                   location
                                   &rest all
                                   &key &allow-other-keys))


(declaim (inline null-bucket-p))
(defun null-bucket-p (bucket)
  (eq bucket 'null-bucket))


(defgeneric functional-counterpart (operation))


(defgeneric destructive-counterpart (operation))


(defgeneric fresh-bucket-status (operation value))


(defmethod fresh-bucket-status ((operation cl-ds.meta:update-function) value)
  cl-ds.common:empty-eager-modification-operation-status)


(defmethod fresh-bucket-status ((operation cl-ds.meta:update-if-function) value)
  cl-ds.common:empty-eager-modification-operation-status)


(defmethod fresh-bucket-status ((operation cl-ds.meta:update!-function) value)
  cl-ds.common:empty-eager-modification-operation-status)


(defmethod fresh-bucket-status ((operation cl-ds.meta:update-if!-function) value)
  cl-ds.common:empty-eager-modification-operation-status)


(defmethod fresh-bucket-status (operation value)
  (cl-ds.common:make-eager-modification-operation-status
   nil nil t))


(defgeneric make-bucket (operation container value status
                         &rest all)
  (:method (operation container value status &rest all)
    (declare (ignore all container))
    (values (cl-ds:force value)
            status)))


(defgeneric alter-bucket! (operation container value bucket &rest all &key &allow-other-keys))


(defmethod alter-bucket! ((operation cl-ds.meta:update!-function) container value bucket &rest all)
  (declare (ignore all))
  (values (cl-ds:force value) (cl-ds.common:make-eager-modification-operation-status t bucket t)))


(defmethod alter-bucket! ((operation cl-ds.meta:insert!-function) container value bucket &rest all)
  (declare (ignore all))
  (values (cl-ds:force value) (cl-ds.common:make-eager-modification-operation-status t bucket t)))


(defmethod alter-bucket! ((operation cl-ds.meta:add!-function) container value bucket &rest all)
  (declare (ignore all))
  (values null-bucket
          (cl-ds.common:make-eager-modification-operation-status t bucket nil)))


(defmethod alter-bucket! ((operation cl-ds.meta:erase!-function) container value bucket &rest all)
  (declare (ignore all))
  (values null-bucket (cl-ds.common:make-eager-modification-operation-status t bucket t)))


(defmethod alter-bucket! ((operation cl-ds.meta:erase-if!-function) container value bucket
                          &rest all &key condition-fn)
  (declare (ignore all))
  (if (funcall condition-fn bucket)
      (values null-bucket (cl-ds.common:make-eager-modification-operation-status t bucket t))
      (values bucket (cl-ds.common:make-eager-modification-operation-status t bucket nil))))


(defmethod alter-bucket! ((operation cl-ds.meta:insert!-function) container value bucket &rest all)
  (declare (ignore all))
  (values (cl-ds:force value) (cl-ds.common:make-eager-modification-operation-status t bucket t)))


(defmethod alter-bucket! ((operation cl-ds.meta:add!-function) container value bucket &rest all)
  (declare (ignore all))
  (values null-bucket
          (cl-ds.common:make-eager-modification-operation-status t bucket nil)))


(defgeneric alter-bucket (operation container value bucket &rest all &key &allow-other-keys))


(defmethod alter-bucket ((operation cl-ds.meta:update-function) container value bucket &rest all)
  (declare (ignore all))
  (values (cl-ds:force value) (cl-ds.common:make-eager-modification-operation-status t bucket t)))


(defmethod alter-bucket ((operation cl-ds.meta:insert-function) container value bucket &rest all)
  (declare (ignore all))
  (values (cl-ds:force value) (cl-ds.common:make-eager-modification-operation-status t bucket t)))


(defmethod alter-bucket ((operation cl-ds.meta:add-function) container value bucket &rest all)
  (declare (ignore all))
  (values null-bucket
          (cl-ds.common:make-eager-modification-operation-status t bucket nil)))


(defmethod alter-bucket ((operation cl-ds.meta:erase-function) container value bucket &rest all)
  (declare (ignore all))
  (values null-bucket (cl-ds.common:make-eager-modification-operation-status t bucket t)))


(defmethod alter-bucket ((operation cl-ds.meta:erase-if-function) container value bucket
                          &rest all &key condition-fn)
  (declare (ignore all))
  (if (funcall condition-fn bucket)
      (values null-bucket (cl-ds.common:make-eager-modification-operation-status t bucket t))
      (values bucket (cl-ds.common:make-eager-modification-operation-status t bucket nil))))
