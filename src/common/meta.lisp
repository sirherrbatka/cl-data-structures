(in-package #:cl-ds.common)


(defmethod cl-ds.meta:alter-bucket! ((operation cl-ds.meta:update!-function) container value bucket &rest all)
  (declare (ignore all))
  (values (cl-ds:force value) (cl-ds.common:make-eager-modification-operation-status t bucket t)))


(defmethod cl-ds.meta:alter-bucket! ((operation cl-ds.meta:insert!-function) container value bucket &rest all)
  (declare (ignore all))
  (values (cl-ds:force value) (cl-ds.common:make-eager-modification-operation-status t bucket t)))


(defmethod cl-ds.meta:alter-bucket! ((operation cl-ds.meta:add!-function) container value bucket &rest all)
  (declare (ignore all))
  (values cl-ds.meta:null-bucket
          (cl-ds.common:make-eager-modification-operation-status t bucket nil)))


(defmethod cl-ds.meta:alter-bucket! ((operation cl-ds.meta:erase!-function) container value bucket &rest all)
  (declare (ignore all))
  (values cl-ds.meta:null-bucket (cl-ds.common:make-eager-modification-operation-status t bucket t)))


(defmethod cl-ds.meta:alter-bucket! ((operation cl-ds.meta:erase-if!-function) container value bucket
                                     &rest all &key condition-fn)
  (declare (ignore all))
  (if (funcall condition-fn bucket)
      (values cl-ds.meta:null-bucket (cl-ds.common:make-eager-modification-operation-status t bucket t))
      (values bucket (cl-ds.common:make-eager-modification-operation-status t bucket nil))))


(defmethod cl-ds.meta:alter-bucket! ((operation cl-ds.meta:insert!-function) container value bucket &rest all)
  (declare (ignore all))
  (values (cl-ds:force value) (cl-ds.common:make-eager-modification-operation-status t bucket t)))


(defmethod cl-ds.meta:alter-bucket! ((operation cl-ds.meta:add!-function) container value bucket &rest all)
  (declare (ignore all))
  (values cl-ds.meta:null-bucket
          (cl-ds.common:make-eager-modification-operation-status t bucket nil)))


(defmethod cl-ds.meta:alter-bucket! ((operation cl-ds.meta:update-if!-function) container value bucket &rest all &key condition-fn)
  (declare (ignore all))
  (let ((effective-value (cl-ds:force value)))
    (if (funcall condition-fn bucket effective-value)
        (values effective-value (cl-ds.common:make-eager-modification-operation-status t bucket t))
        (values bucket (cl-ds.common:make-eager-modification-operation-status t bucket nil)))))


(defmethod cl-ds.meta:alter-bucket ((operation cl-ds.meta:update-function) container value bucket &rest all &key condition-fn)
  (declare (ignore all))
  (let ((effective-value (cl-ds:force value)))
    (if (funcall condition-fn bucket effective-value)
        (values effective-value (cl-ds.common:make-eager-modification-operation-status t bucket t))
        (values bucket (cl-ds.common:make-eager-modification-operation-status t bucket nil)))))


(defmethod cl-ds.meta:alter-bucket ((operation cl-ds.meta:update-function) container value bucket &rest all)
  (declare (ignore all))
  (values (cl-ds:force value)
          (cl-ds.common:make-eager-modification-operation-status t bucket t)))


(defmethod cl-ds.meta:alter-bucket ((operation cl-ds.meta:insert-function) container value bucket &rest all)
  (declare (ignore all))
  (values (cl-ds:force value)
          (cl-ds.common:make-eager-modification-operation-status t bucket t)))


(defmethod cl-ds.meta:alter-bucket ((operation cl-ds.meta:add-function) container value bucket &rest all)
  (declare (ignore all))
  (values cl-ds.meta:null-bucket
          (cl-ds.common:make-eager-modification-operation-status t bucket nil)))


(defmethod cl-ds.meta:alter-bucket ((operation cl-ds.meta:erase-function) container value bucket &rest all)
  (declare (ignore all))
  (values cl-ds.meta:null-bucket
          (cl-ds.common:make-eager-modification-operation-status t bucket t)))


(defmethod cl-ds.meta:alter-bucket ((operation cl-ds.meta:take-out-function) container value bucket &rest all)
  (declare (ignore all))
  (values cl-ds.meta:null-bucket
          (cl-ds.common:make-eager-modification-operation-status t bucket t)))


(defmethod cl-ds.meta:alter-bucket! ((operation cl-ds.meta:take-out!-function) container value bucket &rest all)
  (declare (ignore all))
  (values cl-ds.meta:null-bucket
          (cl-ds.common:make-eager-modification-operation-status t bucket t)))


(defmethod cl-ds.meta:alter-bucket ((operation cl-ds.meta:erase-if-function) container value bucket
                                    &rest all &key condition-fn)
  (declare (ignore all))
  (if (funcall condition-fn bucket)
      (values cl-ds.meta:null-bucket (cl-ds.common:make-eager-modification-operation-status t bucket t))
      (values bucket (cl-ds.common:make-eager-modification-operation-status t bucket nil))))


(defmethod cl-ds.meta:fresh-bucket-status ((operation cl-ds.meta:update-function)
                                           value)
  cl-ds.common:empty-eager-modification-operation-status)


(defmethod cl-ds.meta:fresh-bucket-status ((operation cl-ds.meta:update-if-function)
                                           value)
  cl-ds.common:empty-eager-modification-operation-status)


(defmethod cl-ds.meta:fresh-bucket-status ((operation cl-ds.meta:update!-function)
                                           value)
  cl-ds.common:empty-eager-modification-operation-status)


(defmethod cl-ds.meta:fresh-bucket-status ((operation cl-ds.meta:update-if!-function)
                                           value)
  cl-ds.common:empty-eager-modification-operation-status)


(defmethod cl-ds.meta:fresh-bucket-status (operation value)
  (cl-ds.common:make-eager-modification-operation-status
   nil nil t))


(defmethod cl-ds.meta:make-bucket ((operation cl-ds.meta:grow-function)
                                   container
                                   value
                                   &rest all)
  (declare (ignore all))
  (let ((status (cl-ds.meta:fresh-bucket-status operation value)))
    (values (if (cl-ds:changed status)
                (cl-ds:force value)
                cl-ds.meta:null-bucket)
            status)))


(defmethod cl-ds.meta:make-bucket ((operation cl-ds.meta:shrink-function)
                                   container
                                   value
                                   &rest all)
  (declare (ignore all))
  (values cl-ds.meta:null-bucket
          cl-ds.common:empty-eager-modification-operation-status))
