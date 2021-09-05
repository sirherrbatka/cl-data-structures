(cl:in-package #:cl-ds.composite)


(defclass composite-container ()
  ((%root :initarg :root
          :reader root)
   (%make-bucket-callbacks :initarg :make-bucket-callbacks
                           :reader make-bucket-callbacks)))


(defclass mutable-composite-container (composite-container)
  ())


(defclass functional-composite-container (composite-container)
  ())


(defun position-modification (operations container locations value &rest all)
  (apply #'cl-ds.meta:position-modification (first operations)
           (root container)
           container
           (first locations)
           :value value
           :composite-container-locations (rest locations)
           :composite-container-make-bucket-callbacks (make-bucket-callbacks container)
           :composite-container-operations (rest operations)
           all))


(defmethod cl-ds.meta:alter-bucket (operation
                                    (container functional-composite-container)
                                    value bucket
                                    &rest all
                                    &key
                                      composite-container-operations
                                      composite-container-make-bucket-callbacks
                                      composite-container-locations)
  (if (~> composite-container-make-bucket-callbacks rest endp)
      (apply #'cl-ds.meta:position-modification
               (first composite-container-operations)
               bucket
               bucket
               (first composite-container-locations)
               :composite-container-locations (rest composite-container-locations)
               :composite-container-make-bucket-callbacks (rest composite-container-make-bucket-callbacks)
               :composite-container-operations (rest composite-container-operations)
               all)
      (apply #'cl-ds.meta:alter-bucket
               operation
               container
               value
               bucket
               :composite-container-locations (rest composite-container-locations)
               :composite-container-make-bucket-callbacks (rest composite-container-make-bucket-callbacks)
               :composite-container-operations (rest composite-container-operations)
               all)))


(defmethod cl-ds.meta:alter-bucket! (operation
                                     (container mutable-composite-container)
                                     value bucket
                                     &rest all
                                     &key
                                       composite-container-operations
                                       composite-container-make-bucket-callbacks
                                       composite-container-locations)
  (if (~> composite-container-make-bucket-callbacks rest endp)
      (apply #'cl-ds.meta:position-modification
               (first composite-container-operations)
               bucket
               bucket
               (first composite-container-locations)
               :composite-container-locations (rest composite-container-locations)
               :composite-container-make-bucket-callbacks (rest composite-container-make-bucket-callbacks)
               :composite-container-operations (rest composite-container-operations)
               all)
      (apply #'cl-ds.meta:alter-bucket!
               operation
               container
               value
               bucket
               :composite-container-locations (rest composite-container-locations)
               :composite-container-make-bucket-callbacks (rest composite-container-make-bucket-callbacks)
               :composite-container-operations (rest composite-container-operations)
               all)))


(defmacro make-bucket-callback (make-container
                                &key
                                  (location (gensym))
                                  (value (gensym))
                                  (operation (gensym))
                                  (all (gensym)))
  (with-gensyms (!fresh-status)
    `(lambda (,operation ,location ,value ,all)
       (declare (ignorable ,all))
       (let ((,!fresh-status (cl-ds.meta:fresh-bucket-status ,operation ,value)))
         (if (cl-ds:changed ,!fresh-status)
             (funcall ,operation ,make-container ,location ,value)
             (values cl-ds.meta:null-bucket ,!fresh-status))))))


(defmethod cl-ds.meta:make-bucket (operation
                                   (container composite-container)
                                   value
                                   &rest all
                                   &key
                                     composite-container-operations
                                     composite-container-make-bucket-callbacks
                                     composite-container-locations)
  (declare (optimize (debug 3) (speed 0)))
  (iterate
    (with v = value)
    (for operation in (reverse composite-container-operations))
    (for make-bucket-callback in (reverse composite-container-make-bucket-callbacks))
    (for location in (reverse composite-container-locations))
    (for (values bucket status) = (funcall make-bucket-callback
                                           operation location
                                           v all))
    (unless (cl-ds:changed status)
      (return (values cl-ds.meta:null-bucket status)))
    (setf v bucket)
    (finally
     (return (values v status)))))


(defmethod cl-ds:at ((container composite-container)
                     location &rest more-locations)
  (break)
  (unless (= (length more-locations)
             (~> container
                 make-bucket-callbacks
                 length))
    (error 'cl-ds:dimensionality-error
            :arguments '(location more-locations)
            :bounds `(= ,(~> container make-bucket-callbacks length))
            :value (length more-locations)))
  (iterate
    (with sub-container = (root container))
    (for l in (cons location more-locations))
    (for (values next found) = (cl-ds:at sub-container l))
    (when (not found)
      (return (values nil found)))
    (setf sub-container next)
    (finally (return (values sub-container found)))))
