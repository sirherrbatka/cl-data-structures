(cl:in-package #:cl-ds.composite)


(defclass composite-container ()
  ((%root :initarg :root
          :accessor root)
   (%make-bucket-callbacks :initarg :make-bucket-callbacks
                           :reader make-bucket-callbacks)))


(defclass mutable-composite-container (composite-container)
  ())


(defclass functional-composite-container (composite-container)
  ())


(defun make-mutable-composite-container (root &rest make-bucket-callbacks)
  (make 'mutable-composite-container
        :root root
        :make-bucket-callbacks make-bucket-callbacks))


(defun make-functional-composite-container (root &rest make-bucket-callbacks)
  (make 'functional-composite-container
        :root root
        :make-bucket-callbacks make-bucket-callbacks))


(defun separate-operations/arguments (operations)
  (iterate
    (for op in operations)
    (if (listp op)
        (progn
          (collecting (first op) into just-operations)
          (collecting (rest op) into just-arguments))
        (collecting op into just-operations))
    (finally (return (values just-operations just-arguments)))))


(defun position-modification (operations container locations value &rest all)
  (bind (((:values just-operations just-arguments)
          (separate-operations/arguments operations))
         ((:values new-root status)
          (apply #'cl-ds.meta:position-modification (first just-operations)
                 container
                 (root container)
                 (first locations)
                 :value value
                 :composite-container-locations (rest locations)
                 :composite-container-make-bucket-callbacks (make-bucket-callbacks container)
                 :composite-container-operations (rest just-operations)
                 :composite-container-rest-arguments (rest just-arguments)
                 :composite-container-arguments (first just-arguments)
                 all)))
    (if (cl-ds:changed status)
        (values (make (class-of container)
                      :root new-root
                      :make-bucket-callbacks (make-bucket-callbacks container))
                status)
        (values container status))))


(defun position-modification! (operations container locations value &rest all)
  (bind (((:values just-operations just-arguments)
          (separate-operations/arguments operations))
         ((:values new-root status)
          (apply #'cl-ds.meta:position-modification
                 (first just-operations)
                 container
                 (root container)
                 (first locations)
                 :value value
                 :composite-container-locations (rest locations)
                 :composite-container-make-bucket-callbacks (make-bucket-callbacks container)
                 :composite-container-operations (rest just-operations)
                 :composite-container-rest-arguments (rest just-arguments)
                 :composite-container-arguments (first just-arguments)
                 all)))
    (setf (root container) new-root)
    (values container status)))


(defmethod cl-ds.meta:alter-bucket ((container functional-composite-container)
                                    operation
                                    value bucket
                                    &rest all
                                    &key
                                      composite-container-operations
                                      composite-container-make-bucket-callbacks
                                      composite-container-rest-arguments
                                      composite-container-arguments
                                      composite-container-locations)
  (if (~> composite-container-make-bucket-callbacks rest endp)
      (apply #'cl-ds.meta:position-modification
               (first composite-container-operations)
               bucket
               bucket
               (first composite-container-locations)
               :value value
               composite-container-arguments)
      (apply #'cl-ds.meta:alter-bucket
               container
               operation
               value
               bucket
               :composite-container-locations (rest composite-container-locations)
               :composite-container-make-bucket-callbacks (rest composite-container-make-bucket-callbacks)
               :composite-container-operations (rest composite-container-operations)
               :composite-container-rest-arguments (rest composite-container-rest-arguments)
               all)))


(defmethod cl-ds.meta:alter-bucket! ((container mutable-composite-container)
                                     operation
                                     value bucket
                                     &rest all
                                     &key
                                       composite-container-operations
                                       composite-container-make-bucket-callbacks
                                       composite-container-rest-arguments
                                       composite-container-arguments
                                       composite-container-locations)
  (if (~> composite-container-make-bucket-callbacks rest endp)
      (apply #'cl-ds.meta:position-modification
               (first composite-container-operations)
               bucket
               bucket
               (first composite-container-locations)
               :value value
               composite-container-arguments)
      (apply #'cl-ds.meta:alter-bucket!
               container
               operation
               value
               bucket
               :composite-container-locations (rest composite-container-locations)
               :composite-container-make-bucket-callbacks (rest composite-container-make-bucket-callbacks)
               :composite-container-operations (rest composite-container-operations)
               :composite-container-rest-arguments (rest composite-container-rest-arguments)
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


(defmethod cl-ds.meta:make-bucket ((container composite-container)
                                   operation
                                   value
                                   &rest all
                                   &key
                                     composite-container-operations
                                     composite-container-make-bucket-callbacks
                                     composite-container-rest-arguments
                                     composite-container-arguments
                                     composite-container-locations)
  (declare (ignore all composite-container-rest-arguments))
  (iterate
    (with v = value)
    (for operation in (reverse composite-container-operations))
    (for make-bucket-callback in (reverse composite-container-make-bucket-callbacks))
    (for location in (reverse composite-container-locations))
    (for (values bucket status) = (funcall make-bucket-callback
                                           operation location
                                           v composite-container-arguments))
    (unless (cl-ds:changed status)
      (return (values cl-ds.meta:null-bucket status)))
    (setf v bucket)
    (finally
     (return (values v status)))))


(defmethod cl-ds:at ((container composite-container)
                     location &rest more-locations)
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
