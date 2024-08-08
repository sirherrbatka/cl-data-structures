(cl:in-package #:cl-data-structures.file-system)


(defclass command ()
  ((%command-string :type string
                    :initarg :command-string
                    :reader read-command-string)))


(defmethod print-object ((object command) stream)
  (print-unreadable-object (object stream)
    (format stream "~a" (read-command-string object))))


(defmethod stream-designator-p ((designator command))
  t)


(defun command (format-control-string &rest format-arguments)
  (declare (dynamic-extent format-arguments))
  (check-type format-control-string string)
  (make 'command
        :command-string (apply #'format nil
                               format-control-string format-arguments)))


(defmethod open-stream-designator ((designator command) &key (direction :input) (element-type 'character) (external-format uiop/stream:*utf-8-external-format*))
  (eswitch (direction)
    (:input
     (~> designator read-command-string
         (uiop:launch-program :output :stream
                              :force-shell nil
                              :element-type element-type
                              :external-format external-format)
         uiop/launch-program:process-info-output))
    (:output
     (~> designator read-command-string
         (uiop:launch-program :input :stream
                              :force-shell nil
                              :element-type element-type
                              :external-format external-format)
         uiop/launch-program:process-info-input))
    (:io
     (bind (((:accessors uiop/launch-program:process-info-output uiop/launch-program:process-info-input)
             (~> designator read-command-string
                 (uiop:launch-program :input :stream :output :stream :force-shell nil
                                      :element-type element-type
                                      :external-format external-format))))
       (values (make-two-way-stream uiop/launch-program:process-info-output
                                    uiop/launch-program:process-info-input)
               uiop/launch-program:process-info-output
               uiop/launch-program:process-info-input)))))
