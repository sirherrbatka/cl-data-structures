(in-package #:cl-data-structures.algorithms.meta)


(defgeneric apply-layer (range function &rest all &key &allow-other-keys))


(defclass range-function (closer-mop:standard-generic-function)
  ())


(defclass layer-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass transformation!-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass aggregation-function (range-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass multi-aggregation-function (aggregation-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))


(defclass fundamental-aggregator ()
  ())


(defclass linear-aggregator ()
  ((%stages :initarg :stages
            :reader read-stages)
   (%arguments :initarg :arguments
               :initform nil
               :accessor access-arguments)))


(defgeneric pass-to-aggregation (aggregator element))


(defgeneric construct-aggregator (range stages outer-fn arguments))


(defgeneric begin-aggregation (aggregator))


(defgeneric end-aggregation (aggregator))


(defgeneric extract-result (aggregator))


(defgeneric multi-aggregation-stages (aggregation-function
                                      &rest all &key &allow-other-keys)
  (:method ((function aggregation-function) &rest all &key &allow-other-keys)
    (declare (ignore all))
    nil))


(defgeneric make-state (aggregation-function
                        &rest all
                        &key &allow-other-keys))


(defgeneric aggregation-finished-p (aggregation-function state)
  (:method ((function aggregation-function) state)
    nil))


(defgeneric aggregate (function state element))


(defgeneric state-result (function state)
  (:method ((function aggregation-function) state)
    state))


(defmacro gather-prior-states (fn range into)
  (with-gensyms (!result !name !stage)
    (once-only (fn range)
      `(iterate
         (for (,!name . ,!stage) in (multi-aggregation-stages ,fn ,into))
         (for ,!result = (funcall ,!stage ,range))
         (push ,!result ,into)
         (push ,!name ,into)))))


(defgeneric apply-range-function (range function
                                  &rest all
                                  &key &allow-other-keys))


(defgeneric apply-aggregation-function (range function
                                        &rest all
                                        &key key
                                        &allow-other-keys))


(defmethod apply-range-function ((range cl-ds:fundamental-range)
                                 (function layer-function)
                                 &rest all &key &allow-other-keys)
  (let ((clone (cl-ds:clone range)))
    (apply #'apply-layer clone function all)))


(defclass states-collector (cl-ds:traversable)
  ((%args :initform nil
          :initarg :args
          :accessor access-args)
   (%label :initform nil
           :accessor access-label)
   (%original :initarg :original
              :reader read-original)))


(defmethod apply-aggregation-function ((range states-collector)
                                       (function aggregation-function)
                                       &rest all &key key &allow-other-keys)
  (let ((state (apply #'make-state function all)))
    (unless (aggregation-finished-p function state)
      (block end
        (cl-ds:across (lambda (x)
                        (when (aggregation-finished-p function state)
                          (return-from end))
                        (aggregate function
                                   state
                                   (if key (funcall key x) x)))
                      (read-original range))))
    (push (state-result function state) (access-args range))
    (push (access-label range) (access-args range))))


(defmethod apply-aggregation-function (range
                                       (function multi-aggregation-function)
                                       &rest all &key key &allow-other-keys)
  (declare (ignore key))
  (let ((stages (apply #'multi-aggregation-stages function all)))
    (unless (endp stages)
      (iterate
        (with collector = (make 'states-collector :args all :original range))
        (for (name . stage) in stages)
        (setf (access-label collector) name)
        (apply stage collector (access-args collector))
        (finally (return (apply #'call-next-method
                                range
                                function
                                (access-args collector))))))))


(defmethod apply-aggregation-function (range
                                       (function aggregation-function)
                                       &rest all &key key &allow-other-keys)
  (let ((state (apply #'make-state function all)))
    (unless (aggregation-finished-p function state)
      (block end
        (cl-ds:across (lambda (x)
                        (when (aggregation-finished-p function state)
                          (return-from end))
                        (aggregate function
                                   state
                                   (if key (funcall key x) x)))
                      range)))
    (state-result function state)))


(defmethod extract-result ((aggregator linear-aggregator))
  (bind (((:slots %stages) aggregator)
         ((name function . state) (first %stages)))
    (state-result function state)))


(defmethod apply-aggregation-function ((range linear-aggregator)
                                       (function aggregation-function)
                                       &rest all
                                       &key
                                       &allow-other-keys)
  (bind (((:slots %stages) range)
         (state (apply #'make-state function all)))
    (push (list* (caar %stages) (list* function state)) (rest %stages))))


(defmethod pass-to-aggregation ((aggregator linear-aggregator)
                                element)
  (bind (((:slots %stages) aggregator)
         ((function . state) (cdr (first %stages))))
    (aggregate function state element)))


(defmethod end-aggregation ((aggregator linear-aggregator))
  (bind (((:slots %stages %arguments) aggregator)
         ((name . (function . state)) (first %stages))
         (stage-result (state-result function state))
         (rest (rest %stages)))
    (push stage-result %arguments)
    (push name %arguments)
    (setf %stages rest)
    (unless (endp (rest %stages))
      (apply (cdar %stages)
             aggregator
             %arguments))
    stage-result))


(defun make-linear-aggregator (arguments stages)
  (make 'linear-aggregator
        :stages stages
        :arguments arguments))


(defmethod construct-aggregator ((range cl:sequence)
                                 (stages list)
                                 (outer-fn (eql nil))
                                 (arguments list))
  (make-linear-aggregator arguments stages))


(defmethod construct-aggregator ((range cl:hash-table)
                                 (stages list)
                                 (outer-fn (eql nil))
                                 (arguments list))
  (make-linear-aggregator arguments stages))


(defmethod construct-aggregator ((range fundamental-forward-range)
                                 (stages list)
                                 (outer-fn (eql nil))
                                 (arguments list))
  (make-linear-aggregator arguments stages))


(defmethod construct-aggregator ((range fundamental-forward-range)
                                 (stages list)
                                 outer-fn
                                 (arguments list))
  (funcall outer-fn stages arguments))


(defmethod begin-aggregation ((aggregator linear-aggregator))
  (bind (((:slots %stages %arguments) aggregator)
         ((name . construct-function) (first %stages))
         (rest (rest %stages)))
    (push (list* name (apply construct-function %arguments))
          rest)
    (setf %stages rest)))
