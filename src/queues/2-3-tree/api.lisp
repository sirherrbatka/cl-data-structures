(cl:in-package #:cl-data-structures.queues.2-3-tree)


(eval-always
  (define-constant +buffer-size+ 32))


(deftype queue-buffer ()
  `(simple-vector ,+buffer-size+))


(deftype buffer-index ()
  `(integer 0 ,+buffer-size+))


(defclass synchronization-primitives-mixin ()
  ((%lock :initform (bt:make-lock)
          :reader read-lock)
   (%notify-pop :initform (bt:make-condition-variable)
                :reader read-notify-pop)))


(defclass synchronization-mixin (synchronization-primitives-mixin)
  ())


(defclass fixed-capacity-synchronization-mixin (synchronization-primitives-mixin)
  ((%capacity :initarg :capacity
              :reader read-capacity)
   (%notify-push :initform (bt:make-condition-variable)
                 :reader read-notify-push)))


(defclass 2-3-queue (cl-ds.common.2-3:tree)
  ((%element-type :initarg :element-type
                  :initform t
                  :reader cl-ds:type-specialization
                  :reader read-element-type)
   (%head :accessor access-head
          :initform nil
          :initarg :head)
   (%head-position :accessor access-head-position
                   :initarg :head-position
                   :type buffer-index
                   :initform 0)
   (%tail :accessor access-tail
          :initform nil
          :initarg :tail)
   (%tail-position :accessor access-tail-position
                   :initarg :tail-position
                   :type buffer-index
                   :initform 0)
   (%tail-end :accessor access-tail-end
              :initarg :tail-end
              :type buffer-index
              :initform 0)))


(defclass mutable-2-3-queue
    (2-3-queue cl-ds.queues:fundamental-mutable-queue)
  ())


(defclass 2-3-queue-range (cl-ds:fundamental-forward-range)
  ((%mutex :initform (bt:make-lock)
           :reader read-mutex)
   (%og-container :initarg :og-container
                  :accessor access-og-container)
   (%container :initarg :container
               :accessor access-container)))


(defclass functional-2-3-queue
    (2-3-queue cl-ds.queues:fundamental-functional-queue)
  ())


(defclass transactional-2-3-queue
    (2-3-queue
     cl-ds.common.abstract:fundamental-ownership-tagged-object
     cl-ds.queues:fundamental-transactional-queue)
  ())


(defclass synchronized-mutable-2-3-queue
    (synchronization-mixin mutable-2-3-queue)
  ())


(defclass fixed-capacity-synchronized-mutable-2-3-queue
    (fixed-capacity-synchronization-mixin mutable-2-3-queue)
  ())


(defclass synchronized-transactional-2-3-queue
    (synchronization-mixin transactional-2-3-queue)
  ())


(defclass fixed-capacity-synchronized-transactional-2-3-queue
    (fixed-capacity-synchronization-mixin transactional-2-3-queue)
  ())


(defmethod cl-ds:become-functional ((container mutable-2-3-queue))
  (make 'functional-2-3-queue
        :root (cl-ds.common.2-3:access-root container)
        :head (access-head container)
        :element-type (read-element-type container)
        :size (cl-ds:size container)
        :head-position (access-head-position container)
        :tail (access-tail container)
        :tail-position (access-tail-position container)
        :tail-end (access-tail-end container)))


(defun across/traverse (container function)
  (ensure-functionf function)
  (labels ((visit (node)
             (unless (cl-ds.meta:null-bucket-p node)
               (etypecase node
                 (cl-data-structures.common.2-3-tree:3-node
                  (visit (cl-data-structures.common.2-3-tree:access-right node))
                  (visit (cl-data-structures.common.2-3-tree:access-middle node))
                  (visit (cl-data-structures.common.2-3-tree:access-left node)))
                 (cl-data-structures.common.2-3-tree::2-node
                  (visit (cl-data-structures.common.2-3-tree:access-right node))
                  (visit (cl-data-structures.common.2-3-tree:access-left node)))
                 (vector
                  (map nil function node))))))
    (visit (cl-ds.common.2-3:access-root container))
    (let ((head (access-head container))
          (tail (access-tail container))
          (tail-position (access-tail-position container))
          (head-position (access-head-position container))
          (tail-end (access-tail-end container)))
      (iterate
        (for i from tail-position below tail-end)
        (funcall function (aref tail i)))
      (iterate
        (for i from 0 below head-position)
        (funcall function (aref head i))))
    container))


(defmethod cl-ds:across ((container 2-3-queue) function)
  (across/traverse container function))


(defmethod cl-ds:traverse ((container 2-3-queue) function)
  (across/traverse container function))


(defmethod cl-ds:empty-clone ((container 2-3-queue))
  (make (type-of container)
        :empty-type (read-element-type container)))


(defun copy-array-if-not-nil (array)
  (unless (null array)
    (copy-array array)))


(defmethod cl-ds:empty-clone ((container transactional-2-3-queue))
  (make (type-of container)
        :ownership-tag (cl-ds.common.abstract:make-ownership-tag)
        :empty-type (read-element-type container)))


(defmethod cl-ds:empty-clone ((container fixed-capacity-synchronization-mixin))
  (lret ((result (call-next-method)))
    (setf (slot-value result '%capacity)
          (read-capacity container))))


(defmethod cl-ds:replica ((container transactional-2-3-queue)
                          &optional isolate)
  (cl-ds.common.abstract:replica container isolate))


(defmethod cl-ds:become-transactional ((container 2-3-queue))
  (make 'transactional-2-3-queue
        :root (cl-ds.common.2-3:access-root container)
        :ownership-tag (cl-ds.common.abstract:make-ownership-tag)
        :head (copy-array-if-not-nil (access-head container))
        :element-type (read-element-type container)
        :size (cl-ds:size container)
        :head-position (access-head-position container)
        :tail (copy-array-if-not-nil (access-tail container))
        :tail-position (access-tail-position container)
        :tail-end (access-tail-end container)))


(defmethod cl-ds:become-mutable ((container functional-2-3-queue))
  (make 'mutable-2-3-queue
        :root (cl-ds.common.2-3:access-root container)
        :size (cl-ds:size container)
        :head (access-head container)
        :element-type (read-element-type container)
        :head-position (access-head-position container)
        :tail (access-tail container)
        :tail-position (access-tail-position container)
        :tail-end (access-tail-end container)))


(defmethod cl-ds:become-mutable ((container transactional-2-3-queue))
  (make 'mutable-2-3-queue
        :root (cl-ds.common.2-3:access-root container)
        :size (cl-ds:size container)
        :head (access-head container)
        :element-type (read-element-type container)
        :head-position (access-head-position container)
        :tail (access-tail container)
        :tail-position (access-tail-position container)
        :tail-end (access-tail-end container)))


(defun make-mutable-2-3-queue (&key (element-type t))
  (make 'mutable-2-3-queue :element-type element-type))


(defun make-functional-2-3-queue (&key (element-type t))
  (make 'functional-2-3-queue :element-type element-type))


(defun make-transactional-2-3-queue (&key (element-type t))
  (make 'transactional-2-3-queue
        :element-type element-type))


(defun make-synchronized-transactional-2-3-queue (&key (element-type t) capacity)
  (check-type capacity (or null positive-integer))
  (if (null capacity)
      (make 'synchronized-transactional-2-3-queue
            :element-type element-type)
      (make 'fixed-capacity-synchronized-transactional-2-3-queue
            :element-type element-type
            :capacity capacity)))


(defun make-synchronized-mutable-2-3-queue (&key (element-type t) capacity)
  (check-type capacity (or null positive-integer))
  (if (null capacity)
      (make 'synchronized-mutable-2-3-queue
            :element-type element-type)
      (make 'fixed-capacity-synchronized-mutable-2-3-queue
            :element-type element-type
            :capacity capacity)))


(defmethod cl-ds.meta:position-modification
    ((operation cl-ds.meta:put-function)
     container
     (structure functional-2-3-queue)
     location
     &rest all)
  (let ((head-position (access-head-position structure))
        (size (cl-ds.queues:access-size structure))
        (head (access-head structure))
        (element-type (read-element-type structure)))
    (declare (type buffer-index head-position)
             (type (or null queue-buffer) head))
    (cond ((eql head-position +buffer-size+)
           (bind ((new-root (cl-ds.common.2-3:insert-front-into-tree
                             (cl-ds.common.2-3:access-root structure)
                             (lambda () head)))
                  (new-head (make-array
                             +buffer-size+
                             :element-type element-type))
                  (head-position 1)
                  (size (1+ size))
                  ((:values bucket status) (apply #'cl-ds.meta:make-bucket
                                                  container
                                                  operation
                                                  location
                                                  all)))
             (if (cl-ds:changed status)
                 (progn
                   (setf (aref new-head 0) bucket)
                   (values (make (type-of structure)
                                 :head new-head
                                 :size size
                                 :element-type element-type
                                 :head-position head-position
                                 :tail (access-tail structure)
                                 :tail-position (access-tail-position structure)
                                 :root new-root
                                 :tail-end (access-tail-end structure))
                           cl-ds.common:empty-eager-modification-operation-status))
                 structure)))
          (t (bind ((head (or (copy-array-if-not-nil head)
                              (make-array +buffer-size+
                                          :element-type element-type)))
                    (size (1+ size))
                    ((:values bucket status) (apply #'cl-ds.meta:make-bucket
                                                    container
                                                    operation
                                                    location
                                                    all)))
               (if (cl-ds:changed status)
                   (progn
                     (setf (aref head head-position) bucket)
                     (values (make (type-of structure)
                                   :head head
                                   :element-type element-type
                                   :size size
                                   :head-position (1+ head-position)
                                   :tail (access-tail structure)
                                   :tail-position (access-tail-position structure)
                                   :root (cl-ds.common.2-3:access-root structure)
                                   :tail-end (access-tail-end structure))
                             cl-ds.common:empty-eager-modification-operation-status))
                   (values structure status)))))))


(defmethod cl-ds.meta:position-modification
    ((operation cl-ds.meta:put!-function)
     container
     (structure mutable-2-3-queue)
     location
     &rest all)
  (let ((head-position (access-head-position structure))
        (size (cl-ds.queues:access-size structure))
        (head (ensure (access-head structure)
                #1=(make-array +buffer-size+
                               :element-type (read-element-type structure)))))
    (declare (type buffer-index head-position)
             (type queue-buffer head))
    (cond ((eql head-position +buffer-size+)
           (cl-ds.common.2-3:insert-front-into-tree! structure
                                                     (lambda () head))
           (setf (access-head-position structure) 1
                 head #1#
                 (access-head structure) head
                 (aref head 0) (apply #'cl-ds.meta:make-bucket
                                      container
                                      operation
                                      location
                                      all)
                 (cl-ds.queues:access-size structure) (1+ size))
           (values structure
                   cl-ds.common:empty-eager-modification-operation-status))
          (t (setf (aref head head-position) (apply #'cl-ds.meta:make-bucket
                                                    container
                                                    operation
                                                    location
                                                    all)
                   (access-head-position structure) (1+ head-position)
                   (cl-ds.queues:access-size structure) (1+ size))
             (values
              structure
              cl-ds.common:empty-eager-modification-operation-status)))))


(defmethod cl-ds.meta:position-modification
    ((operation cl-ds.meta:put!-function)
     container
     (structure transactional-2-3-queue)
     location
     &rest all)
  (let ((head-position (access-head-position structure))
        (size (cl-ds.queues:access-size structure))
        (head (ensure (access-head structure)
                #1=(make-array +buffer-size+
                               :element-type (read-element-type structure)))))
    (declare (type buffer-index head-position)
             (type queue-buffer head))
    (cond ((eql head-position +buffer-size+)
           (cl-ds.common.2-3:transactional-insert-front-into-tree!
            structure
            (lambda () head))
           (setf (access-head-position structure) 1
                 head #1#
                 (access-head structure) head
                 (aref head 0) (apply #'cl-ds.meta:make-bucket
                                      container
                                      operation
                                      location
                                      all)
                 (cl-ds.queues:access-size structure) (1+ size))
           (values structure
                   cl-ds.common:empty-eager-modification-operation-status))
          (t (setf (aref head head-position) (apply #'cl-ds.meta:make-bucket
                                                    container
                                                    operation
                                                    location
                                                    all)
                   (access-head-position structure) (1+ head-position)
                   (cl-ds.queues:access-size structure) (1+ size))
             (values
              structure
              cl-ds.common:empty-eager-modification-operation-status)))))


(defmethod cl-ds.meta:position-modification
    ((operation cl-ds.meta:take-out-function)
     container
     (structure functional-2-3-queue)
     location
     &rest all)
  (let ((tail-position (access-tail-position structure))
        (root (cl-ds.common.2-3:access-root structure))
        (size (cl-ds:size structure))
        (tail (access-tail structure))
        (tail-end (access-tail-end structure)))
    (declare (type buffer-index tail-position)
             (type (or null queue-buffer) tail))
    (if (< tail-position tail-end)
        (bind ((bucket (aref tail tail-position))
               (tail-position (access-tail-position structure))
               ((:values new-buffer status)
                (apply #'cl-ds.meta:alter-bucket
                       container operation
                       location bucket all)))
          (if (cl-ds.meta:null-bucket-p new-buffer)
              (progn
                (decf size)
                (incf tail-position))
              (setf tail (copy-array tail)
                    (aref tail tail-position) new-buffer))
          (values (make (type-of structure)
                        :root (cl-ds.common.2-3:access-root structure)
                        :head (access-head structure)
                        :element-type (read-element-type structure)
                        :head-position (access-head-position structure)
                        :tail tail
                        :size size
                        :tail-position tail-position
                        :tail-end (access-tail-end structure))
                  status))
        (if (cl-ds.meta:null-bucket-p root)
            (if (~> structure cl-ds:size zerop)
                (error 'cl-ds:operation-not-allowed
                       :format-control "Can't reduce size of the empty queue!")
                (bind ((head (access-head structure))
                       (bucket (aref head 0))
                       (head-position (access-head-position structure))
                       ((:values shrinked-bucket status)
                        (apply #'cl-ds.meta:alter-bucket
                               container operation
                               location bucket
                               all))
                       (tail-position 0)
                       (tail head))
                  (if (cl-ds.meta:null-bucket-p shrinked-bucket)
                      (progn
                        (decf size)
                        (incf tail-position))
                      (setf tail (copy-array tail)
                            (aref tail 0) shrinked-bucket))
                  (values (make (type-of structure)
                                :head nil
                                :element-type (read-element-type structure)
                                :head-position 0
                                :tail tail
                                :size size
                                :tail-position tail-position
                                :tail-end head-position)
                          status)))
            (bind (((:values root buffer)
                    (cl-ds.common.2-3:delete-back-from-tree
                     (cl-ds.common.2-3:access-root structure)))
                   ((:values new-bucket status)
                    (apply #'cl-ds.meta:alter-bucket
                           container operation
                           location (aref buffer 0)
                           all))
                   (tail buffer)
                   (tail-end +buffer-size+)
                   (tail-position 0)
                   (size (cl-ds:size structure)))
              (if (cl-ds.meta:null-bucket-p new-bucket)
                  (progn
                    (decf size)
                    (incf tail-position))
                  (setf tail (copy-array tail)
                        (aref tail 0) new-bucket))
              (values (make (type-of structure)
                            :root root
                            :head (access-head structure)
                            :element-type (read-element-type structure)
                            :head-position (access-head-position structure)
                            :tail tail
                            :size size
                            :tail-position tail-position
                            :tail-end tail-end)
                      status))))))


(defmethod cl-ds.meta:position-modification
    ((operation cl-ds.meta:take-out!-function)
     container
     (structure transactional-2-3-queue)
     location
     &rest all)
  (let ((tail-position (access-tail-position structure))
        (root (cl-ds.common.2-3:access-root structure))
        (tail (access-tail structure))
        (tail-end (access-tail-end structure)))
    (declare (type buffer-index tail-position)
             (type (or null queue-buffer) tail))
    (if (< tail-position tail-end)
        (bind ((bucket (aref tail tail-position))
               ((:values new-buffer status)
                (apply #'cl-ds.meta:alter-bucket
                       container operation
                       location bucket all)))
          (if (cl-ds.meta:null-bucket-p new-buffer)
              (progn
                (decf (cl-ds.queues:access-size structure))
                (incf (access-tail-position structure)))
              (setf (aref tail tail-position) new-buffer))
          (values structure status))
        (if (cl-ds.meta:null-bucket-p root)
            (if (~> structure cl-ds:size zerop)
                (error 'cl-ds:operation-not-allowed
                       :format-control "Can't reduce size of the empty queue!")
                (bind ((head (access-head structure))
                       (bucket (aref head 0))
                       (head-position (access-head-position structure))
                       ((:values shrinked-bucket status)
                        (apply #'cl-ds.meta:alter-bucket
                               container
                               operation
                               location
                               bucket
                               all)))
                  (setf (access-tail-end structure) head-position
                        (access-head-position structure) 0
                        (access-head structure) nil
                        (access-tail structure) head
                        (access-tail-position structure) 0)
                  (if (cl-ds.meta:null-bucket-p shrinked-bucket)
                      (progn
                        (decf (cl-ds.queues:access-size structure))
                        (incf (access-tail-position structure)))
                      (setf (aref head 0) shrinked-bucket))
                  (values structure status)))
            (bind (((:values _ buffer)
                    (cl-ds.common.2-3:transactional-delete-back-from-tree!
                     structure))
                   ((:values new-bucket status)
                    (apply #'cl-ds.meta:alter-bucket
                           container
                           operation
                           location
                           (aref buffer 0)
                           all)))
              (setf (access-tail-position structure) 0
                    (access-tail-end structure) +buffer-size+
                    (access-tail structure) buffer)
              (if (cl-ds.meta:null-bucket-p new-bucket)
                  (progn
                    (decf (cl-ds.queues:access-size structure))
                    (incf (access-tail-position structure)))
                  (setf (aref buffer 0) new-bucket))
              (values structure status))))))


(defmethod cl-ds.meta:position-modification
    ((operation cl-ds.meta:take-out!-function)
     container
     (structure mutable-2-3-queue)
     location
     &rest all)
  (let ((tail-position (access-tail-position structure))
        (root (cl-ds.common.2-3:access-root structure))
        (tail (access-tail structure))
        (tail-end (access-tail-end structure)))
    (declare (type buffer-index tail-position)
             (type (or null queue-buffer) tail))
    (if (< tail-position tail-end)
        (bind ((bucket (aref tail tail-position))
               ((:values new-buffer status)
                (apply #'cl-ds.meta:alter-bucket!
                       container operation
                       location bucket all)))
          (if (cl-ds.meta:null-bucket-p new-buffer)
              (progn
                (decf (cl-ds.queues:access-size structure))
                (incf (access-tail-position structure)))
              (setf (aref tail tail-position) new-buffer))
          (values structure status))
        (if (cl-ds.meta:null-bucket-p root)
            (if (~> structure cl-ds:size zerop)
                (error 'cl-ds:operation-not-allowed
                       :format-control "Can't reduce size of the empty queue!")
                (bind ((head (access-head structure))
                       (bucket (aref head 0))
                       (head-position (access-head-position structure))
                       ((:values shrinked-bucket status)
                        (apply #'cl-ds.meta:alter-bucket!
                               container
                               operation
                               location
                               bucket
                               all)))
                  (setf (access-tail-end structure) head-position
                        (access-head-position structure) 0
                        (access-head structure) nil
                        (access-tail structure) head
                        (access-tail-position structure) 0)
                  (if (cl-ds.meta:null-bucket-p shrinked-bucket)
                      (progn
                        (decf (cl-ds.queues:access-size structure))
                        (incf (access-tail-position structure)))
                      (setf (aref head 0) shrinked-bucket))
                  (values structure status)))
            (bind (((:values _ buffer)
                    (cl-ds.common.2-3:delete-back-from-tree! structure))
                   ((:values new-bucket status)
                    (apply #'cl-ds.meta:alter-bucket!
                           container
                           operation
                           location
                           (aref buffer 0)
                           all)))
              (setf (access-tail-position structure) 0
                    (access-tail-end structure) +buffer-size+
                    (access-tail structure) buffer)
              (if (cl-ds.meta:null-bucket-p new-bucket)
                  (progn
                    (decf (cl-ds.queues:access-size structure))
                    (incf (access-tail-position structure)))
                  (setf (aref buffer 0) new-bucket))
              (values structure status))))))


(defmethod cl-ds:at ((container 2-3-queue)
                     (position (eql :front))
                     &rest more)
  (cl-ds:assert-one-dimension more)
  (when (~> container cl-ds:size zerop)
    (return-from cl-ds:at (values nil nil)))
  (let ((tail (access-tail container))
        (tail-position (access-tail-position container))
        (tail-end (access-tail-end container)))
    (if (eql tail-position tail-end)
        (iterate
          (for node initially (cl-ds.common.2-3:access-root container)
               then (cl-ds.common.2-3:access-right node))
          (while (typep node 'cl-ds.common.2-3:node))
          (finally
           (return (values (aref (the queue-buffer node) 0)
                           t))))
        (values (aref tail tail-position)
                t))))


(defmethod cl-ds:at ((container 2-3-queue)
                     (position (eql :back))
                     &rest more)
  (cl-ds:assert-one-dimension more)
  (when (~> container cl-ds:size zerop)
    (return-from cl-ds:at (values nil nil)))
  (let ((head (access-head container))
        (head-position (access-head-position container)))
    (if (or (null head) (zerop head-position))
        (iterate
          (for node initially (cl-ds.common.2-3:access-root container)
               then (cl-ds.common.2-3:access-left node))
          (while (typep node 'cl-ds.common.2-3:node))
          (finally
           (return (values (aref (the queue-buffer node) #.(1- +buffer-size+))
                           t))))
        (values (aref head (1- head-position))
                t))))


(defmethod cl-ds:whole-range ((container 2-3-queue))
  (make '2-3-queue-range
        :container (cl-ds:become-transactional container)
        :og-container container))


(defmethod cl-ds:peek-front ((range 2-3-queue-range))
  (let ((container (access-container range)))
    (if (~> container cl-ds:size zerop)
        (values nil nil)
        (values (cl-ds:at container :front) t))))


(defmethod cl-ds:consume-front ((range 2-3-queue-range))
  (let ((container (access-container range)))
    (if (~> container cl-ds:size zerop)
        (values nil nil)
        (cl-ds:mod-bind (container found value) (cl-ds:take-out! container)
          (values value t)))))


(defmethod cl-ds:clone ((range 2-3-queue-range))
  (bt:with-lock-held ((read-mutex range))
    (let ((container (access-container range)))
      (setf (access-container container)
            (cl-ds:become-transactional container))
      (make '2-3-queue-range
            :container (cl-ds:become-transactional container)
            :og-container container))))


(defmethod cl-ds:reset! ((queue mutable-2-3-queue))
  (setf (access-head-position queue) 0
        (access-tail-end queue) 0
        (cl-ds.queues:access-size queue) 0
        (cl-ds.common.2-3:access-root queue) cl-ds.meta:null-bucket)
  queue)


(defmethod cl-ds:reset! ((queue transactional-2-3-queue))
  (setf (access-head-position queue) 0
        (access-tail-end queue) 0
        (cl-ds.queues:access-size queue) 0
        (cl-ds.common.2-3:access-root queue) cl-ds.meta:null-bucket)
  queue)


(defmethod cl-ds:reset! ((range 2-3-queue-range))
  (setf (access-container range)
        (~> range access-og-container cl-ds:become-transactional))
  range)


(defmethod cl-ds:across ((range 2-3-queue-range) function)
  (ensure-functionf function)
  (cl-ds:across (access-container range) function))


(defun mutable-from-traversable (traversable arguments)
  (lret ((result (apply #'make-mutable-2-3-queue arguments)))
    (cl-ds:across traversable
                  (lambda (x) (cl-ds:put! result x)))))


(defmethod cl-ds:make-from-traversable (traversable
                                        (class (eql 'mutable-2-3-queue))
                                        &rest arguments)
  (mutable-from-traversable traversable arguments))


(defmethod cl-ds:make-from-traversable (traversable
                                        (class (eql 'synchronized-mutable-2-3-queue))
                                        &rest arguments)
  (let ((result (mutable-from-traversable traversable arguments)))
    (change-class result 'synchronized-mutable-2-3-queue)))


(defmethod cl-ds:make-from-traversable (traversable
                                        (class (eql 'functional-2-3-queue))
                                        &rest arguments)
  (~> (mutable-from-traversable traversable arguments)
      cl-ds:become-functional))


(defmethod cl-ds:traverse ((container 2-3-queue) function)
  (ensure-functionf function)
  (labels ((visit-node (node)
             (typecase node
               (cl-ds.common.2-3:3-node
                (visit-node (cl-ds.common.2-3:access-right node))
                (visit-node (cl-ds.common.2-3:access-middle node))
                (visit-node (cl-ds.common.2-3:access-left node)))
               (cl-ds.common.2-3:2-node
                (visit-node (cl-ds.common.2-3:access-right node))
                (visit-node (cl-ds.common.2-3:access-left node)))
               (t
                (unless (cl-ds.meta:null-bucket-p node)
                  (map nil
                       (lambda (x)
                         (cl-ds.meta:map-bucket container x function))
                       node))))))
    (let ((tail (access-tail container))
          (tail-size (access-tail-position container)))
      (unless (null tail)
        (iterate
          (for i from 0 below tail-size)
          (cl-ds.meta:map-bucket container (aref tail i) function))))
    (visit-node (cl-ds.common.2-3:access-root container))
    (let ((head (access-head container))
          (head-size (access-head-position container)))
      (unless (null head)
        (iterate
          (for i from 0 below head-size)
          (cl-ds.meta:map-bucket container (aref head i) function))))
    (cl-ds.meta:map-bucket container (access-tail container) function)
    container))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:take-out!-function)
                                             container
                                             (structure synchronization-mixin)
                                             location
                                             &rest all)
  (declare (ignore all))
  (let ((lock (read-lock structure)))
    (bt:with-lock-held (lock)
      (iterate
        (while (zerop (slot-value structure 'cl-ds.queues::%size)))
        (~> structure read-notify-pop (bt:condition-wait lock)))
      (call-next-method))))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:put!-function)
                                             container
                                             (structure synchronization-mixin)
                                             location
                                             &rest all)
  (declare (ignore all))
  (let ((lock (read-lock structure)))
    (bt:with-lock-held (lock)
      (bind ((empty #1=(zerop (slot-value structure 'cl-ds.queues::%size)))
             ((:values v1 v2) (call-next-method)))
        (when (and empty (not #1#))
          (~> structure read-notify-pop bt:condition-notify))
        (values v1 v2)))))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:put!-function)
                                             container
                                             (structure fixed-capacity-synchronization-mixin)
                                             location
                                             &rest all)
  (declare (ignore all))
  (let ((lock (read-lock structure)))
    (bt:with-lock-held ((read-lock structure))
      (let ((size (slot-value structure 'cl-ds.queues::%size))
            (capacity (read-capacity structure)))
        (iterate
          (while (>= size capacity))
          (~> structure read-notify-push (bt:condition-wait lock)))
        (bind ((empty #1=(zerop (slot-value structure 'cl-ds.queues::%size)))
               ((:values v1 v2) (call-next-method)))
          (when (and empty (not #1#))
            (~> structure read-notify-pop bt:condition-notify))
          (values v1 v2))))))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:take-out!-function)
                                             container
                                             (structure fixed-capacity-synchronization-mixin)
                                             location
                                             &rest all)
  (declare (ignore all))
  (let ((lock (read-lock structure)))
    (bt:with-lock-held (lock)
      (iterate
        (while (zerop (slot-value structure 'cl-ds.queues::%size)))
        (~> structure read-notify-pop (bt:condition-wait lock)))
      (bind ((full #1=(>= (slot-value structure 'cl-ds.queues::%size)
                          (read-capacity structure)))
             ((:values v1 v2) (call-next-method)))
        (when (and full (not #1#))
          (~> structure read-notify-push bt:condition-notify))
        (values v1 v2)))))


(defmethod cl-ds:size ((container synchronization-primitives-mixin))
  (bt:with-lock-held ((read-lock container))
    (call-next-method)))


(defmethod cl-ds:across ((container synchronization-primitives-mixin) function)
  (bt:with-lock-held ((read-lock container))
    (call-next-method)))


(defmethod cl-ds:traverse ((container synchronization-primitives-mixin) function)
  (bt:with-lock-held ((read-lock container))
    (call-next-method)))


(defmethod cl-ds:reset! ((container synchronization-mixin))
  (bt:with-lock-held ((read-lock container))
    (call-next-method)))


(defmethod cl-ds:reset! ((container fixed-capacity-synchronization-mixin))
  (bt:with-lock-held ((read-lock container))
    (let ((full #1=(>= (slot-value container 'cl-ds.queues::%size)
                       (read-capacity container))))
      (prog1 (call-next-method)
        (when (and full (not #1#))
          (~> container read-notify-push bt:condition-notify))))))


(defmethod cl-ds:become-transactional ((container fixed-capacity-synchronized-transactional-2-3-queue))
  (make 'fixed-capacity-synchronized-transactional-2-3-queue
        :capacity (read-capacity container)
        :root (cl-ds.common.2-3:access-root container)
        :ownership-tag (cl-ds.common.abstract:make-ownership-tag)
        :head (copy-array-if-not-nil (access-head container))
        :element-type (read-element-type container)
        :size (cl-ds:size container)
        :head-position (access-head-position container)
        :tail (copy-array-if-not-nil (access-tail container))
        :tail-position (access-tail-position container)
        :tail-end (access-tail-end container)))


(defmethod cl-ds:become-transactional ((container synchronized-transactional-2-3-queue))
  (make 'synchronized-transactional-2-3-queue
        :root (cl-ds.common.2-3:access-root container)
        :ownership-tag (cl-ds.common.abstract:make-ownership-tag)
        :head (copy-array-if-not-nil (access-head container))
        :element-type (read-element-type container)
        :size (cl-ds:size container)
        :head-position (access-head-position container)
        :tail (copy-array-if-not-nil (access-tail container))
        :tail-position (access-tail-position container)
        :tail-end (access-tail-end container)))


(defmethod cl-ds:become-transactional ((container fixed-capacity-synchronized-mutable-2-3-queue))
  (make 'fixed-capacity-synchronized-transactional-2-3-queue
        :capacity (read-capacity container)
        :root (cl-ds.common.2-3:access-root container)
        :ownership-tag (cl-ds.common.abstract:make-ownership-tag)
        :head (copy-array-if-not-nil (access-head container))
        :element-type (read-element-type container)
        :size (cl-ds:size container)
        :head-position (access-head-position container)
        :tail (copy-array-if-not-nil (access-tail container))
        :tail-position (access-tail-position container)
        :tail-end (access-tail-end container)))


(defmethod cl-ds:become-transactional ((container synchronized-mutable-2-3-queue))
  (make 'synchronized-transactional-2-3-queue
        :root (cl-ds.common.2-3:access-root container)
        :ownership-tag (cl-ds.common.abstract:make-ownership-tag)
        :head (copy-array-if-not-nil (access-head container))
        :element-type (read-element-type container)
        :size (cl-ds:size container)
        :head-position (access-head-position container)
        :tail (copy-array-if-not-nil (access-tail container))
        :tail-position (access-tail-position container)
        :tail-end (access-tail-end container)))


(defmethod cl-ds:become-mutable ((container synchronized-transactional-2-3-queue))
  (make 'synchronized-mutable-2-3-queue
        :root (cl-ds.common.2-3:access-root container)
        :size (cl-ds:size container)
        :head (access-head container)
        :element-type (read-element-type container)
        :head-position (access-head-position container)
        :tail (access-tail container)
        :tail-position (access-tail-position container)
        :tail-end (access-tail-end container)))


(defmethod cl-ds:become-mutable ((container fixed-capacity-synchronized-transactional-2-3-queue))
  (make 'fixed-capacity-synchronized-mutable-2-3-queue
        :capacity (read-capacity container)
        :root (cl-ds.common.2-3:access-root container)
        :size (cl-ds:size container)
        :head (access-head container)
        :element-type (read-element-type container)
        :head-position (access-head-position container)
        :tail (access-tail container)
        :tail-position (access-tail-position container)
        :tail-end (access-tail-end container)))


(defmethod cl-ds:whole-range ((container synchronization-mixin))
  (bt:with-lock-held ((read-lock container))
    (make '2-3-queue-range
          :container (cl-ds:become-transactional container)
          :og-container container)))
