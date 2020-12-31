(cl:in-package #:cl-ds.dicts.hamt)


(defclass hamt-dictionary (cl-ds.common.hamt:hamt-container
                           cl-ds.dicts:fundamental-hashing-dictionary)
  ())


(defmethod cl-ds:empty-clone ((container hamt-dictionary))
  (make (type-of container)
        :root nil
        :size 0
        :hash-fn (cl-ds.dicts:read-hash-fn container)))


(defclass functional-hamt-dictionary (cl-ds.dicts:functional-hashing-dictionary
                                      hamt-dictionary)
  ())


(defclass mutable-hamt-dictionary (cl-ds.dicts:mutable-hashing-dictionary
                                   hamt-dictionary)
  ())


(defmacro with-hash-tree-functions ((container &key (cases nil)) &body body)
  "Simple macro adding local functions (all forwards to the container closures)."
  (once-only (container)
    (with-gensyms (!test !hash)
      `(let ((,!test (cl-ds.dicts:read-equal-fn ,container))
             (,!hash (cl-ds.dicts:read-hash-fn ,container)))
         (nest
          ,(if cases
               `(cl-ds.utils:cases
                    ((eq ,!test #'eql)
                     (eq ,!test #'equal)
                     (eq ,!test #'string=)
                     (eq ,!test #'eq)))
               `(progn))
          (flet ((equal-fn (a b)
                   (funcall ,!test a b))
                 (hash-fn (x)
                   (funcall ,!hash x)))
            (declare (ignorable (function hash-fn)
                                (function equal-fn))
                     (inline hash-fn
                             equal-fn)
                     (dynamic-extent (function hash-fn)
                                     (function equal-fn)))
            ,@body))))))


(defclass transactional-hamt-dictionary (mutable-hamt-dictionary
                                         cl-ds.dicts:transactional-hashing-dictionary)
  ())


(-> make-functional-hamt-dictionary ((or function symbol) (or function symbol))
    functional-hamt-dictionary)
(defun make-functional-hamt-dictionary (hash-fn equal-fn)
  (declare (optimize (safety 3)))
  (assure functional-hamt-dictionary (make 'functional-hamt-dictionary
                                           :hash-fn (ensure-function hash-fn)
                                           :root nil
                                           :equal-fn (ensure-function equal-fn))))


(-> make-mutable-hamt-dictionary ((or function symbol) (or function symbol))
    mutable-hamt-dictionary)
(defun make-mutable-hamt-dictionary (hash-fn equal-fn)
  (declare (optimize (safety 3)))
  (assure mutable-hamt-dictionary (make 'mutable-hamt-dictionary
                                        :equal-fn (ensure-function equal-fn)
                                        :hash-fn (ensure-function hash-fn)
                                        :root nil)))

(-> make-transactional-hamt-dictionary
    ((or function symbol) (or function symbol))
    transactional-hamt-dictionary)
(defun make-transactional-hamt-dictionary (hash-fn equal-fn)
  (declare (optimize (safety 3)))
  (~> (make-mutable-hamt-dictionary hash-fn equal-fn)
      cl-ds:become-transactional))


(-> hamt-dictionary-at (hamt-dictionary t) (values t boolean))
(defun hamt-dictionary-at (container location)
  (declare (optimize (speed 3) (safety 1) (debug 0) (space 0)))
  "Implementation of AT"
  (with-hash-tree-functions (container)
    (let* ((hash (hash-fn location))
           (root (access-root container)))
      (declare (type fixnum hash))
      (hash-do
          (node index)
          (root hash)
          :on-leaf (cl-ds.dicts:find-content container
                                             node
                                             location
                                             :hash hash)
          :on-nil (values nil nil)))))


(-> hamt-dictionary-size (hamt-dictionary) non-negative-fixnum)
(defun hamt-dictionary-size (container)
  "Implementation of SIZE"
  (access-size container))


#|

Methods. Those will just call non generic functions.

|#


(defmethod cl-ds:size ((container hamt-dictionary))
  (hamt-dictionary-size container))


(defmethod cl-ds:at ((container hamt-dictionary) location &rest more-locations)
  (cl-ds:assert-one-dimension more-locations)
  (hamt-dictionary-at container location))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:grow-function)
                                             (structure functional-hamt-dictionary)
                                             container
                                             location &rest all &key value)
  (declare (optimize (speed 3)
                     (safety 1)
                     (debug 0)
                     (space 0)))
  (with-hash-tree-functions (structure :cases nil)
    (bind ((changed nil)
           (tag nil)
           (hash (hash-fn location))
           ((:dflet grow-bucket (bucket))
            (multiple-value-bind (a b)
                (apply #'cl-ds.meta:grow-bucket
                       operation
                       container
                       bucket
                       location
                       :hash hash
                       :value value
                       all)
              (setf changed (cl-ds:changed b))
              (values a b changed)))
           ((:dflet copy-on-write (indexes path depth conflict))
            (copy-on-write structure
                           tag
                           indexes
                           path
                           depth
                           conflict))
           ((:dflet make-bucket ())
            (multiple-value-bind (a b)
                (apply #'cl-ds.meta:make-bucket
                       operation
                       container
                       location
                       :hash hash
                       :value value
                       all)
              (setf changed (cl-ds:changed b))
              (values a b changed)))
           ((:values new-root status)
            (go-down-on-path structure
                             hash
                             #'grow-bucket
                             #'make-bucket
                             #'copy-on-write)))
      (values (if changed
                  (make
                   'functional-hamt-dictionary
                   :hash-fn (cl-ds.dicts:read-hash-fn structure)
                   :equal-fn (cl-ds.dicts:read-equal-fn structure)
                   :ownership-tag tag
                   :root new-root
                   :size (if (cl-ds:found status)
                             (the non-negative-fixnum (access-size structure))
                             (1+ (the non-negative-fixnum (access-size structure)))))
                  structure)
              status))))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:grow-function)
                                             (structure transactional-hamt-dictionary)
                                             container
                                             location &rest all &key value)
  (declare (optimize (speed 3)
                     (safety 1)
                     (debug 0)
                     (space 0)))
  (with-hash-tree-functions (structure :cases nil)
    (bind ((hash (hash-fn location))
           (changed nil)
           ((:dflet grow-bucket (bucket))
            (multiple-value-bind (a b)
                (apply #'cl-ds.meta:grow-bucket
                       operation
                       container
                       bucket
                       location
                       :hash hash
                       :value value
                       all)
              (setf changed (cl-ds:changed b))
              (values a b changed)))
           ((:dflet make-bucket ())
            (multiple-value-bind (a b)
                (apply #'cl-ds.meta:make-bucket
                       operation
                       container
                       location
                       :hash hash
                       :value value
                       all)
              (setf changed (cl-ds:changed b))
              (values a b changed)))
           ((:dflet copy-on-write (indexes path depth conflict))
            (transactional-copy-on-write structure
                                         (read-ownership-tag structure)
                                         indexes
                                         path
                                         depth
                                         conflict))
           ((:values new-root status)
            (go-down-on-path structure
                             hash
                             #'grow-bucket
                             #'make-bucket
                             #'copy-on-write)))
      (when changed
        (setf (access-root structure) new-root)
        (unless (cl-ds:found status)
          (incf (the non-negative-fixnum (access-size structure)))))
      (values structure
              status))))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:shrink-function)
                                             (structure functional-hamt-dictionary)
                                             container
                                             location
                                             &rest all
                                             &key)
  (declare (optimize (speed 3)
                     (safety 1)
                     (debug 0)
                     (space 0)))
  (with-hash-tree-functions (structure :cases nil)
    (bind ((hash (hash-fn location))
           (tag nil)
           (changed nil)
           ((:dflet shrink-bucket (bucket))
            (multiple-value-bind (a b)
                (apply #'cl-ds.meta:shrink-bucket
                       operation
                       container
                       bucket
                       location
                       :hash hash
                       all)
              (setf changed (cl-ds:changed b))
              (values a b changed)))
           ((:dflet copy-on-write (indexes path depth conflict))
            (copy-on-write structure
                           tag
                           indexes
                           path
                           depth
                           conflict))
           ((:dflet just-return ())
            (return-from cl-ds.meta:position-modification
              (values structure
                      cl-ds.common:empty-eager-modification-operation-status)))
           ((:values new-root status)
            (go-down-on-path structure
                             hash
                             #'shrink-bucket
                             #'just-return
                             #'copy-on-write)))
      (values (if changed
                  (make 'functional-hamt-dictionary
                        :hash-fn (cl-ds.dicts:read-hash-fn structure)
                        :equal-fn (cl-ds.dicts:read-equal-fn structure)
                        :root new-root
                        :ownership-tag tag
                        :size (1- (the non-negative-fixnum (access-size structure))))
                  structure)
              status))))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:shrink-function)
                                             (structure transactional-hamt-dictionary)
                                             container
                                             location
                                             &rest all
                                             &key)
  (declare (optimize (speed 3)
                     (safety 1)
                     (debug 0)
                     (space 0)))
  (with-hash-tree-functions (structure :cases nil)
    (bind ((hash (hash-fn location))
           (changed nil)
           ((:dflet shrink-bucket (bucket))
            (multiple-value-bind (a b)
                (apply #'cl-ds.meta:shrink-bucket
                       operation
                       container
                       bucket
                       location :hash hash
                       all)
              (setf changed (cl-ds:changed b))
              (values a b changed)))
           ((:dflet just-return ())
            (return-from cl-ds.meta:position-modification
              (values structure
                      cl-ds.common:empty-eager-modification-operation-status)))
           ((:dflet copy-on-write (indexes path depth conflict))
            (transactional-copy-on-write structure
                                         (read-ownership-tag structure)
                                         indexes
                                         path
                                         depth
                                         conflict))
           ((:values new-root status)
            (go-down-on-path structure
                             hash
                             #'shrink-bucket
                             #'just-return
                             #'copy-on-write)))
      (when changed
        (setf (access-root structure) new-root)
        (decf (the non-negative-fixnum (access-size structure))))
      (values structure
              status))))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:shrink-function)
                                             (structure mutable-hamt-dictionary)
                                             container
                                             location
                                             &rest all
                                             &key)
  (declare (optimize (speed 3)
                     (safety 1)
                     (debug 0)
                     (space 0)))
  (with-hash-tree-functions (structure)
    (let* ((modification-status nil)
           (hash (hash-fn location))
           (new-root
             (with-destructive-erase-hamt node structure hash
               :on-leaf
               (multiple-value-bind (bucket status)
                   (apply #'cl-ds.meta:shrink-bucket!
                          operation
                          container
                          node
                          location
                          :hash hash
                          all)
                 (unless (cl-ds:changed status)
                   (return-from cl-ds.meta:position-modification
                     (values structure status)))
                 (setf modification-status status)
                 bucket)
               :on-nil
               (return-from cl-ds.meta:position-modification
                 (values
                  structure
                  cl-ds.common:empty-eager-modification-operation-status)))))
      (decf (access-size structure))
      (setf (access-root structure) new-root)
      (values structure
              modification-status))))


(defmethod cl-ds.meta:position-modification ((operation cl-ds.meta:grow-function)
                                             (structure mutable-hamt-dictionary)
                                             container
                                             location
                                             &rest all
                                             &key value)
  (declare (optimize (speed 3) (safety 1) (debug 0) (space 0))
           (ignore all))
  (let ((status nil)
        (hash (funcall (the (-> (t) fixnum)
                            (cl-ds.dicts:read-hash-fn structure)) location)))
    (macrolet ((handle-bucket (&body body)
                 `(multiple-value-bind (bucket s)
                      ,@body
                    (unless (cl-ds:changed s)
                      (return-from cl-ds.meta:position-modification
                        (values structure
                                s)))
                    (setf status s)
                    bucket)))
      (let* ((prev-node nil)
             (prev-index 0)
             (root (access-root structure))
             (tag (read-ownership-tag structure))
             (result
               (hash-do
                   (node index c)
                   ((access-root structure) hash)
                   :on-every (setf prev-node node prev-index index)
                   :on-nil (if prev-node
                               (progn
                                 (hash-node-insert!
                                  prev-node
                                  (rebuild-rehashed-node
                                   structure
                                   c
                                   (handle-bucket
                                    (cl-ds.meta:make-bucket operation
                                                            container
                                                            location
                                                            :hash hash
                                                            :value value))
                                   tag)
                                  prev-index)
                                 root)
                               (handle-bucket
                                (cl-ds.meta:make-bucket operation
                                                        container
                                                        location
                                                        :hash hash
                                                        :value value)))
                   :on-leaf (if prev-node
                                (progn
                                  (hash-node-replace!
                                   prev-node
                                   (rebuild-rehashed-node structure c
                                                          (handle-bucket
                                                           (cl-ds.meta:grow-bucket! operation
                                                                                    container
                                                                                    node
                                                                                    location
                                                                                    :hash hash
                                                                                    :value value))
                                                          tag)
                                   prev-index)
                                  root)
                                (rebuild-rehashed-node
                                 structure
                                 c
                                 (handle-bucket
                                  (cl-ds.meta:grow-bucket! operation
                                                           container
                                                           node
                                                           location
                                                           :hash hash
                                                           :value value))
                                 tag)))))
        (setf (access-root structure) result)
        (unless (cl-ds:found status)
          (incf (the fixnum (access-size structure))))
        (values structure
                status)))))


(defmethod cl-ds:become-mutable ((container functional-hamt-dictionary))
  (make 'mutable-hamt-dictionary
        :hash-fn (cl-ds.dicts:read-hash-fn container)
        :root (access-root container)
        :equal-fn (cl-ds.dicts:read-equal-fn container)
        :size (access-size container)))


(defmethod cl-ds:become-functional ((container mutable-hamt-dictionary))
  (make 'functional-hamt-dictionary
        :hash-fn (cl-ds.dicts:read-hash-fn container)
        :root (access-root container)
        :equal-fn (cl-ds.dicts:read-equal-fn container)
        :size (access-size container)))


(defmethod cl-ds:become-transactional ((container hamt-dictionary))
  (make 'transactional-hamt-dictionary
        :hash-fn (cl-ds.dicts:read-hash-fn container)
        :root (access-root container)
        :ownership-tag (cl-ds.common.abstract:make-ownership-tag)
        :equal-fn (cl-ds.dicts:read-equal-fn container)
        :size (access-size container)))


(defmethod cl-ds:become-mutable ((container transactional-hamt-dictionary))
  (let ((root (~> container access-root)))
    (make 'mutable-hamt-dictionary
          :hash-fn (cl-ds.dicts:read-hash-fn container)
          :root root
          :equal-fn (cl-ds.dicts:read-equal-fn container)
          :size (access-size container))))


(defmethod cl-ds:replica ((container hamt-dictionary)
                          &optional isolate)
  (cl-ds.common.abstract:replica container isolate))


(defmethod cl-ds:become-functional ((container transactional-hamt-dictionary))
  (let ((root (~> container access-root)))
    (make 'functional-hamt-dictionary
          :hash-fn (cl-ds.dicts:read-hash-fn container)
          :root root
          :equal-fn (cl-ds.dicts:read-equal-fn container)
          :size (access-size container))))


(flet ((fn (x)
         (list* (cl-ds.common:hash-content-location x)
                (cl-ds.common:hash-dict-content-value x))))
  (defmethod get-range-key-function ((container hamt-dictionary))
    #'fn))


(defmethod cl-ds:whole-range ((container hamt-dictionary))
  (make 'cl-ds.common:forward-tree-range
        :obtain-value #'obtain-value
        :key (get-range-key-function container)
        :forward-stack (if (null (access-root container))
                           nil
                           (~> container access-root new-cell list))
        :container container))


(defmethod cl-ds:across ((container hamt-dictionary) function)
  (labels ((impl (node)
             (if (listp node)
                 (cl-ds.meta:map-bucket container node function)
                 (iterate
                   (with content = (cl-ds.common.hamt:hash-node-content node))
                   (for i from 0 below (cl-ds.common.hamt:hash-node-size node))
                   (impl (aref content i))))))
    (impl (access-root container))
    container))


(defmethod cl-ds:reset! ((obj mutable-hamt-dictionary))
  (bind (((:accessors (root cl-ds.common.hamt:access-root)
                      (size cl-ds.common.hamt:access-size))
          obj))
    (setf root nil
          size 0)
    obj))


(defmethod cl-ds:make-from-traversable (traversable
                                        (class (eql 'mutable-hamt-dictionary))
                                        &rest arguments)
  (let* ((hash-fn (getf arguments :hash-fn))
         (equal-fn (getf arguments :equal-fn))
         (result (make-mutable-hamt-dictionary hash-fn equal-fn)))
    (cl-ds:across traversable
                  (lambda (x &aux (key (car x)) (value (cdr x)))
                    (setf (cl-ds:at result key) value)))
    result))


(defmethod cl-ds:make-from-traversable (traversable
                                        (class (eql 'functional-hamt-dictionary))
                                        &rest arguments)
  (~> (apply #'cl-ds:make-from-traversable
             traversable
             'mutable-hamt-dictionary
             arguments)
      cl-ds:become-functional))


(defmethod cl-ds:make-from-traversable (traversable
                                        (class (eql 'transactional-hamt-dictionary))
                                        &rest arguments)
  (~> (apply #'cl-ds:make-from-traversable
             traversable
             'mutable-hamt-dictionary
             arguments)
      cl-ds:become-transactional))
