(cl:in-package #:cl-data-structures.common)


(defstruct changes
  (content (vect) :type vector)
  (parent nil :type (or changes null))
  (end-index 1 :type fixnum)
  (unique t :type boolean))


(defun execute-changes (changes instance)
  (declare (type (or null changes) changes))
  (unless (null changes)
    (bind ((parent (changes-parent changes))
           ((:vectors content) (changes-content changes)))
      (unless (null parent)
        (execute-changes parent instance))
      (iterate
        (for i from 0 below (changes-end-index changes))
        (funcall (content i) instance)))))


(defun add-change (changes next)
  (if (null changes)
      (make-changes :content (vect next))
      (if (changes-unique changes)
          (progn
            (vector-push-extend next (changes-content changes))
            (setf (changes-unique changes) nil)
            (make-changes :content (changes-content changes)
                          :end-index (1+ (changes-end-index changes))))
          (make-changes :content (vect next)
                        :parent changes))))


(defclass lazy-modification-operation-status
    (cl-ds:fundamental-modification-operation-status)
  ((%eager-status :accessor access-eager-status
                  :type eager-modification-operation-status
                  :initarg :eager-status)
   (%lazy-instance :reader read-lazy-instance
                   :writer write-lazy-instance
                   :initarg :lazy-instance)))


(defclass box-container (cl-ds:fundamental-container)
  ((%content :initarg :content
             :type fundamental-container
             :accessor access-content)))


(defclass lazy-box-container (cl-ds:lazy box-container)
  ((%operations :initform nil
                :type (or null changes)
                :initarg :operations
                :accessor access-operations)))


(-> force-version (lazy-box-container) lazy-box-container)
(defun force-version (instance)
  (bind (((:accessors (operations access-operations)
                      (content access-content))
          instance)
         (transactional-instance (cl-ds:become-transactional content)))
    (execute-changes operations transactional-instance)
    (setf operations nil
          content transactional-instance)
    instance))


(flet ((enclose-wrapper (t-operation location lazy-status args)
         (lambda (instance)
           (assert (cl-ds:transactionalp instance))
           (let ((eager-status
                   (nth-value 1
                              (apply #'cl-ds.meta:position-modification
                                     t-operation
                                     instance
                                     instance
                                     location
                                     args))))
             (if (slot-boundp lazy-status '%eager-status)
                 (let ((status (access-eager-status lazy-status)))
                   (assert (eq (read-found eager-status)
                               (read-found status)))
                   (assert (eq (read-value eager-status)
                               (read-value status))))
                 (setf (access-eager-status lazy-status) eager-status))
             eager-status))))
  (defmethod cl-ds.meta:position-modification (operation
                                               container
                                               (structure lazy-box-container)
                                               location &rest args
                                               &key &allow-other-keys)
    (bind (((:accessors (operations access-operations)
                        (content access-content))
            container)
           (lazy-status (make 'lazy-modification-operation-status))
           (t-operation (cl-ds.meta:destructive-counterpart operation))
           (next-instance (make (type-of container)
                                :content content
                                :operations (add-change operations
                                                        (enclose-wrapper t-operation
                                                                         location
                                                                         lazy-status
                                                                         args)))))
      (write-lazy-instance next-instance lazy-status)
      (values next-instance
              lazy-status))))


(defmethod cl-ds:size ((container lazy-box-container))
  (force-version container)
  (cl-ds:size (access-content container)))


(defmethod cl-ds:reset! ((container lazy-box-container))
  (force-version container)
  (cl-ds:reset! (access-content container)))


(defmethod cl-ds:become-transactional ((container lazy-box-container))
  (force-version container)
  (cl-ds:become-transactional (access-content container)))


(defmethod cl-ds:become-functional ((container lazy-box-container))
  (force-version container)
  (cl-ds:become-functional (access-content container)))


(defmethod cl-ds:become-mutable ((container lazy-box-container))
  (force-version container)
  (cl-ds:become-mutable (access-content container)))


(flet ((force-status (status)
         (unless (slot-boundp status '%eager-status)
           (force-version (read-lazy-instance status)))
         (access-eager-status status)))

  (defmethod cl-ds:found ((status lazy-modification-operation-status))
    (cl-ds:found (force-status status)))

  (defmethod cl-ds:value ((status lazy-modification-operation-status))
    (cl-ds:value (force-status status)))

  (defmethod cl-ds:changed ((status lazy-modification-operation-status))
    (cl-ds:changed (force-status status))))
