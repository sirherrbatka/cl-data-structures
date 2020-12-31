(cl:in-package #:cl-data-structures.common.abstract)


(defun make-ownership-tag ()
  (list t))


(defclass fundamental-ownership-tagged-object ()
  ((%ownership-tag :reader read-ownership-tag
                   :writer write-ownership-tag
                   :initform nil
                   :initarg :ownership-tag)))


(defclass tagged-node ()
  ((%ownership-tag :initarg :ownership-tag
                   :reader read-ownership-tag)))


(defstruct tagged-struct-node
  ownership-tag)


(defun tagged-node-ownership-tag (node)
  (declare (optimize (speed 3) (safety 0)))
  (slot-value node '%ownership-tag))


(declaim (inline acquire-ownership))
(-> acquire-ownership (t (or list tagged-node t)) boolean)
(defun acquire-ownership (node ownership-tag)
  (declare (optimize (speed 3)))
  (typecase node
    (list (eq (cdr node) ownership-tag))
    (tagged-struct-node (eq (tagged-struct-node-ownership-tag node) ownership-tag))
    (tagged-node (eq (read-ownership-tag node) ownership-tag))
    (t nil)))


(defun reset-ownership-tag (object)
  (write-ownership-tag (make-ownership-tag) object))


(defun replica (object isolate)
  (check-type object fundamental-ownership-tagged-object)
  (lret ((result (cl-ds:become-transactional object)))
    (when isolate
      (reset-ownership-tag object))))


(defun struct-accessor-name (type slot)
  (intern (format nil "~a-~a" type
                    (if (listp slot) (first slot) slot))))


(defmacro define-tagged-untagged-node (name &body slots)
  (let* ((string (symbol-name name))
         (tagged-name (intern (format nil "~a-TAGGED" string)))
         (untagged-name (intern (format nil "~a-UNTAGGED" string))))
    `(progn
       (defstruct ,untagged-name
         ,@slots)
       (defstruct (,tagged-name (:include tagged-struct-node))
         ,@slots)
       (deftype ,name ()
         `(or ,',untagged-name ,',tagged-name))
       ,@(iterate
           (for slot in slots)
           (for fname = (struct-accessor-name name slot))
           (collect `(defun ,fname
                         (argument)
                       (declare (optimize (speed 3)))
                       (if (typep argument ',tagged-name)
                           (,(struct-accessor-name tagged-name slot) argument)
                           (,(struct-accessor-name untagged-name slot) argument))))
           (collect `(defun (setf ,fname)
                         (new-value argument)
                       (declare (optimize (speed 3)))
                       (if (typep argument ',tagged-name)
                            (setf (,(struct-accessor-name tagged-name slot) argument)
                                   new-value)
                            (setf  (,(struct-accessor-name untagged-name slot) argument)
                                   new-value)))))
       (defun ,(intern (format nil "MAKE-~a" name))
            (&key ,@(iterate
                      (for slot in slots)
                      (for symbolp = (symbolp slot))
                      (for symbol = (if symbolp slot (first slot)))
                      (if symbolp
                          (collect symbol)
                          (collect `(,symbol ,(second slot)))))
               ownership-tag)
          (if (null ownership-tag)
              (,(intern (format nil "MAKE-~a" untagged-name))
               ,@(iterate
                   (for slot in slots)
                   (for symbol = (if (symbolp slot) slot (first slot)))
                   (collect (make-keyword symbol))
                   (collect symbol)))
              (,(intern (format nil "MAKE-~a" tagged-name))
               ,@(iterate
                   (for slot in slots)
                   (for symbol = (if (symbolp slot) slot (first slot)))
                   (collect (make-keyword symbol))
                   (collect symbol))
               ,(make-keyword 'ownership-tag) ownership-tag))))))
