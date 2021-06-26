(cl:in-package #:cl-data-structures.common.skip-list)


(defstruct skip-list-node
  (pointers #() :type simple-vector)
  (content nil :type t))


(defstruct (assoc-skip-list-node (:include skip-list-node))
  (value nil :type t))


(defmethod print-object ((object skip-list-node) stream)
  (print-unreadable-object (object stream :type nil :identity nil)
    (format stream "[~a]" (skip-list-node-content object))
    (print-object (skip-list-node-at object 0) stream)))


(cl-ds.utils:define-list-of-slots skip-list-node ()
  (pointers skip-list-node-pointers)
  (level skip-list-node-level)
  (content skip-list-node-content))

(cl-ds.utils:define-list-of-slots assoc-skip-list-node (skip-list-node)
  (value assoc-skip-list-node-value))


(-> skip-list-node-level (skip-list-node) fixnum)
(defun skip-list-node-level (skip-list-node)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (~> skip-list-node skip-list-node-pointers length))


(-> skip-list-node-at (skip-list-node cl-ds.utils:index) t)
(defun skip-list-node-at (skip-list-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (~> skip-list-node skip-list-node-pointers (aref index)))


(-> (setf skip-list-node-at)
    ((or null skip-list-node) skip-list-node cl-ds.utils:index)
    (or null skip-list-node))
(defun (setf skip-list-node-at) (new-value skip-list-node index)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (cl-ds.utils:with-slots-for (skip-list-node skip-list-node)
    (setf (aref pointers index) new-value)))


(defgeneric skip-list-node-clone (node))


(defmethod skip-list-node-clone ((skip-list-node (eql nil)))
  (values nil (make-hash-table :test 'eq)))


(defmethod skip-list-node-clone ((skip-list-node skip-list-node))
  (bind ((stack (vect))
         (table (make-hash-table :test 'eq))
         ((:labels impl (skip-list-node))
          (if (null skip-list-node)
              nil
              (if-let ((existing-node (gethash skip-list-node table)))
                existing-node
                (if (typep skip-list-node 'assoc-skip-list-node)
                    (cl-ds.utils:with-slots-for (skip-list-node assoc-skip-list-node)
                      (lret ((result (make-assoc-skip-list-node
                                      :pointers (copy-array pointers)
                                      :value value
                                      :content content)))
                        (setf (gethash skip-list-node table) result
                              (gethash result table) result)
                        (vector-push-extend (skip-list-node-pointers result)
                                            stack)))
                    (cl-ds.utils:with-slots-for (skip-list-node skip-list-node)
                      (lret ((result (make-skip-list-node
                                      :pointers (copy-array pointers)
                                      :content content)))
                        (setf (gethash skip-list-node table) result
                              (gethash result table) result)
                        (vector-push-extend (skip-list-node-pointers result)
                                            stack))))))))
    (iterate
      (with result = (impl skip-list-node))
      (for fill-pointer = (fill-pointer stack))
      (until (zerop fill-pointer))
      (for pointers = (aref stack (1- fill-pointer)))
      (decf (fill-pointer stack))
      (cl-ds.utils:transform #'impl pointers)
      (finally (return (values result table))))))


(defmethod skip-list-node-clone ((skip-list-node assoc-skip-list-node))
  (bind ((stack (vect))
         (table (make-hash-table :test 'eq))
         ((:labels impl (skip-list-node))
          (if (null skip-list-node)
              nil
              (if-let ((existing-node (gethash skip-list-node table)))
                existing-node
                (cl-ds.utils:with-slots-for (skip-list-node assoc-skip-list-node)
                  (lret ((result (make-assoc-skip-list-node
                                  :value value
                                  :pointers (copy-array pointers)
                                  :content content)))
                    (setf (gethash skip-list-node table) result
                          (gethash result table) result)
                    (vector-push-extend (skip-list-node-pointers result)
                                        stack)))))))
    (iterate
      (with result = (impl skip-list-node))
      (for fill-pointer = (fill-pointer stack))
      (until (zerop fill-pointer))
      (for pointers = (aref stack (1- fill-pointer)))
      (decf (fill-pointer stack))
      (cl-ds.utils:transform #'impl pointers)
      (finally (return (values result table))))))


(-> copy-into! (simple-vector simple-vector &optional fixnum fixnum) simple-vector)
(declaim (inline copy-into!))
(defun copy-into! (destination source
                   &optional
                     (limit (min (length (the simple-vector destination))
                                 (length (the simple-vector source))))
                     (start 0))
  (declare (optimize (speed 3) (debug 0) (safety 0))
           (type fixnum limit))
  (iterate
    (declare (type fixnum i))
    (for i from start below limit)
    (setf (aref destination i) (aref source i))
    (finally (return destination))))


(-> skip-list-node-update-pointers! (skip-list-node simple-vector) skip-list-node)
(defun skip-list-node-update-pointers! (skip-list-node new-pointers)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (cl-ds.utils:with-slots-for (skip-list-node skip-list-node)
    (copy-into! pointers new-pointers))
  skip-list-node)


(-> skip-list-node-compare (function (or null skip-list-node) (or null skip-list-node)) boolean)
(declaim (inline skip-list-node-compare))
(defun skip-list-node-compare (test node1 node2)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (cl-ds.utils:cond+ ((null node1) (null node2))
    ((t t) nil)
    ((nil t) t)
    ((t nil) nil)
    ((nil nil) (funcall test
                        (skip-list-node-content node1)
                        (skip-list-node-content node2)))))


(-> new-node-update-pointers! (function skip-list-node simple-vector) skip-list-node)
(defun new-node-update-pointers! (test spliced-node pointers)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (iterate
    (declare (type fixnum i))
    (with spliced-level = (skip-list-node-level spliced-node))
    (for i from 0 below (length pointers))
    (for rest = (aref pointers i))
    (when (null rest)
      (next-iteration))
    (cl-ds.utils:with-slots-for (rest skip-list-node)
      (iterate
        (declare (type fixnum j))
        (for j from (the fixnum (1- (min level spliced-level))) downto 0)
        (if (or (null (aref pointers j))
                (skip-list-node-compare test spliced-node
                                        (aref pointers j)))
            (setf (aref pointers j) spliced-node)
            (finish))))
    (finally (return spliced-node))))


(-> random-level (positive-fixnum) positive-fixnum)
(defun random-level (maximum-level)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (iterate
    (declare (type fixnum i))
    (for i from 1 to maximum-level)
    (until (zerop (random 2)))
    (finally (return i))))


(-> make-skip-list-node-of-level (fixnum &optional boolean) skip-list-node)
(defun make-skip-list-node-of-level (level &optional assoc)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if assoc
      (make-skip-list-node :pointers (make-array level :initial-element nil))
      (make-assoc-skip-list-node :pointers (make-array level :initial-element nil))))


(-> make-skip-list-node-of-random-level (fixnum &optional boolean) skip-list-node)
(defun make-skip-list-node-of-random-level (maximum-level &optional assoc)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (make-skip-list-node-of-level (random-level maximum-level) assoc))


(declaim (notinline locate-node))
(-> locate-node (simple-vector t function) (values simple-vector
                                                   simple-vector))
(defun locate-node (pointers item test)
  (declare (optimize (speed 3) (safety 0) (debug 0)
                     (compilation-speed 0) (space 0)))
  (let* ((pointers-length (length pointers))
         (prev-result (make-array pointers-length
                                  :initial-element nil))
         (last (1- pointers-length)))
    (iterate
      (declare (type fixnum i)
               (type simple-vector result))
      (with result = (copy-array pointers))
      (with i = last)
      (for node = (aref result i))
      (cl-ds.utils:with-slots-for (node skip-list-node)
        (when (and node (funcall test content item))
          (copy-into! prev-result result level)
          (copy-into! result pointers)
          (setf i level)))
      (decf i)
      (while (>= i 0))
      (finally (return (values result prev-result))))))


(defun gather-pointers (node pointers)
  (setf (aref pointers 0) node)
  (iterate
    (with level = (length pointers))
    (with current-level = 0)
    (while (< current-level level))
    (until (null node))
    (for node-level = (skip-list-node-level node))
    (iterate
      (for i from (1+ current-level) below node-level)
      (for next = (skip-list-node-at node i))
      (setf (aref pointers i) next))
    (when (> node-level (1+ current-level))
      (setf current-level node-level))
    (setf node (iterate
                 (for i from (1- node-level) downto 0)
                 (for next = (skip-list-node-at node i))
                 (finding next such-that (not (null next)))))
    (finally (return pointers))))


(-> insert-node-between! (simple-vector simple-vector function skip-list-node) skip-list-node)
(defun insert-node-between! (pointers previous-pointers test skip-list-node)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (new-node-update-pointers! test skip-list-node previous-pointers)
  (skip-list-node-update-pointers! skip-list-node pointers)
  skip-list-node)


(-> delete-node-between! (simple-vector simple-vector) skip-list-node)
(defun delete-node-between! (pointers prev-pointers)
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (lret ((result (aref pointers 0)))
    (assert (not (null result)))
    (iterate
      (declare (type fixnum i next-size))
      (with next = (skip-list-node-pointers result))
      (with next-size = (length next))
      (for i from 0 below next-size)
      (for node = (aref prev-pointers i))
      (copy-into! (skip-list-node-pointers node)
                  next))))


(defclass fundamental-skip-list (cl-ds:mutable cl-ds:fundamental-container)
  ((%ordering-function :initarg :ordering-function
                       :reader read-ordering-function)
   (%pointers :initarg :pointers
              :reader read-pointers
              :type simple-vector)
   (%maximum-level :initarg :maximum-level
                   :accessor access-maximum-level))
  (:default-initargs
   :maximum-level 32))


(defmethod cl-ds:size ((container fundamental-skip-list))
  (iterate
   (for i from 0)
   (for node
        initially (~> container read-pointers (aref 0))
        then (~> node skip-list-node-pointers (aref 0)))
   (until (null node))
   (finally (return i))))


(cl-ds.utils:define-list-of-slots fundamental-skip-list ()
  (ordering-function read-ordering-function)
  (pointers read-pointers)
  (maximum-level access-maximum-level))


(-> skip-list-locate-node (fundamental-skip-list t) (values simple-vector
                                                            simple-vector))
(defun skip-list-locate-node (skip-list item)
  (cl-ds.utils:with-slots-for (skip-list fundamental-skip-list)
    (locate-node pointers item ordering-function)))


(defun update-head-pointers! (skip-list skip-list-node)
  (declare (type skip-list-node skip-list-node)
           (type fundamental-skip-list skip-list)
           (optimize (speed 3) (debug 0) (safety 0)))
  (iterate
    (declare (type fixnum i)
             (type simple-vector head))
    (with head = (read-pointers skip-list))
    (with content = (skip-list-node-content skip-list-node))
    (with ordering-function = (ensure-function (read-ordering-function skip-list)))
    (for i from (~> skip-list-node skip-list-node-level 1-) downto 0)
    (for node = (aref head i))
    (when (null node)
      (setf (aref head i) skip-list-node)
      (next-iteration))
    (for old-content = (skip-list-node-content node))
    (for should-go-before = (funcall ordering-function content old-content))
    (if should-go-before
        (setf (aref head i) skip-list-node)
        (finish))))


(defmethod cl-ds.utils:cloning-information append ((object fundamental-skip-list))
  '((:pointers read-pointers)
    (:ordering-function read-ordering-function)
    (:maximum-level access-maximum-level)))


(defclass fundamental-skip-list-range (cl-ds:fundamental-forward-range)
  ((%current-node :initarg :current-node
                  :accessor access-current-node)
   (%initial-current-node :initarg :current-node
                          :reader read-initial-current-node)
   (%last-node :initarg :last-node
               :reader read-last-node))
  (:default-initargs
   :last-node nil))


(defmethod cl-ds.utils:cloning-information
    append ((object fundamental-skip-list-range))
  '((:current-node access-current-node)
    (:last-node read-last-node)))


(defmethod cl-ds:reset! ((object fundamental-skip-list-range))
  (setf (access-current-node object) (read-initial-current-node object))
  object)


(defmethod cl-ds:peek-front ((object fundamental-skip-list-range))
  (let* ((result (access-current-node object))
         (last-node (read-last-node object)))
    (if (eq result last-node)
        nil
        result)))


(defmethod cl-ds:consume-front ((object fundamental-skip-list-range))
  (let* ((result (access-current-node object))
         (last-node (read-last-node object)))
    (if (eq result last-node)
        (progn
          (setf (access-current-node object) nil)
          nil)
        (progn
          (setf (access-current-node object) (skip-list-node-at result 0))
          result))))


(defmethod cl-ds:drop-front ((object fundamental-skip-list-range) count)
  (check-type count non-negative-integer)
  (iterate
    (for result
         initially (cl-ds:peek-front object)
         then (skip-list-node-at result 0))
    (until (null result))
    (repeat count)
    (finally (setf (access-current-node object) result)))
  object)


(defmethod cl-ds:traverse ((object fundamental-skip-list-range) function)
  (ensure-functionf function)
  (iterate
    (with last-node = (read-last-node object))
    (with result = (access-current-node object))
    (until (eq last-node result))
    (funcall function result)
    (setf result (skip-list-node-at result 0)
          (access-current-node object) result))
  object)


(defmethod cl-ds:across ((object fundamental-skip-list-range) function)
  (ensure-functionf function)
  (iterate
    (with last-node = (read-last-node object))
    (for result
         initially (access-current-node object)
         then (skip-list-node-at result 0))
    (until (eq last-node result))
    (funcall function result))
  object)


(defmethod cl-ds:clone ((range fundamental-skip-list-range))
  (cl-ds.utils:clone range))


(defmethod cl-ds:clone ((container fundamental-skip-list))
  (cl-ds.utils:with-slots-for (container fundamental-skip-list)
    (make (class-of container)
          :size size
          :ordering-function ordering-function
          :pointers (map 'vector
                         (rcurry #'gethash
                                 (nth-value 1 (~> pointers
                                                  (aref 0)
                                                  skip-list-node-clone)))
                         pointers)
          :maximum-level maximum-level)))


(defmethod cl-ds:empty-clone ((container fundamental-skip-list))
  (cl-ds.utils:with-slots-for (container fundamental-skip-list)
    (make (class-of container)
          :size 0
          :ordering-function ordering-function
          :pointers (make-array maximum-level :initial-element nil)
          :maximum-level maximum-level)))


(defmethod cl-ds:reset! ((container fundamental-skip-list))
  (cl-ds.utils:with-slots-for (container fundamental-skip-list)
    (setf size 0)
    (cl-ds.utils:transform (constantly nil) pointers)
    container))


(defmethod cl-ds.meta:position-modification ((function cl-ds.meta:erase*!-function)
                                             (structure fundamental-skip-list)
                                             container
                                             (location fundamental-skip-list-range)
                                             &rest all)
  (declare (ignore all container))
  (bind ((starting-node (access-current-node location))
         (ending-node (read-last-node location)))
    (when (eq starting-node ending-node)
      (return-from cl-ds.meta:position-modification
        (values structure
                (cl-ds.common:make-eager-modification-operation-status
                 nil
                 location
                 nil))))
    (bind ((pointers (read-pointers structure))
           (level (length pointers))
           (ordering-function (read-ordering-function structure))
           ((:values current prev)
            (locate-node pointers
                         (skip-list-node-content starting-node)
                         ordering-function)))
      (unless (eq starting-node (aref current 0))
        (return-from cl-ds.meta:position-modification
          (values structure
                  (cl-ds.common:make-eager-modification-operation-status
                   nil
                   location
                   nil))))
      (if (null ending-node)
          (progn
            (iterate
              (for p in-vector prev)
              (unless (null p)
                (map-into (skip-list-node-pointers p)
                          (constantly nil))))
            (iterate
              (for i from 0 below level)
              (for node = (aref pointers i))
              (when (null node)
                (next-iteration))
              (when (funcall ordering-function
                             (skip-list-node-content starting-node)
                             (skip-list-node-content node))
                (setf (aref pointers i) nil))))
          (let ((nexts (gather-pointers ending-node
                                        (make-array level :initial-element nil))))
            (iterate
              (for p in-vector prev)
              (unless (null p)
                (iterate
                  (with node-pointers = (skip-list-node-pointers p))
                  (for i from 0 below (length  node-pointers))
                  (when (eq (aref node-pointers i) starting-node)
                    (setf (aref node-pointers i) (aref nexts i))))))
            (iterate
              (for i from 0 below level)
              (when (and (not (skip-list-node-compare ordering-function
                                                      (aref pointers i)
                                                      starting-node))
                         (skip-list-node-compare ordering-function
                                                 (aref pointers i)
                                                 (aref nexts i)))
                (setf (aref pointers i) (aref nexts i))))))
      (values structure
              (cl-ds.common:make-eager-modification-operation-status
               t location t)))))


(defmethod cl-ds:traverse ((object fundamental-skip-list)
                           function)
  (ensure-functionf function)
  (cl-ds:traverse (cl-ds:whole-range object)
                  function)
  object)

(defmethod cl-ds:across ((object fundamental-skip-list)
                         function)
  (cl-ds:traverse object function))
