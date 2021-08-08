(cl:in-package #:cl-data-structures.common.skip-list)


(defstruct skip-list-node
  (pointers #() :type simple-vector)
  (content nil :type t))


(defstruct (assoc-skip-list-node (:include skip-list-node))
  (value nil :type t))


(defun make-skip-list-node* (assoc &rest initargs)
  (if assoc
      (apply #'make-assoc-skip-list-node initargs)
      (apply #'make-skip-list-node initargs)))


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
        (if (skip-list-node-compare test
                                    spliced-node
                                    (aref pointers j))
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


(-> make-skip-list-node-of-level (fixnum &optional t boolean) skip-list-node)
(defun make-skip-list-node-of-level (level &optional (value nil value-bound) (assoc value-bound))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (if assoc
      (make-skip-list-node :pointers (make-array level :initial-element nil))
      (make-assoc-skip-list-node :pointers (make-array level :initial-element nil)
                                 :value value)))


(-> make-skip-list-node-of-random-level (fixnum &optional t) skip-list-node)
(defun make-skip-list-node-of-random-level (maximum-level &optional (value nil value-bound))
  (make-skip-list-node-of-level (random-level maximum-level) value value-bound))


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
   (%test-function :initarg :test-function
                   :accessor access-test-function)
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
    (:test-function access-test-function)
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
          :test-function (access-test-function container)
          :maximum-level maximum-level)))


(defmethod cl-ds:reset! ((container fundamental-skip-list))
  (cl-ds.utils:with-slots-for (container fundamental-skip-list)
    (setf size 0)
    (cl-ds.utils:transform (constantly nil) pointers)
    container))


(defmethod cl-ds.meta:position-modification
    ((function cl-ds.meta:erase!-function)
     (structure fundamental-skip-list)
     container
     location
     &rest all)
  (declare (ignore all container))
  (bind ((pointers (cl-ds.common.skip-list:read-pointers structure))
         (test (cl-ds.common.skip-list:read-ordering-function structure))
         ((:values current prev)
          (cl-ds.common.skip-list:locate-node pointers location test))
         (result (aref current 0)))
    (when (null result)
      (return-from cl-ds.meta:position-modification
        (values structure
                cl-ds.common:empty-eager-modification-operation-status)))
    (let ((content (cl-ds.common.skip-list:skip-list-node-content result)))
      (if (~> structure access-test-function (funcall content location))
          (let ((rests (cl-ds.common.skip-list:skip-list-node-pointers
                        result))
                (level (cl-ds.common.skip-list:skip-list-node-level
                        result)))
            (iterate
              (declare (type fixnum i))
              (for i from (1- level) downto 0)
              (if (eq (aref pointers i) result)
                  (setf (aref pointers i)
                        (if (< i level)
                            (aref rests i)
                            nil))
                  (finish)))
            (when (<= level (length pointers))
              (iterate
                (declare (type fixnum j))
                (for j from 0 below (length prev))
                (for previous = (aref prev j))
                (when (or (null previous)
                          (eq previous result))
                  (next-iteration))
                (iterate
                  (declare (type fixnum i))
                  (for i from 0
                       below (min (cl-ds.common.skip-list:skip-list-node-level previous)
                                  (cl-ds.common.skip-list:skip-list-node-level result)))
                  (for node-at = (cl-ds.common.skip-list:skip-list-node-at previous i))
                  (for rest = (cl-ds.common.skip-list:skip-list-node-at result i))
                  (when (eq node-at result)
                    (setf (cl-ds.common.skip-list:skip-list-node-at previous i)
                          rest)))))
            (values
             structure
             (cl-ds.common:make-eager-modification-operation-status
              t content t)))
          (values structure
                  cl-ds.common:empty-eager-modification-operation-status)))))


(defgeneric make-range (container current-node last-node))


(defmethod cl-ds:reset! ((container fundamental-skip-list))
  (iterate
    (with pointers = (read-pointers container))
    (for i from 0 below (length pointers))
    (setf (aref pointers i) nil))
  container)


(defmethod make-range ((container fundamental-skip-list)
                       current-node
                       last-node)
  (make-instance 'fundamental-skip-list-range
                 :last-node last-node
                 :current-node current-node))


(defmethod cl-ds:between* ((container fundamental-skip-list)
                           &key (low nil low-bound-p) (high nil high-bound-p))
  (bind (((:flet node (location boundp default))
          (if boundp
              (aref (skip-list-locate-node container
                                           location)
                    0)
              default)))
    (make-range container
                (node low low-bound-p
                      (~> container
                          cl-ds.common.skip-list:read-pointers
                          (aref 0)))
                (node high high-bound-p nil))))


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



(defun insert-or (structure location create-if-not-exists-p alter-if-exists-p &optional (value nil value-bound))
  (bind ((pointers (read-pointers structure))
         (test (read-ordering-function structure))
         ((:values current prev)
          (locate-node pointers location test))
         (result (aref current 0)))
    (when (null result)
      (unless create-if-not-exists-p
        (return-from insert-or
          (values structure cl-ds.common:empty-eager-modification-operation-status)))
      (let ((new-node (if value-bound
                          (make-skip-list-node-of-random-level (length pointers)
                           value)
                          (make-skip-list-node-of-random-level (length pointers)))))
        (setf (skip-list-node-content new-node) location)
        (insert-node-between!
         current prev
         (read-ordering-function structure)
         new-node)
        (update-head-pointers! structure new-node)
        (return-from insert-or
          (values structure
                  (cl-ds.common:make-eager-modification-operation-status
                   nil nil t)))))
    (bind ((content (skip-list-node-content result))
           (found (~> structure cl-ds.common.skip-list:access-test-function
                      (funcall content location)))
           ((:flet result-status (changed))
            (if value-bound
                (cl-ds.common:make-eager-modification-operation-status
                 found (assoc-skip-list-node-value result) changed)
                (cl-ds.common:make-eager-modification-operation-status
                 found t changed))))
      (if found
          (if alter-if-exists-p
              (let ((status (result-status t)))
                (setf (assoc-skip-list-node-value result) value)
                (values structure status))
              (values structure (result-status nil)))
          (let ((new-node (make-skip-list-node-of-random-level (array-dimension pointers 0)
                                                               value)))
            (setf (skip-list-node-content new-node)
                  location)
            (insert-node-between! current prev
                                  test new-node)
            (update-head-pointers! structure new-node)
            (values structure (result-status t)))))))
