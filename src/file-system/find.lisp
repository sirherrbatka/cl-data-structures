(cl:in-package #:cl-data-structures.file-system)


(defclass find-range (cl-ds:chunking-mixin
                      cl-ds:fundamental-forward-range)
  ((%stack :initarg :stack
           :initform nil
           :accessor access-stack)))


(defun clone-of-stack (range)
  (when-let ((stack (access-stack range)))
    (clone-cell stack)))


(defmethod cl-ds.utils:cloning-information append ((object find-range))
  '((:stack clone-of-stack)))


(defclass fundamental-file-range-stack-cell ()
  ((%prev-cell :initarg :prev-cell
               :initform nil
               :type (or null fundamental-file-range-stack-cell)
               :accessor access-prev-cell)
   (%predicate :initarg :predicate
               :type function
               :reader read-predicate
               :initform (constantly t))
   (%path :initarg :path
          :type (or nil string pathname)
          :reader read-path)))


(defclass stateful-file-range-stack-cell ()
  ((%state :initform nil
           :accessor access-state
           :type list
           :initarg :state)
   (%initial-state :initform nil
                   :accessor access-initial-state
                   :type list
                   :initarg :initial-state)))


(defclass directory-file-range-stack-cell (fundamental-file-range-stack-cell)
  ())


(defclass recursive-content-file-range-stack-cell (stateful-file-range-stack-cell
                                                   directory-file-range-stack-cell)
  ())


(defclass file-file-range-stack-cell (fundamental-file-range-stack-cell)
  ())


(defclass all-files-file-range-stack-cell (stateful-file-range-stack-cell
                                           file-file-range-stack-cell)
  ())


(defclass regex-file-file-range-stack-cell (stateful-file-range-stack-cell
                                            file-file-range-stack-cell)
  ())


(defclass regex-directory-file-range-stack-cell (stateful-file-range-stack-cell
                                                 directory-file-range-stack-cell)
  ((%times :initarg :times
           :reader read-times))
  (:default-initargs :times 1))


(defgeneric eat-cell (cell))


(defgeneric reset-cell (cell))


(defgeneric clone-cell (cell))


(defmethod (setf access-prev-cell)
    :before ((new-val all-files-file-range-stack-cell)
             (cell regex-directory-file-range-stack-cell))
  (error 'cl-ds:initialization-error
         :format-control "Directory can't be stacked on top of file in the path description."
         :class 'find-range))


(defmethod (setf access-prev-cell)
    :before ((new-val file-file-range-stack-cell)
             (cell directory-file-range-stack-cell))
  (error 'cl-ds:initialization-error
         :format-control "Directory can't be stacked on top of file in the path description."
         :class 'find-range))


(defmethod (setf access-prev-cell)
    :before ((new-val (eql nil))
             (cell regex-directory-file-range-stack-cell))
  (error 'cl-ds:initialization-error
         :format-control "Regex form can't occur as first in the path description."
         :class 'find-range))


(defmethod (setf access-prev-cell)
    :before ((new-val (eql nil))
             (cell file-file-range-stack-cell))
  (error 'cl-ds:initialization-error
         :format-control "Files form can't occur as first in the path description."
         :class 'find-range))


(defgeneric make-stack-cell (name &key &allow-other-keys))


(defmethod make-instance :before ((range regex-directory-file-range-stack-cell)
                                  &key path times &allow-other-keys)
  (check-type path string)
  (check-type times (or list positive-integer symbol))
  (if (listp times)
      (progn
        (check-type (first times) positive-integer)
        (check-type (second times) (or positive-integer symbol))
        (unless (eql (length times) 2)
          (error 'cl-ds:invalid-argument
                 :argument :times
                 :format-control "Times list should contain lower bound and upper bound."))
        (if (symbolp (second times))
            (unless (eql :recursive (second times))
              (error 'cl-ds:not-in-allowed-set
                     :value (second times)
                     :bounds '(:recursive)
                     :format-control "Upper bound is supposed to be either integer or :recursive symbol."))
            (unless (apply #'<= times)
              (error 'cl-ds:invalid-value
                     :value times
                     :format-control "Lower bound of times should be less then upper bound."))))
      (check-type times positive-integer)))


(defmethod make-instance :before ((range file-file-range-stack-cell)
                                  &key path predicate &allow-other-keys)
  (ensure-function predicate)
  (check-type path (or string pathname)))


(defmethod make-instance :before ((range directory-file-range-stack-cell)
                                  &key path &allow-other-keys)
  (check-type path (or string pathname)))


(defmethod make-instance :before ((range recursive-content-file-range-stack-cell)
                                  &key path &allow-other-keys)
  (check-type path (or string pathname)))


(defmethod make-instance :before ((range regex-file-file-range-stack-cell)
                                  &key path &allow-other-keys)
  (check-type path string))


(defmethod make-stack-cell ((name (eql :directory)) &key path (predicate (constantly t)))
  (make 'directory-file-range-stack-cell
        :predicate (ensure-function predicate)
        :path path))


(defmethod make-stack-cell ((name (eql :regex-directory)) &key path (times 1) (predicate (constantly t)))
  (make 'regex-directory-file-range-stack-cell
        :predicate (ensure-function predicate)
        :path (cl-ppcre:create-scanner path)
        :times times))


(defmethod make-stack-cell ((name (eql :regex-file)) &key path (predicate (constantly t)))
  (make 'regex-file-file-range-stack-cell
        :predicate (ensure-function predicate)
        :path (cl-ppcre:create-scanner path)))


(defmethod make-stack-cell ((name (eql :file)) &key path (predicate (constantly t)))
  (make 'file-file-range-stack-cell
        :predicate (ensure-function predicate)
        :path path))


(defmethod make-stack-cell ((name (eql :all-directories)) &key (path "") (predicate (constantly t)))
  (make 'recursive-content-file-range-stack-cell
        :predicate (ensure-function predicate)
        :path path))


(defmethod make-stack-cell ((name (eql :all-files)) &key (predicate (constantly t)))
  (make 'all-files-file-range-stack-cell
        :predicate (ensure-function predicate)
        :path nil))


(defun to-pathname (x &rest args)
  (etypecase x
    (string (apply #'make-pathname args))
    (pathname x)))


(defmethod eat-cell ((cell directory-file-range-stack-cell))
  (tagbody :start
     (if (~> cell access-prev-cell null)
         (let* ((path (read-path cell)))
           (setf (slot-value cell '%path) nil)
           (if (and (not (null path))
                    (setf path (to-pathname path :directory path))
                    (uiop:directory-exists-p path)
                    (~> cell read-predicate (funcall path)))
               (return-from eat-cell (values path t))
               (return-from eat-cell (values nil nil))))
         (let ((prev-path (~> cell access-prev-cell eat-cell)))
           (if (null prev-path)
               (return-from eat-cell (values nil nil))
               (let ((result (merge-pathnames (read-path cell) prev-path)))
                 (if (and (uiop:directory-exists-p result)
                          (~> cell read-predicate (funcall result)))
                     (return-from eat-cell (values result t))
                     (go :start))))))))


(defmethod eat-cell ((cell recursive-content-file-range-stack-cell))
  (tagbody :start
     (when (~> cell access-state first (eql :end))
       (return-from eat-cell
         (values nil nil)))
     (bind ((path (read-path cell)))
       (if (null (access-state cell))
           (let ((prev-path nil))
             (if (~> cell access-prev-cell null)
                 (progn
                   (setf prev-path (to-pathname path :directory path))
                   (unless (uiop:directory-exists-p prev-path)
                     (push :end (access-state cell))
                     (return-from eat-cell
                       (values nil nil)))
                   (setf (access-state cell) (list prev-path))
                   (go :start))
                 (if-let ((p (~>> cell access-prev-cell eat-cell)))
                   (progn
                     (setf prev-path (merge-pathnames path p))
                     (when (uiop:directory-exists-p prev-path)
                       (setf (access-state cell) (list prev-path)))
                     (go :start))
                   (progn
                     (push :end (access-state cell))
                     (return-from eat-cell
                       (values nil nil))))))
           (iterate
             (until (or (null (access-state cell))
                        (~> cell access-state first (eql :end))))
             (for next-path = (pop (access-state cell)))
             (for directory-content = (~>> next-path directory-content
                                           (delete-if-not #'uiop:directory-exists-p)))
             (setf (access-state cell)
                   (cl-ds.utils:add-to-list directory-content
                                            (access-state cell)))
             (when (and (~> cell access-state endp)
                        (null (access-prev-cell cell)))
               (push :end (access-state cell)))
             (when (~> cell read-predicate (funcall next-path))
               (return-from eat-cell
                 (values next-path t)))
             (finally (if (null (access-prev-cell cell))
                          (progn
                            (push :end (access-state cell))
                            (return-from eat-cell
                              (values nil nil)))
                          (go :start))))))))


(defmethod eat-cell ((cell all-files-file-range-stack-cell))
  (tagbody :start
     (bind ((state (access-state cell)))
       (if (endp state)
           (let ((prev-path (~> cell access-prev-cell eat-cell)))
             (if (null prev-path)
                 (return-from eat-cell
                   (values nil nil))
                 (let* ((directory-content
                          (~>> prev-path directory-content
                               (delete-if-not (alexandria:conjoin
                                               (read-predicate cell)
                                               #'uiop:file-exists-p)))))
                   (setf (access-state cell) directory-content)
                   (go :start))))
           (iterate
             (until (endp (access-state cell)))
             (for next-path = (pop (access-state cell)))
             (return-from eat-cell
               (values next-path t))
             (finally (go :start)))))))


(defmethod eat-cell ((cell regex-file-file-range-stack-cell))
  (tagbody :start
     (bind ((path (read-path cell))
            (state (access-state cell)))
       (if (endp state)
           (let ((prev-path (~> cell access-prev-cell eat-cell)))
             (if (null prev-path)
                 (return-from eat-cell
                   (values nil nil))
                 (let* ((directory-content
                          (~>> prev-path directory-content
                               (delete-if-not #'uiop:file-exists-p))))
                   (setf (access-state cell) directory-content)
                   (go :start))))
           (iterate
             (until (endp (access-state cell)))
             (for next-path = (pop (access-state cell)))
             (when (and (regex-matches path next-path)
                        (~> cell read-predicate (funcall next-path)))
               (return-from eat-cell
                 (values next-path t)))
             (finally (go :start)))))))


(defun directory-content (directory)
  (append (uiop:directory-files directory)
          (uiop:subdirectories directory)))


(defun unmerge-pathnames (pathspec default)
  "Removes those leading directory components from PATHSPEC that
are shared with DEFAULT."
  (let* ((dir (pathname-directory pathspec))
         (mismatch (mismatch dir
                             (pathname-directory default)
                             :test #'equal)))
    (make-pathname :directory (when mismatch
                                (cons :relative (subseq dir mismatch)))
                   :defaults pathspec)))


(defun directory-regex-matches (regex path parent)
  (~> (unmerge-pathnames path parent)
      namestring
      (cl-ppcre:scan regex _)
      not
      null))


(defun regex-matches (regex path)
  (~>> (make-pathname :name (pathname-name path)
                      :type (pathname-type path))
       namestring
       (cl-ppcre:scan regex)
       not
       null))


(defun not-greater (times depth)
  (if (listp times)
      (or (eql (second times) :recursive)
          (<= depth (second times)))
      (if (eql times :recursive)
          t
          (<= depth times))))


(defun times-matches (times depth)
  (if (listp times)
      (and (>= depth (first times))
           (or (eql (second times) :recursive)
               (< depth (second times))))
      (if (eql times :recursive)
          t
          (= depth times))))


(defmethod eat-cell ((cell regex-directory-file-range-stack-cell))
  (tagbody :start
     (bind ((path (read-path cell))
            (state (access-state cell))
            (times (read-times cell)))
       (if (endp state)
           (let ((prev-path (~> cell access-prev-cell eat-cell)))
             (if (null prev-path)
                 (return-from eat-cell
                   (values nil nil))
                 (let* ((directory-content (~>> prev-path directory-content
                                                (delete-if-not #'uiop:directory-exists-p)))
                        (new-state (mapcar (curry #'list 1 prev-path) directory-content)))
                   (setf (access-state cell) new-state)
                   (go :start))))
           (iterate
             (until (endp (access-state cell)))
             (for (depth prev-path next-path) = (pop (access-state cell)))
             (when (not-greater times depth)
               (let ((directory-content (~>> next-path directory-content
                                             (delete-if-not #'uiop:directory-exists-p))))
                 (setf (access-state cell)
                       (cl-ds.utils:add-to-list (mapcar (curry #'list (1+ depth) prev-path)
                                                        directory-content)
                                                (access-state cell)))
                 (when (and (times-matches times depth)
                            (~> cell read-predicate (funcall next-path))
                            (directory-regex-matches path next-path prev-path))
                   (return-from eat-cell
                     (values next-path t)))))
             (finally (go :start)))))))


(defmethod eat-cell ((cell file-file-range-stack-cell))
  (tagbody :start
     (bind ((path (read-path cell))
            (inner (~> cell access-prev-cell eat-cell)))
       (if (null inner)
           (values nil nil)
           (let ((next-path (merge-pathnames path inner)))
             (if (and (uiop:file-exists-p next-path :regular-file)
                      (~> cell read-predicate (funcall next-path)))
                 (return-from eat-cell (values next-path t))
                 (go :start)))))))


(defgeneric find (description)
  (:method ((description list))
    (let ((result nil))
      (iterate
        (for spec in description)
        (for stack-cell = (apply #'make-stack-cell spec))
        (setf (access-prev-cell stack-cell) result
              result stack-cell))
      (make 'find-range
            :stack result))))


(defmethod cl-ds:consume-front ((range find-range))
  (if-let ((stack (access-stack range)))
    (eat-cell stack)
    (values nil nil)))


(defmethod clone-cell :around ((cell stateful-file-range-stack-cell))
  (lret ((result (call-next-method)))
    (setf (access-state result) (access-state cell)
          (access-initial-state result) (access-state cell))))


(defmethod clone-cell ((cell regex-directory-file-range-stack-cell))
  (lret ((result (call-next-method)))
    (setf (slot-value result '%times) (read-times cell))))


(defmethod clone-cell ((cell fundamental-file-range-stack-cell))
  (make (type-of cell)
        :prev-cell (and #1=(access-prev-cell cell) (clone-cell #1#))
        :predicate (read-predicate cell)
        :path (read-path cell)))


(defmethod cl-ds:peek-front ((range find-range))
  (cl-ds:consume-front (cl-ds:clone range)))


(defmethod cl-ds:clone ((range find-range))
  (cl-ds.utils:clone range))


(defmethod cl-ds:reset! ((range find-range))
  (when-let ((stack (access-stack range)))
    (reset-cell stack))
  range)


(defmethod reset-cell ((cell stateful-file-range-stack-cell))
  (setf (access-state cell) (access-initial-state cell))
  (call-next-method))


(defmethod reset-cell ((cell fundamental-file-range-stack-cell))
  (when-let ((stack (access-prev-cell cell)))
    (reset-cell stack)))
