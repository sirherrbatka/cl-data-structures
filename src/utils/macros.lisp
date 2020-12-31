(cl:in-package #:cl-data-structures.utils)


(defmacro lazy-let (bindings &body body)
  "Like LET but bind variable only once it is accessed. Does that by replacing calls to variable by calls to local function so variables are not seen when introspecting lexical enviorement."
  (flet ((gensym-list (x) (list (car x) (gensym) (cadr x) (caddr x) (gensym)))
         (make-let-list (x) `(,(cadr x) ',(cadr x)))
         (make-macro-list (x) (destructuring-bind (symbol var form rec init) x
                                (declare (ignore var form rec))
                                `(,symbol (,init))))
         (make-set-list (x) (destructuring-bind (symbol var form rec init) x
                              (declare (ignore form symbol rec))
                              `((setf ,init) (new-value)
                                (setf ,var new-value))))
         (make-init-list (x) (destructuring-bind (symbol var form rec init) x
                               `(,init ()
                                       ,(if rec
                                            `(progn (when (eq ,var ',var)
                                                      (let ((,symbol ,rec))
                                                        (setf ,var (nlet >>> (,symbol) ,form))))
                                                    ,var)
                                            `(progn (when (eq ,var ',var)
                                                      (setf ,var ,form))
                                                    ,var))))))
    (let* ((gensym-list (mapcar #'gensym-list bindings))
           (functions (mapcar (rcurry #'elt 4) gensym-list)))
      `(let ,(mapcar #'make-let-list gensym-list)
         (symbol-macrolet ,(mapcar #'make-macro-list gensym-list)
           (labels (,@(mapcar #'make-init-list gensym-list)
                    ,@(mapcar #'make-set-list gensym-list))
             (declare (ignorable ,@(mapcar (lambda (x) `(function ,x))
                                           functions)
                                 ,@(mapcar (lambda (x) `(function (setf ,x)))
                                           functions))
                      (inline ,@functions
                              ,@(mapcar (lambda (x) `(setf ,x))
                                        functions)))
             ,@body))))))


(defmacro bind-lambda (fn &rest args)
  "Curry function. For instance (bin-lambda #'+ 2 :_) is like (curry #+ 2). However this can be used to bind more arguments. For instance (bind-lambda #'/ 5 :_ 3 :_ 4 :_ 2 :_ :_ 12)"
  (let* ((args-count 0)
         (fargs nil)
         (binded nil)
         (funcall-args (mapcar (lambda (x)
                                 (if (eq :_ x)
                                     (let ((arg (intern (format nil "ARG~a" (incf args-count)))))
                                       (push arg fargs)
                                       arg)
                                     (let ((symbol (gensym)))
                                       (push (list symbol x) binded)
                                       symbol)))
                               args)))
    `(let ,(reverse binded)
       (lambda ,(reverse fargs)
         (funcall ,fn ,@funcall-args)))))


(defmacro with-vectors (vector-bindings &body body)
  "Macro for simpler vector usage. Will expand into local functions that act as accessors for vectors so you will not have to write stuff like (aref vector 0), instead you can write (vector 0)"
  (let ((vector-bindings (if (symbolp vector-bindings)
                             (list vector-bindings)
                             vector-bindings)))
    (with-gensyms (!index !value)
      (flet ((get-f-name (x)
               (if (symbolp x)
                   x
                   (car x)))
             (get-let-forms (x)
               (if (symbolp x)
                   (list x x)
                   x)))
        (flet ((make-aref-list (x)
                 `(,x (,!index) (aref ,x ,!index)))
               (make-setf-list (x)
                 `((setf ,x)  (,!value ,!index)
                   (setf (aref ,x ,!index) ,!value))))
          (let ((functions (mapcar #'get-f-name vector-bindings)))
            `(let* ,(mapcar #'get-let-forms vector-bindings)
               (labels (,@(mapcar #'make-aref-list functions)
                        ,@(mapcar #'make-setf-list functions))
                 (declare (ignorable ,@(mapcar (lambda (x) `(function ,x))
                                               functions)
                                     ,@(mapcar (lambda (x) `(function (setf ,x)))
                                               functions))
                          (dynamic-extent ,@(mapcar (lambda (x) `(function ,x))
                                                    functions)
                                          ,@(mapcar (lambda (x) `(function (setf ,x)))
                                                    functions))
                          (inline ,@functions
                                  ,@(mapcar (lambda (x) `(setf ,x))
                                            functions)))
                 ,@body))))))))


(eval-always
  (defun generate-if-else (conditions forms)
    (flet ((without-test (x)
             (destructuring-bind (tests . form) x
               (list* (cdr tests) form)))
           (check-test (x)
             (destructuring-bind ((b . w) . form) x
               (declare (ignore w form))
               b)))
      (if conditions
          (list 'if (car conditions)
                (if-let ((r (mapcar #'without-test
                                    (remove-if (compose #'not #'check-test) forms))))
                  (generate-if-else (cdr conditions) r)
                  '(error "Unhalded case!"))
                (if-let ((r (mapcar #'without-test
                                    (remove-if #'check-test forms))))
                  (generate-if-else (cdr conditions) r)
                  '(error "Unhalded case!")))
          (cons 'progn (first (mapcar #'rest forms))))))

  (defun every-possible-combination (count)
    (assert (>= count 0))
    (labels ((impl (count ac)
               (if (zerop count)
                   ac
                   (append (impl (1- count)
                                 (mapcar (curry #'cons t)
                                         ac))
                           (impl (1- count)
                                 (mapcar (curry #'cons nil)
                                         ac))))))
      (impl count '(nil)))))


(defmacro cond+ (tests &body forms)
  "If else ladder generator.

  @b(Values and arguments:)
  @begin(list)
  @item(tests -- list of conditions)
  @item(forms -- list of clauses that are supposed to be checked against conditions)
  @end(list)"
  (generate-if-else tests forms))

(defmacro cases (all-cases &body form)
  "Macro, used for elemination of code branches by duplicating code."
  (bind ((tests (remove :variant all-cases
                        :key (lambda (x) (when (listp x) (first x)))))
         (count (length tests))
         (variants (~>> (remove :variant all-cases
                                :key (lambda (x)
                                       (when (listp x)
                                         (first x)))
                                :test-not #'eql)
                        (mapcar #'rest)
                        (mapcar (lambda (x) (append x '(t))))))
         ((:labels expand-variants (&optional (var variants)))
          (if (endp var)
              `(progn ,@form)
              `(cond ,@(mapcar (lambda (x)
                                 `(,x ,(expand-variants (rest var))))
                               (first var))))))
    (if (null tests)
        (expand-variants)
        `(cond+ ,tests
           ,@(mapcar (lambda (x) `(,x ,(expand-variants)))
                     (every-possible-combination count))))))


(defmacro cond-compare ((a b) < = >)
  (once-only (a b)
    `(cond ((< ,a ,b) ,<)
           ((> ,a ,b) ,>)
           ((= ,a ,b) ,=))))


(defmacro import-all-package-symbols (from-package to-package)
  "Macro. Will import all internal symbols from package to package. For tests."
  (let ((from-package (find-package from-package))
        (to-package (find-package to-package)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (do-symbols (symbol ,from-package)
         (when (nth-value 1 (find-symbol (string-upcase symbol) ,from-package))
           (import symbol ,to-package))))))


(eval-always
  (defun build-setf-form (vars args)
    (apply #'append
           (iterate
             (with p-fake-keys = (serapeum:plist-keys args))
             (with p-fake-values = (serapeum:plist-values args))
             (for key in p-fake-keys)
             (for value in p-fake-values)
             (for fake = (cdr (find (make-keyword key) vars
                                    :key #'car
                                    :test #'string=)))
             (collect (list fake value))))))


(defmacro let-generator (forms send-finish finish recur send-recur
                         &body body)
  "Poor man's generator (no continuations, no code-walking)."
  (with-gensyms (!end !result !self !finished)
    (let ((final-forms nil))
      (iterate
        (for (name vars . content) in forms)
        (for vars-length = (length vars))
        (for fake-vars = (iterate (repeat vars-length) (collect (gensym))))
        (for vars-asg = (mapcar #'list* vars fake-vars))
        (push `(,name (&key ,@vars)
                      (let ((,!finished nil))
                        (let ,(mapcar #'list fake-vars vars)
                          (values
                           (lambda ()
                             (block ,!end
                               (macrolet ((,send-finish (&body ,!result)
                                            `(progn
                                               (setf ,',!finished t)
                                               (return-from ,',!end
                                                 (values (progn ,@,!result)
                                                         t))))
                                          (,finish ()
                                            `(progn
                                               (setf ,',!finished t)
                                               (return-from ,',!end
                                                 (values nil nil))))
                                          (,recur (&rest args)
                                            `(progn
                                               (setf ,@(build-setf-form ',vars-asg args))
                                               (go ,',!self)))
                                          (,send-recur (,!result &rest args)
                                            `(return-from ,',!end
                                               (values (prog1
                                                           ,,!result
                                                         (setf ,@(build-setf-form ',vars-asg args)))
                                                       t))))
                                 (tagbody
                                    ,!self
                                    (when ,!finished
                                      (return-from ,!end (values nil nil)))
                                    (let ,(mapcar #'list vars fake-vars)
                                      ,@content
                                      (setf ,!finished t)
                                      (return-from ,!end (values nil nil)))))))
                           (lambda ()
                             (list ,@(apply #'append
                                            (mapcar (lambda (name binding) (list (make-keyword name) binding))
                                                    vars
                                                    fake-vars))))
                           (lambda ()
                             ,!finished)))))
              final-forms))
      `(flet ,final-forms
         ,@body))))


(defmacro optimize-value ((&rest bindings) &body body)
  (let* ((values (mapcar #'first bindings))
         (initial-value-present (mapcar #'cddr bindings))
         (first-times (map-into (make-list (length bindings)) #'gensym))
         (first-times-values (mapcar (lambda (first-symbol value)
                                       (list first-symbol (null value)))
                                     first-times
                                     initial-value-present))
         (initial-values (mapcar (lambda (value-symbol value-present)
                                   (list value-symbol
                                         (when value-present
                                           (car value-present))))
                                 values initial-value-present))
         (calls (mapcar #'first bindings))
         (optimizers (mapcar #'second bindings))
         (!val (gensym))
         (functions (mapcar (lambda (first-time value optimize call)
                              `(,call (,!val)
                                      (when (or ,first-time
                                                (,optimize ,!val ,value))
                                        (setf ,value ,!val
                                              ,first-time nil))
                                      ,value))
                            first-times
                            values
                            optimizers
                            calls)))
    `(let (,@initial-values ,@first-times-values)
       (flet ,functions
         (progn
           ,@body
           (values ,@values))))))


(defmacro macro-with-dynamic-vars (bindings body)
  `(let* ((original-hook *macroexpand-hook*)
          (*macroexpand-hook*
            (lambda (expander form env)
              (let ,bindings
                (funcall original-hook expander form env)))))
     (macroexpand ',body)))


(defmacro toggle-vars (bindings &body body)
  (let ((!vars (mapcar (lambda (x)
                         (declare (ignore x))
                         (gensym))
                       bindings)))
    `(progn
       (let ,(mapcar (lambda (binding symbol)
                       (list binding (car symbol)))
                     !vars bindings)
         (eval-always
           (setf ,@(apply #'append bindings)))
         (unwind-protect
              (progn ,@body)
           (eval-always
             (setf ,@(apply #'append (mapcar (lambda (binding symbol)
                                               (list (car symbol) binding))
                                             !vars bindings)))))))))


(eval-always
  (defgeneric list-of-slots (symbol)))


(defmacro define-list-of-slots (id (&rest inherited) &body slots)
  `(eval-always
     (defmethod list-of-slots ((class (eql ',id)))
       '(,@slots
         ,@(iterate
             (with result = '())
             (for i in (reverse inherited))
             (iterate
               (for slot in (list-of-slots i))
               (push slot result))
             (finally (return (nreverse result))))))))


(defmacro with-slots-for ((object id) &body body)
  (let* ((slots (list-of-slots id)))
    `(with-accessors ,slots ,object ,@body)))


(defmacro with-rebind (variables &body body)
  (let* ((gensym-list (mapcar (lambda (x) (gensym (symbol-name x))) variables))
         (let1 (mapcar #'list gensym-list variables))
         (let2 (mapcar #'list variables gensym-list)))
    `(let ,let1
       (macrolet ((rebind (&body body)
                    `(let ,',let2
                       ,@body)))
         ,@body))))


(defmacro lolol (variables &body body)
  (let* ((let-form (mapcar #'list variables variables)))
    `(let ,let-form
       ,@body)))


(defmacro lparallel-future (variables &body body)
  `(with-rebind ,variables
     (lparallel:future (rebind ,@body))))


(defmacro copy-slots ((from to) &body slot-names)
  `(progn ,@(mapcar (lambda (slot)
                      `(setf (slot-value ,to ',slot)
                             (slot-value ,from ',slot)))
                    slot-names)))


(defmacro with-keys (forms &body body)
  (let ((key-symbols (mapcar (lambda (x)
                               (declare (ignore x))
                               (gensym))
                             forms)))
    `(let ,(mapcar (lambda (key form) (list key (third form)))
                   key-symbols forms)
       (symbol-macrolet ,(mapcar (lambda (form key)
                                   `(,(first form)
                                     (funcall ,key ,(second form))))
                          forms
                          key-symbols)
             ,@body))))
