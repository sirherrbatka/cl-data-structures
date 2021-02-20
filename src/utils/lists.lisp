(cl:in-package #:cl-data-structures.utils)


(-> insert-or-replace (list t &key
                            (:test (-> (t t) boolean))
                            (:list-key (-> (t) t))
                            (:item-key (-> (t) t))
                            (:preserve-order boolean))
    (values list boolean t))
(declaim (inline insert-or-replace))
(defun insert-or-replace (list element &key
                                         (test #'eql)
                                         (list-key #'identity)
                                         (item-key #'identity)
                                         (preserve-order nil))
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  "Insert element into set if it is not already here.

   @b(Returns three values:)
   @begin(list)
    @item(first -- new list)
    @item(second -- was any item replaced?)
    @item(third -- old value that was replaced (or nil if there was no such value))
   @end(list)"
  (iterate
    (with last-cell = nil)
    (with result = nil)
    (with replaced = nil)
    (with value = nil)
    (for sublist on list)
    (for elt = (car sublist))
    (if (funcall test (funcall list-key elt) (funcall item-key element))
        (progn
          (push element result)
          (setf replaced t
                value elt))
        (push elt result))
    (unless last-cell
      (setf last-cell result))
    (when (and replaced last-cell (not preserve-order))
      (setf (cdr last-cell)
            (cdr sublist))
      (finish))
    (finally (return (values (let ((r (if preserve-order
                                          (nreverse result)
                                          result)))
                               (if replaced
                                   r
                                   (cons element
                                         r)))
                             replaced
                             value)))))


(-> try-remove (t list &key
                  (:test (-> (t t) boolean))
                  (:key (-> (t) t))
                  (:preserve-order boolean))
    (values list boolean t))
(declaim (inline try-remove))
(defun try-remove (item list &key (test #'eql) (key #'identity) (preserve-order nil))
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  "Try to remove first item matching from the list.

   @b(Returns three values:)
   @begin(list)
    @item(first -- new list)
    @item(second -- did anything was removed?)
    @item(third -- value that was removed (or nil if nothing was removed))
   @end(list)"
  (iterate
    (for sublist on list)
    (for elt = (car sublist))
    (with removed = nil)
    (with value = nil)
    (with last-cell = nil)
    (if (funcall test
                 (funcall key elt)
                 item)
        (setf removed t
              value elt)
        (collect elt into result at start))
    (unless last-cell
      (setf last-cell result))
    (when (and removed last-cell (not preserve-order))
      (setf (cdr last-cell) (cdr sublist))
      (finish))
    (finally (return (values (if preserve-order
                                 (reverse result)
                                 result)
                             removed
                             value)))))


(-> try-find-cell (t list &key (:test (-> (t t) boolean)) (:key (-> (t) t))) list)
(defun try-find-cell (item list &key (test #'eql) (key #'identity))
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  "@b(Returns) first matching sublist"
  (iterate
    (for elt on list)
    (when (funcall test
                   (funcall key (car elt))
                   item)
      (leave elt))))


(-> try-find (t list &key (:test (-> (t t) boolean)) (:key (-> (t) t))) (values t boolean))
(defun try-find (item list &key (test #'eql) (key #'identity))
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  "@b(Returns) first matching elements as first value and boolean telling if it was found as second"
  (let ((r (try-find-cell item list :test test :key key)))
    (values (car r)
            (not (null r)))))


(defun lexicographic-compare (compare same av bv &key (key #'identity))
  (setf compare (alexandria:ensure-function compare))
  (setf same (alexandria:ensure-function same))
  (check-type av sequence)
  (check-type bv sequence)
  (check-type compare function)
  (check-type same function)
  (iterate
    (for ea1 in-sequence av)
    (for eb1 in-sequence bv)
    (for ea = (funcall key ea1))
    (for eb = (funcall key eb1))
    (for sm = (funcall same ea eb))
    (for comp = (funcall compare ea eb))
    (finding comp such-that comp into r)
    (always sm)
    (finally
     (if r
         (return r)
         (when sm
           (return (< (length av) (length bv))))))))


(defun add-to-list (list data &optional (function #'identity))
  (reduce (flip #'cons)
          data
          :initial-value list
          :key function))


(defun normalize-sequence-to-sum (sequence sum)
  (declare (type sequence sequence)
           (type number sum))
  (let* ((current-sum (reduce #'+ sequence))
         (ratio (/ sum current-sum)))
    (map-into sequence (curry #'* ratio) sequence)))


(defun normalize-sequence-to-span (sequence min max)
  (declare (type sequence sequence)
           (type number min max))
  (assert (< min max))
  (let* ((current-min (reduce #'min sequence))
         (current-max (reduce #'max sequence))
         (current-span (- current-max current-min))
         (diff (- 0 current-min))
         (ratio (/ (- max min) current-span)))
    (map-into sequence
              (compose (curry #'+ min)
                       (curry #'* ratio)
                       (curry #'+ diff))
              sequence)))


(defun scan (function sequence
             &rest args
             &key key from-end (start 0) initial-value)
  (declare (ignore key start initial-value from-end))
  (let* ((result '())
         (final (apply #'reduce
                       (lambda (&rest all)
                         (lret ((r (apply function all)))
                           (push r result)))
                       sequence
                       args)))
    (values (nreverse result)
            final)))


(defun homogenousp (sequence &key (test 'eql) (key 'identity))
  (if (emptyp sequence)
      t
      (progn
        (reduce (lambda (&optional (prev nil prev-bound) (next nil next-bound))
                  (if next-bound
                      (if (funcall test
                                   (funcall key prev)
                                   (funcall key next))
                          next
                          (return-from homogenousp nil))
                      (return-from homogenousp t)))
                sequence)
        t)))

(declaim (inline transform))
(defun transform (function sequence &rest more-sequences)
  (apply #'map-into sequence function sequence
         more-sequences))
