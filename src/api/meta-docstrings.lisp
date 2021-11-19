(cl:in-package #:cl-data-structures.meta)


(docs:define-docs
  :formatter docs.ext:rich-aggregating-formatter

  (function position-modification
    (:syntax "position-modification operation container location more-args => container status"
     :arguments  (("OPERATION" "Instance of API function.")
                  ("CONTAINER" "Instance of container class")
                  ("LOCATION" "Where modification is supposed to happen?"))
     :returns ("Container (new or the same instance)"
               "Modification status")
     :description "A low level function used as de facto implementation point of all API modification functions (INSERT, ADD, UPDATE)."
     :notes "Implementations of this generic function are multimethods dispatched on the class of the OPERATION and on the CONTAINER."))

  (function functional-counterpart
    (:syntax "functional-counterpart operation => functional-operation"
     :arguments (("OPERATION" "Instance of the modification API function."))
     :description "Low level function that returns an instance of the functional api modification function that serves the same purpose as the OPERATION. Will, for instance, return #'UPDATE when OPERATION is #'UPDATE! Will return original the OPERATION if OPERATION is already part of the functional API."
     :returns "The instance of api function."
     :notes "This function is low level, and therefore should be rarely (if ever) used by the user of this library."))

  (function destructive-counterpart
    (:syntax "destructive-counterpart operation => destructive-operation"
     :arguments (("OPERATION" "Instance of the modification API function."))
     :description "Low level function that returns an instance of the destrutive api modification function that serves the same purpose as the OPERATION. Will, for instance, return #'UPDATE! when OPERATION is #'UPDATE Will return original the OPERATION if OPERATION is already part of the functional API."
     :returns "The instance of api function."
     :notes "This function is low level, and therefore should be rarely (if ever) used by the user of this library."))

  (function grow-bucket
    (:description "Generic, low level function used to grow all sorts of buckets (like those in dictionaries). Buckets are created by make-bucket function."
     :returns ("Bucket."
               "Instance of modification-operation-status.")
     :notes "This function is not allowed to perform any side effects. See grow-bucket! for function that is allowed to do so."))

  (function shrink-bucket
    (:description "Generic, low level function used to shrink all sorts of buckets (like those in dictionaries). Buckets are created by make-bucket function."
     :returns ("Bucket. May also return nil to indicate empty bucket."
               "Instance of modification-operation-status.")
     :notes "This function is not allowed to perform any side effects. See shrink-bucket! for function that is allowed to do so."))

  (function shrink-bucket!
    (:description "Generic, low level function used to shrink all sorts of buckets (like those in dictionaries). Buckets are created by make-bucket function. This is destructive function."
     :returns ("Bucket. May also return nil to indicate empty bucket."
               "Instance of modification-operation-status.")))

  (function make-bucket
    (:description "Generic, low level function used to create all sorts of buckets (like those in dictionaries). Buckets are modified by grow-bucket and shrink-bucket functions."
     :returns "New bucket. Precise type of bucket is not relevant to this level of abstraction and usually is defined in context of specific container."
     :notes "This function is not allowed to perform any side effects."))

  (function grow-bucket!
    (:description "Generic, low level function used to destructivly grow all sorts of buckets (like those in dictionaries). Buckets are created by make-bucket function."
     :returns ("New or passed bucket."
               "Instance of position modification status.")
     :side-effects "Can modify bucket."))

  (function make-bucket
    (:returns ("New or passed bucket. May return nil as empty bucket."
               "Instance of position modification status.")
     :side-effects "Can modify bucket."
     :description "Constructs abstract bucket."))

  (type functional-function
    (:description "Function that inherits this class is not allowed to perform any side effects."))

  (type destructive-function
    (:description "Function that inherits this class is expected to perform side effects."))

  (type grow-function
    (:description "Function will attempt to add new element to container."))

  (type shrink-function
    (:description "Function will attempt to remove element from container."))

  (type insert-function
    (:description "Class of INSERT and (SETF AT)."))

  (type update-function
    (:description "Class of UPDATE and UPDATE!."))

  (type add-function
    (:description "Class of ADD and ADD!."))

  (type erase-function
    (:description "Class of ERASE and ERASE!."))

  (type erase-if-function
    (:description "Class of ERASE-IF and ERASE-IF!."))

  (type update-if-function
    (:description "Class of UPDATE-IF and UPDATE-IF!."))

  (type functional-insert-function
    (:description "Class of INSERT."))

  (type functional-update-function
    (:description "Class of UPDATE."))

  (type functional-add-function
    (:description "Class of ADD."))

  (type functional-erase-function
    (:description "Class of ERASE."))

  (type functional-update-if-function
    (:description "Class of UPDATE-IF."))

  (type insert!-function
    (:description "Class of INSERT."))

  (type update!-function
    (:description "Class of UPDATE."))

  (type add!-function
    (:description "Class of ADD."))

  (type erase!-function
    (:description "Class of ERASE."))

  (type update-if!-function
    (:description "Class of UPDATE-IF!."))

  (type erase-if!-function
    (:description "Class of ERASE-IF!."))

  (type functional-erase-if-function
    (:description "Class of ERASE-IF."))

  (type erase-if-function
    (:description "Class of ERASE-IF and ERASE-IF!.")))
