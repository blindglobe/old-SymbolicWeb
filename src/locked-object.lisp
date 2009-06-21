;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :locked-object.lisp))


(defclass locked-object-group ()
  ((lock :reader lock-of
         :initform (make-lock))
   
   (objects-lock :reader objects-lock-of
                 :initform (make-lock))
   
   (objects :type list
            :initform nil))
  
  (:documentation "
When you need to lock multiple objects at the _same time_
\(locking them in sequential order of execution == fail)."))
(export '(locked-object-group lock-of))


(defmethod objects-of ((locked-object-group locked-object-group))
  (with-lock-held ((objects-lock-of locked-object-group))
    (slot-value locked-object-group 'objects)))
(export 'objects-of)


(defmethod (setf objects-of) (new-objects (locked-object-group locked-object-group))
  (with-lock-held ((objects-lock-of locked-object-group))
    (setf (slot-value locked-object-group 'objects) new-objects)))
(export 'objects-of)
