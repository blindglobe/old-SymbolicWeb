;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :base-classes.lisp))


#| These are placed here so the compiler will know about them as early as
possible and be able to optimize type-checking code based on this. |#



;;; widget-base.lisp
;;;;;;;;;;;;;;;;;;;;

(defclass widget-base (#|object|# self-ref view-base dom-mirror)
  ((id :reader id-of
       :type string)))
(export '(widget-base id id-of))


(defmethod initialize-instance :before ((widget widget-base) &key (id nil id-supplied-p))
  (declare (optimize speed))
  (setf (slot-value widget 'id)
        (if id-supplied-p
            id
            (catstr (string (type-of widget)) "-" (id-generator-next-str -id-generator-)))))




;;; container-base.lisp
;;;;;;;;;;;;;;;;;;;;;;;

(defclass container-base ()
  ((children :reader children-of
             :type list
             :initform nil
             :documentation "
Contains View instances.")))
(export '(container-base children children-of))


(defmethod initialize-instance :after ((container-base container-base) &key
                                       (children nil children-supplied-p)
                                       (child nil child-supplied-p))
  (with-object container-base
    (setf ¤children
          (loop :for child :in (mklst (cond
                                        (children-supplied-p children)
                                        (child-supplied-p child)))
             :collect (if (typep child 'view-base)
                          child
                          (view-in-context-of container-base child))))))
