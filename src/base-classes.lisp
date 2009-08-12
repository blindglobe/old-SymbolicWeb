;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :base-classes.lisp))


#| These are placed here so the compiler will know about them as early as
possible and be able to optimize type-checking code based on this. |#



;;; widget-base.lisp
;;;;;;;;;;;;;;;;;;;;

(defclass widget-base (#|object|# id-mixin self-ref view-base dom-mirror)
  ())
(export '(widget-base id id-of))



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
