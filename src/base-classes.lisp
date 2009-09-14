;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :base-classes.lisp))


#| These are placed here so the compiler will know about them as early as
possible and be able to optimize type-checking code based on this. |#



;;; widget-base.lisp
;;;;;;;;;;;;;;;;;;;;

(defclass widget-base (id-mixin self-ref view-base dom-mirror)
  ())
(export 'widget-base)



;;; container-base.lisp
;;;;;;;;;;;;;;;;;;;;;;;

(defclass container-base ()
  ((children :reader children-of
             :type list
             :initform nil
             :documentation "
Contains View instances. Note that this is also the only hard link to the View part of
a Model <-> View relationship.")))
(export '(container-base children children-of))
