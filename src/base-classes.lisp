;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations))


#| These are placed here so the compiler will know about them as early as
possible and be able to optimize type-checking code based on this. |#



;;; widget-base.lisp
;;;;;;;;;;;;;;;;;;;;

(defclass widget-base (id-mixin object self-ref view-base dom-mirror)
  ())
(export '(widget-base))



;;; container-base.lisp
;;;;;;;;;;;;;;;;;;;;;;;

(defclass container-base ()
  ((children :reader children-of
             :type list
             :initform nil)))
(export '(container-base children children-of))
