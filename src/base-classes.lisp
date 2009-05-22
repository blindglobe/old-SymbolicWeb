;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


#| These are placed here so the compiler will know about them as early as
possible and be able to optimize type-checking code based on this. |#



;;; widget-base.lisp
;;;;;;;;;;;;;;;;;;;;

(defclass widget-base (id-mixin object view-base)
  ()
  
  (:metaclass mvc-stm-class))
(export '(widget-base))


;; These methods ensure that dataflow from Models to Views (or widgets) do not
;; happen unless the widgets are visible.
;; TODO: I have a feeling this is something which should be generalized somehow, because
;; this stuff doesn't stop other ways of dataflow.

(defmethod handle-view-set-object-model :around ((view widget-base) model)
  (when (visible-p-of view)
    (call-next-method)))


(defmethod handle-view-set-slot-model :around ((view widget-base) model slot-name)
  (when (visible-p-of view)
    (call-next-method)))


(defmethod handle-model-event :around ((view widget-base) model event)
  (when (visible-p-of view)
    (call-next-method)))


(defmethod handle-model-slot-set-event :around ((view widget-base) model slot-name event)
  (when (visible-p-of view)
    (call-next-method)))



;;; container-base.lisp
;;;;;;;;;;;;;;;;;;;;;;;

(defclass container-base ()
  ((children :reader children-of
             :type list
             :initform nil)

   ;; TODO: I don't think anything uses or should use this; this is a Model type of thing.
   #|(num-children :reader num-children-of
                 :type integer
                 :initform 0)|#

   (type-check-fn :reader type-check-fn-of
                  :type (or null function))))  
(export '(container-base children-of type-check-fn-of))
