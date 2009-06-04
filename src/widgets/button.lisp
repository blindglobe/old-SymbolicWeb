;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations))


(defclass button (container)
  ()

  (:default-initargs
   :element-type "button"))


(defmethod (setf model-of) ((model single-value-model) (button button))
  (with-object button
    (setf ¤formula
          #λ~model)))
