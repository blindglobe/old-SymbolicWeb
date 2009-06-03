;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations))


(defclass button (container)
  ()

  (:default-initargs
    :element-type "button"
    :model (make-instance 'boolean-model)))


(declaim (inline mk-button))
(defun mk-button (children &rest initargs)
  (apply #'make-instance 'button
         :children (amx:mklst children)
         initargs))


(defmethod (setf model-of) ((model single-value-model) (button button))
  (with-object button
    (setf ¤formula
          #λ(dbg-princ ~model))))