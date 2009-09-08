;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass image (widget)
  ()

  (:default-initargs
   :element-type "image"
   :model #λ""))
(export 'image)


(defun mk-image (src &rest initargs)
  (with1 (apply #'make-instance 'image initargs)
    (setf (src-of it) src)))
(export 'mk-image)


(defmethod (setf model-of) ((model cell) (view image))
  )