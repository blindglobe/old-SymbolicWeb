;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)


(defclass image (widget)
  ()

  (:default-initargs
   :element-type "image"
   :model #λ""))


(defun mk-image (src &rest initargs)
  (with1 (apply #'make-instance 'image initargs)
    (setf (src-of it) src)))


(defmethod (setf model-of) ((model cell) (view image))
  )