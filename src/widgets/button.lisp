;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations))


(defclass button (container)
  ()

  (:default-initargs
   :element-type "button"))
