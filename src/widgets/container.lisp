;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :widgets/container.lisp))


(defclass container (abstract-container)
  ()

  (:default-initargs
   :model (sw-mvc:dlist)))
(export 'container)


(defmethod view-constructor ((container container) (model cell))
  (make-instance 'html-element :model model))


(defmethod view-constructor ((container container) (model multiple-value-model))
  (make-instance 'container :model model))
