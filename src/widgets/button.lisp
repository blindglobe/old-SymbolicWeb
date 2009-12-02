;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :widgets/button.lisp))


(defclass button (container)
  ()

  (:default-initargs
   :element-type "button"))


(defun mk-button (content)
  (let* ((container-view (make-instance 'button))
         (container-model ~container-view))
    (prog1 container-view
      (etypecase content
        (string
         (sw-mvc:insert #~content :in container-model))

        ((or single-value-model sw-mvc:container)
         (sw-mvc:insert content :in container-model))))))
