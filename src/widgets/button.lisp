;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :widgets/button.lisp))


(defclass button (container)
  ()

  (:default-initargs
   :element-type "button"))
(export 'button)


(defun mk-button (content)
  (let* ((container-view (make-instance 'button))
         (container-model ~container-view))
    (prog1 container-view
      (typecase content
        (string
         (sw-mvc:insert #~content :in container-model))

        ((or single-value-model sw-mvc:container)
         (sw-mvc:insert content :in container-model))

        (otherwise
         (error "Don't know what to do with ~S." content))))))
(export 'mk-button)