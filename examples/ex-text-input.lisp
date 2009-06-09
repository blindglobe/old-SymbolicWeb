;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass text-input-app (application)
  ((text-input :initform (mk-text-input () "Hello"))
   (value-observer :initform ↑λ(format t "value: ~S~%" ~~¤text-input))
   (enter-observer :initform ↑λ(when (enterpress-state-of ¤text-input)
                                 (write-line "enter pressed!"))))

  (:metaclass mvc-stm-class))

(set-uri 'text-input-app "/text-input")



(defmethod render-viewport ((viewport viewport) (app text-input-app))
  (with-object app
    (add-to (root) ¤text-input)))