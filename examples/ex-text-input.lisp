;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass text-input-app (application)
  ((text-input :initform (mk-text-input () "Hello"))
   (value-observer :initform ↑λ(format t "value changed to: ~S~%" ~~¤text-input))
   (enter-observer :initform ↑λ(when (enterpress-state-of ¤text-input)
                                 (format t "enter pressed, and value is: ~S~%" ~~¤text-input)))

   (integer-input :initform (mk-text-input () 42)))

  (:metaclass mvc-stm-class))

(set-uri 'text-input-app "/text-input")


(defmethod render-viewport ((viewport viewport) (app text-input-app))
  (with-object app
    (add-to (root)
     (mk-htmlc :div
       (:h1 "TEXT-INPUT-APP")
       (:sw ¤text-input)))))