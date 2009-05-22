;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass simple-event-flow-2 (application self-ref)
  ((click-me-widget :initform ¤(html-element :model #~"Click me!"))
   (x :initform 0)
   (square-of-x :initform ↑λ(* ¤x ¤x)))

  (:metaclass mvc-stm-class))

(set-uri 'simple-event-flow-2 "/simple-event-flow-2")


(defmethod main ((app simple-event-flow-2))
  (with-object app
    
    λ(when (mouse-click-state-of ¤click-me-widget)
       (incf ¤x))))


(defmethod render-viewport ((viewport viewport) (app simple-event-flow-2))
  (with-object app
    (add-to (root)
            ¤click-me-widget
            ¤(html-element :model #λ(format nil "X: ~A" ¤x))
            ¤(html-element :model #λ(format nil "SQUARE-OF-X: ~A" ¤square-of-x))
            ¤(html-element :model #λ(format nil "X + X: ~A" (+ ¤x ¤x))))))
