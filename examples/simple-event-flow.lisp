;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass simple-event-flow (application)
  ((click-me-widget :initform ¤(html-element :model #~"Click me!"))
   (message :initform ""))

  (:metaclass mvc-stm-class))

(set-uri 'simple-event-flow "/simple-event-flow")


(defmethod main ((app simple-event-flow))
  (with-object app
    λ(if (mouse-button-state-of ¤click-me-widget)
         (setf ¤message "Mouse button down!")
         (setf ¤message "Mouse button up!"))
    
    λ(when (mouse-click-state-of ¤click-me-widget)
       (write-line "CLICKED!"))))


(defmethod render-viewport ((viewport viewport) (app simple-event-flow))
  (with-object app
    (add-to (root)
            ¤click-me-widget
            ¤(html-element :model #λ¤message))))
