;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass simple-event-flow (self-ref)
  ((x :initform 0)
   (square-of-x :initform ↑λ(* ¤x ¤x)))

  (:metaclass mvc-stm-class))


(defmethod incf-x ((model simple-event-flow))
  (with-object model
    (incf ¤x)))



(defclass simple-event-flow-app (simple-event-flow application)
  ()

  (:metaclass mvc-stm-class))

(set-uri 'simple-event-flow-app "/simple-event-flow")


(defmethod render-viewport ((viewport viewport) (app simple-event-flow-app))
  (with-object app
    ;; Create some widgets.
    (let ((x-widget ¤(html-element :model #λ(fmtn "X: ~A" ¤x)))
          (square-of-x-widget ¤(html-element :model #λ(fmtn "SQUARE-OF-X: ~A" ¤square-of-x)))
          (click-me-widget ¤(html-element :html-content "Click me!")))

      ;; Whenever the CLICK-ME-WIDGET is clicked the INCF-X method should be called.
      λ(when (mouse-click-state-of click-me-widget)
         (incf-x app))

      ;; Add the widgets to the page.
      (add-to (root)
              x-widget
              square-of-x-widget
              click-me-widget))))
