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
    (add-to (root)
            (mk-html
              (:div
               (:ul (:li "X: " (:sw #λ¤x))
                    (:li "SQUARE-OF-X: " (:sw #λ¤square-of-x)))

               (:p (:sw (letp1 ((button (mk-button "Click me!")))
                          λ(when (mouse-click-state-of button)
                             (incf-x app))))))))))
