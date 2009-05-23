;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


;; Model?
(defclass simple-event-flow-2 (application)
  ((x :initform 0)
   (square-of-x :initform ↑λ(* ¤x ¤x)))

  (:metaclass mvc-stm-class))

(set-uri 'simple-event-flow-2 "/simple-event-flow-2")


;; Controller?
(defmethod incf-x ((app simple-event-flow-2))
  (with-object app
    (incf ¤x)))


;; View?
(defmethod render-viewport ((viewport viewport) (app simple-event-flow-2))
  (with-object app
    (let ((click-me-widget ¤(html-element :model #~"Click me!")))
      
      λ(when (mouse-click-state-of click-me-widget)
         (incf-x app)) ;; ..calls Controller.
      
      (add-to (root)
              click-me-widget
              ¤(html-element :model #λ(fmtn "X: ~A" ¤x))
              ¤(html-element :model #λ(fmtn "SQUARE-OF-X: ~A" ¤square-of-x))))))


#|
Is this MVC "done right"? I can manipulate the Model via the Controller from the REPL:

  > (incf-x *app*)


|#


