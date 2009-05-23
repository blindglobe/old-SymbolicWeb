;;;; http://nostdal.org/ ;;;;

(in-package #:sw)



;; Model.
(defclass simple-event-flow (self-ref)
  ((x :initform 0)
   (square-of-x :initform ↑λ(* ¤x ¤x)))
   
  (:metaclass mvc-stm-class))



;; Controller.
(defmethod incf-x ((model simple-event-flow))
  (with-object model
    (incf ¤x)))



;; (The APPLICATION class represents a user "web-session" etc.)
(defclass simple-event-flow-app (simple-event-flow application)
  ()
  
  (:metaclass mvc-stm-class))

(set-uri 'simple-event-flow-app "/simple-event-flow")



;; View.
(defmethod render-viewport ((viewport viewport) (app simple-event-flow-app))
  (with-object app
    (let ((click-me-widget ¤(html-element :model #~"Click me!")))
      
      λ(when (mouse-click-state-of click-me-widget)
         (incf-x app)) ;; ..calls Controller.
      
      (add-to (root)
              click-me-widget
              ¤(html-element :model λ(fmtn "X: ~A" ¤x))
              ¤(html-element :model λ(fmtn "SQUARE-OF-X: ~A" ¤square-of-x))))))




#|
Is this MVC "done right"? I can manipulate the Model via the Controller from the REPL:

> (incf-x *app*)

> (let ((model (make-instance 'simple-event-flow)))
    (with-slots (x square-of-x) model
      (dotimes (i 10)
        (incf x)
        (pprint (list x square-of-x)))))
(1 1)
(2 4)
(3 9)
(4 16)
(5 25)
(6 36)
(7 49)
(8 64)
(9 81)
(10 100)
NIL
|#







  
  