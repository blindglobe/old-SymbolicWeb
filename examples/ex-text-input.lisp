;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

#| NOTE:
This isn't optimized for LOC; I'm trying to "do the right thing" by separating data and presentation proper. |#


(defclass text-input-app-model (self-ref)
  ((x :initform #λ(random 100))
   (y :initform #λ(random 100))
   (square-of-x :initform ↑#λ(* ¤x ¤x))
   (sum :initform ↑#λ(+ ¤square-of-x ¤y)))

  (:metaclass mvc-class))



(defclass text-input-app-view (html-container self-ref)
  ((x :initform (mk-text-input ()))
   (x-feedback :initform (mk-span () "X needs more cowbell!"))

   (y :initform (mk-text-input ()))
   (y-feedback :initform (mk-span () "Y needs more cowbell!"))

   (square-of-x :initform (mk-span ()))
   (sum :initform (mk-span ())))

  (:default-initargs
   :model (make-instance 'text-input-app-model)))


(defmethod (setf model-of) ((model text-input-app-model) (view text-input-app-view))
  #| We connect MODEL and VIEW for automatic dataflow. At the same time, we make sure to return a list of the
  connections so the framework can disconnect stuff later if we where to assign another Model to VIEW (reassign). |#
  (with-object view
    (list (setf ~¤x (with-object model
                      (with1 #λ¤x (forward-cell (mk-number-parser it t) (cell-of ¤x)))))
          (set-show-on-feedback ¤x-feedback ~¤x)

          (setf ~¤y (with-object model
                      (with1 #λ¤y (forward-cell (mk-number-parser it t) (cell-of ¤y)))))
          (set-show-on-feedback ¤y-feedback ~¤y)

          (setf ~¤square-of-x (with-object model #λ¤square-of-x))
          (setf ~¤sum (with-object model #λ¤sum)))))


(defmethod generate-html ((view text-input-app-view))
  (with-object view
    (who
      (:p "X: " (:sw ¤x) (:sw ¤x-feedback) :br
          "Y: " (:sw ¤y) (:sw ¤y-feedback))

      (:p "SQUARE-OF-X => " (:sw ¤square-of-x) :br
          "(+ SQUARE-OF-X Y) => " (:sw ¤sum)))))



(defclass text-input-app (application)
  ((view :initform (make-instance 'text-input-app-view)))

  (:metaclass mvc-class))

(set-uri 'text-input-app "/text-input")


(defmethod render-viewport ((viewport viewport) (app text-input-app))
  (with-object app
    (add-to (root)
      (mk-html ()
        (:div
         (:h1 "TEXT-INPUT-APP")

         (:sw ¤view)

         :hr
         (:a :href "http://gitorious.org/symbolicweb/symbolicweb/blobs/master/examples/ex-text-input.lisp"
             "source code")
         :br
         "Hosted on a crummy ADSL line...")))))
