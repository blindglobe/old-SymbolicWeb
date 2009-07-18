;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

#| NOTE:
This isn't optimized for LOC; I'm trying to "do the right thing" by separating data and presentation proper. |#


(defclass text-input-widget-model (self-ref)
  ((x :initform #λ(random 100))
   (y :initform #λ(random 100))
   (square-of-x :initform ↑#λ(* ¤x ¤x))
   (sum :initform ↑#λ(+ ¤square-of-x ¤y)))

  (:metaclass mvc-class))



(defclass text-input-widget-view (html-container)
  ((x :initform (mk-text-input ()))
   (x-feedback :initform (mk-span () "X needs more cowbell!"))

   (y :initform (mk-text-input ()))
   (y-feedback :initform (mk-span () "Y needs more cowbell!"))

   (square-of-x :initform (mk-span ()))
   (square-of-x-str :initform (mk-span ()))
   (sum :initform (mk-span ())))

  (:default-initargs
   :model (make-instance 'text-input-widget-model)))


(defmethod (setf model-of) ((model text-input-widget-model) (view text-input-widget-view))
  #| We connect MODEL and VIEW for automatic dataflow. At the same time, we make sure to return a list of the
  connections so the framework can disconnect stuff later if we where to assign another Model to VIEW (reassign).

  A lot of stuff happens here. The first SETF expression basically:

    * Assigns a Model to the TEXT-INPUT widget/View 'X'.

    * This Model will forward changes from the back-end ("Model-end") to the front-end ("View-end").

    * It will forward user input from the front-end ("View-end") to the back-end ("Model-end"), but do so
      by going via a temporary CELL created by MK-NUMBER-PARSER which will parse the input converting it from
      a string to a number type.

    * In addition, this parsing might signal a condition and we handle that (any condition; in this case we don't
      care about details) by stating that we'd like the X-FEEDBACK widget to become visible when this happens.
  |#
  (with-object view
    (list (setf ~¤x (with-object model
                      (with1 #λ¤x (forward-cell (mk-number-parser it) (cell-of ¤x)))))
          (set-show-on-feedback ¤x-feedback ~¤x)

          (setf ~¤y (with-object model
                      (with1 #λ¤y (forward-cell (mk-number-parser it) (cell-of ¤y)))))
          (set-show-on-feedback ¤y-feedback ~¤y)

          (setf ~¤square-of-x (with-object model #λ¤square-of-x))
          ;; A second View of the the SQUARE-OF-X Model.
          (setf ~¤square-of-x-str (with-object model #λ(handler-case (format nil "~R" ¤square-of-x)
                                                         (error () "Can't show this number as text."))))

          (setf ~¤sum (with-object model #λ¤sum)))))


(defmethod generate-html ((view text-input-widget-view))
  (with-object view
    (who
      (:p "X: " (:sw ¤x) (:sw ¤x-feedback) :br
          "Y: " (:sw ¤y) (:sw ¤y-feedback))

      (:p "SQUARE-OF-X => " (:sw ¤square-of-x) :br
          "(+ SQUARE-OF-X Y) => " (:sw ¤sum))

      (:p "SQUARE-OF-X-STR => " (:sw ¤square-of-x-str)))))



(defclass text-input-app (application)
  ((shared-model :initform (make-instance 'text-input-widget-model))
   ;; Two Views viewing/observing the same Model instance.
   (view-1 :initform ↑(make-instance 'text-input-widget-view :model ¤shared-model))
   (view-2 :initform ↑(make-instance 'text-input-widget-view :model ¤shared-model)))

  (:metaclass mvc-class))

(set-uri 'text-input-app "/text-input")


(defmethod render-viewport ((viewport viewport) (app text-input-app))
  (with-object app
    (add-to (root)
      (mk-html ()
        (:div
         (:h1 "TEXT-INPUT-APP")

         (:h2 "VIEW-1")
         (:sw ¤view-1)

         (:h2 "VIEW-2")
         (:sw ¤view-2)

         :hr
         (:a :href "http://gitorious.org/symbolicweb/symbolicweb/blobs/master/examples/ex-text-input.lisp"
             "source code")
         :br
         "Hosted on a crummy ADSL line...")))))
