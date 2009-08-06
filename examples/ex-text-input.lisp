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
  ((x :initform (mk-text-input nil))
   (x-feedback :initform (mk-span ()))

   (y :initform (mk-text-input nil))
   (y-feedback :initform (mk-span ()))

   (square-of-x :initform (mk-span ()))
   (square-of-x-str :initform (mk-span ()))
   (sum :initform (mk-span ())))

  (:default-initargs
   :model (make-instance 'text-input-widget-model)))


(defmethod (setf model-of) ((model text-input-widget-model) (view text-input-widget-view))
  #| We connect MODEL and VIEW for automatic dataflow. At the same time, we make sure to return a list of the
  connections so the framework can disconnect stuff later if we where to assign another Model to VIEW (reassign).

  A lot of stuff is going on here. The first SETF expression sets up something that looks like this (we use X-MODEL
  to denote the slot X in TEXT-INPUT-WIDGET-MODEL, X-VIEW to denote the slot X in TEXT-INPUT-WIDGET-VIEW etc. and
  IT denotes the CELL created at the first argument for the WITH1 macro):


                      (sync-back)
         -----------------------------------
         |                                 |
         v                                 |
      X-MODEL -------> IT ---------> NUMBER-PARSER
         |             ^
         |             |
         |             | (two-way: from X-VIEW to IT is user-input (a string))
         |             |
         |             v
         |           X-VIEW
         |
         v
  SQUARE-OF-X-MODEL
         |
         V
  SQUARE-OF-X-VIEW



  NUMBER-PARSER is the CELL created and returned by MK-NUMBER-PARSER. An important feature of SW-MVC is that the
  'sync-back' connection does not cause things to get stuck propagating in circles, this applies for two-way
  connections as well. |#
  (with-object view
    (list (setf ~¤x (with-object model
                      (with1 #λ¤x (forward-cell (mk-number-parser it) (cell-of ¤x)))))
          #λ(setf ~~¤x-feedback
                  (if-let (c ~(feedback-event-of ~¤x))
                    c
                    ""))

          (setf ~¤y (with-object model
                      (with1 #λ¤y (forward-cell (mk-number-parser it) (cell-of ¤y)))))
          #λ(setf ~~¤y-feedback
                  (if-let (c ~(feedback-event-of ~¤y))
                    c
                    ""))

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
  ((shared-model :initform (make-instance 'text-input-widget-model)))

  (:metaclass mvc-class))

(set-uri 'text-input-app "/text-input")


(defmethod render-viewport ((viewport viewport) (app text-input-app))
  (with-object app
    (add-to (root)
      (mk-html ()
        (:div
         (:h1 "TEXT-INPUT-APP")

         (:h2 "VIEW-1")
         (:sw (make-instance 'text-input-widget-view :model ¤shared-model))

         (:h2 "VIEW-2")
         (:sw (make-instance 'text-input-widget-view :model ¤shared-model))

         :hr
         (:a :href "http://gitorious.org/symbolicweb/symbolicweb/blobs/raw/master/examples/ex-text-input.lisp"
             "source code")
         :br
         "Hosted on a crummy ADSL line...")))))
