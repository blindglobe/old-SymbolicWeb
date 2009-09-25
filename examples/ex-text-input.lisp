;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass text-input-widget-model (self-ref)
  ((x :accessor x-of
      :initform (random 100))

   (y :accessor y-of
      :initform (random 100))

   (square-of-x :reader square-of-x-of
                :initform ↑λf(* ¤x ¤x))

   (sum :reader sum-of
        :initform ↑λf(+ ¤square-of-x ¤y)))

  (:metaclass mvc-class))



(defclass text-input-widget-view (html-container)
  ((x :initform ↑(text-input (:model (cell-of (x-of ¤model)))))
   (y :initform ↑(text-input (:model (cell-of (y-of ¤model)))))

   (square-of-x :initform ↑(span (:model (cell-of (square-of-x-of ¤model)))))
   (square-of-x-str :initform ↑(span (:model #λ(handler-case (format nil "~R" (square-of-x-of ¤model))
                                                 (error () "Can't show this number as text.")))))
   (sum :initform ↑(span (:model (cell-of (sum-of ¤model))))))

  (:default-initargs
   :model (make-instance 'text-input-widget-model)))


(defmethod (setf model-of) ((model text-input-widget-model) (view text-input-widget-view))
  (with-object view
    (list (add-input-handler ¤x #'mk-number-parser)
          (add-input-handler ¤y #'mk-number-parser))))


(defmethod generate-html ((view text-input-widget-view))
  (with-object view
    (who
      (:p "X: " (:sw ¤x) :br
          "Y: " (:sw ¤y))

      (:p "SQUARE-OF-X => " (:sw ¤square-of-x) :br
          "(+ SQUARE-OF-X Y) => " (:sw ¤sum))

      (:p "SQUARE-OF-X-STR => " (:sw ¤square-of-x-str)))))



(defclass text-input-app (application)
  ((shared-model :initform (make-instance 'text-input-widget-model))))

(set-uri 'text-input-app "/text-input")


(defmethod render-viewport ((viewport viewport) (app text-input-app))
  (with-object app
    (insert
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
         "Hosted on a crummy ADSL line..."))
     :in (root))))
