;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass text-input-app-model (self-ref)
  ((x :initform (with1 #λ5 (add-number-parser it t)))
   (y :initform (with1 #λ17 (add-number-parser it t)))
   (square-of-x :initform ↑#λ(* ¤x ¤x))
   (sum :initform ↑#λ(+ ¤square-of-x ¤y)))

  (:metaclass mvc-class))



(defclass text-input-app (text-input-app-model application)
  ((x-view :initform ↑(mk-text-input (:model (cell-of ¤x))))
   (x-feedback :initform ↑(with1 (mk-elt :span "X needs more cowbell!")
                            (set-show-on-feedback it (cell-of ¤x))))

   (y-view :initform ↑(mk-text-input (:model (cell-of ¤y))))
   (y-feedback :initform ↑(with1 (mk-elt :span "Y needs more cowbell!")
                            (set-show-on-feedback it (cell-of ¤y)))))

  (:metaclass mvc-class))

(set-uri 'text-input-app "/text-input")


(defmethod render-viewport ((viewport viewport) (app text-input-app))
  (with-object app
    (add-to (root)
      (mk-html ()
        (:div
         (:h1 "TEXT-INPUT-APP")

         (:p "X: " (:sw ¤x-view) (:sw ¤x-feedback) :br
             "Y: " (:sw ¤y-view) (:sw ¤y-feedback))

         (:p "SQUARE-OF-X => " (:sw (cell-of ¤square-of-x)) :br
             "(+ SQUARE-OF-X Y) => " (:sw (cell-of ¤sum)))

         :hr
         (:a :href "http://gitorious.org/symbolicweb/symbolicweb/blobs/master/examples/ex-text-input.lisp"
             "source code"))))))
