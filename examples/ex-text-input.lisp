;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass text-input-app (application)
  ((x :initform #~5)
   (y :initform #~17)
   (square-of-x :initform ↑#λ(* ¤x ¤x))
   (sum :initform ↑#λ(+ ¤square-of-x ¤y))

   (x-view :initform ↑(mk-text-input (:model (cell-of ¤x))))
   (x-feedback :initform ↑(letp1 ((span (mk-elt :span "need more cowbell")))
                            (set-show-on-feedback span (cell-of ¤x)))))

  (:metaclass mvc-stm-class))

(set-uri 'text-input-app "/text-input")


(defmethod main ((app text-input-app))
  (with-object app
    (allf #'number-input-translator
          (input-translator-of (cell-of ¤x))
          (input-translator-of (cell-of ¤y)))))


(defmethod render-viewport ((viewport viewport) (app text-input-app))
  (with-object app
    (add-to (root)
      (mk-html
        (:div
         (:h1 "TEXT-INPUT-APP")

         "X: " (:sw ¤x-view) (:sw ¤x-feedback)
         :br

         "SQUARE-OF-X: " (:sw (cell-of ¤square-of-x))
         :br

         "Y: " (:sw (mk-text-input (:model (cell-of ¤y))))
         (:sw (letp1 ((span (mk-elt :span "need more cowbell!!")))
                (set-show-on-feedback span (cell-of ¤y))))
         :br

         "(+ SQUARE-OF-X Y): " (:sw (cell-of ¤sum))
         :p

         :hr
         (:a :href "http://gitorious.org/symbolicweb/symbolicweb/blobs/master/examples/ex-text-input.lisp"
             "source code"))))
    (focus ¤x-view)))
