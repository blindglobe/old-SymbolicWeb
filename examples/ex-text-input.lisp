;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass text-input-app-model (self-ref)
  ((x :initform #λ5)
   (y :initform #λ17)
   (square-of-x :initform ↑#λ(* ¤x ¤x))
   (sum :initform ↑#λ(+ ¤square-of-x ¤y)))

  (:metaclass mvc-class))


#| X-VIEW and Y-VIEW are "session bound" (created "inside" the DEFCLASS form) to ensure that focus tracking
works between page refreshing. |#


(defclass text-input-app (text-input-app-model application)
  ((x-view :initform ↑(mk-text-input (:model (cell-of ¤x))))
   (x-feedback :initform ↑(letp1 ((span (mk-elt :span "X needs more cowbell!")))
                            (set-show-on-feedback span (cell-of ¤x))))

   (y-view :initform ↑(mk-text-input (:model (cell-of ¤y))))
   (y-feedback :initform ↑(letp1 ((span (mk-elt :span "Y needs more cowbell!")))
                            (set-show-on-feedback span (cell-of ¤y)))))

  (:metaclass mvc-class))


(set-uri 'text-input-app "/text-input")


(defmethod main ((app text-input-app))
  (with-object app
    (add-writer-fn (cell-of ¤x) (make-number-parser (cell-of ¤x) t))
    (add-writer-fn (cell-of ¤y) (make-number-parser (cell-of ¤y) t))))


(defmethod render-viewport ((viewport viewport) (app text-input-app))
  (with-object app
    (remove-all (root))
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
