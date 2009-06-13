;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass text-input-app (application)
  ((x :initform #~5)
   (y :initform #~17)
   (square-of-x :initform ↑#λ(* ¤x ¤x))
   (sum :initform ↑#λ(+ ¤square-of-x ¤y)))

  (:metaclass mvc-stm-class))

(set-uri 'text-input-app "/text-input")


(defmethod main ((app text-input-app))
  (with-object app
    (setf (input-translator-of (cell-of ¤x))
          (lambda (input)
            (handler-case (number-input-translator input)
              (error () ¤x)))

          (input-translator-of (cell-of ¤y))
          (lambda (input)
            (handler-case (number-input-translator input)
              (error () ¤y))))))


(defmethod render-viewport ((viewport viewport) (app text-input-app))
  (with-object app
    (let ((x-view (mk-text-input (:model (cell-of ¤x)))))
      (add-to (root)
        (mk-html
          (:div
           (:h1 "TEXT-INPUT-APP")
           "X: " (:sw x-view) :br
           "SQUARE-OF-X: " (:sw (cell-of ¤square-of-x)) :br
           "Y: " (:sw (mk-text-input (:model (cell-of ¤y)))) :br
           "(+ SQUARE-OF-X Y): " (:sw (cell-of ¤sum))
           :p
           :hr
           (:a :href "http://gitorious.org/symbolicweb/symbolicweb/blobs/master/examples/ex-text-input.lisp"
               "source code"))))
      (focus x-view))))
