;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass text-input-app (application)
  ((x :initform #~42)
   (square-of-x :initform ↑#λ(* ~¤x ~¤x))))

(set-uri 'text-input-app "/text-input")


(defmethod main ((app text-input-app))
  (with-object app
    (setf (input-translator-of ¤x)
          (lambda (input)
            (handler-case
                (typecase input
                  (number input)
                  (string (parse-integer input :junk-allowed nil))
                  (t (error "Don't know what to do with ~S~%" input)))
              (error (c)
                (declare (ignore c))
                (warn "TEXT-INPUT-APP: Got ~A as user input.~%" input)
                ~¤x))))))


(defmethod render-viewport ((viewport viewport) (app text-input-app))
  (with-object app
    (let ((x-view (mk-text-input (:model ¤x))))
      (add-to (root)
        (mk-html
          (:div
           (:h1 "TEXT-INPUT-APP")
           "X: " (:sw x-view) :br
           "SQUARE-OF-X: " (:sw #λ~¤square-of-x))))
      (focus x-view))))
