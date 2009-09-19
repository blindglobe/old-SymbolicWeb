(in-package #:sw)


(defclass vecto-1-app (application)
  ())

(set-uri 'vecto-1-app "/vecto-1")


(defmethod render-viewport ((viewport viewport) (app vecto-1-app))
  (insert (with1 (make-instance 'vecto)
                  ;; CSS stuff.
            (setf (position-of it) "absolute"
                  (width-of it) "50%"
                  (height-of it) "50%"
                  (border-of it) "1px solid black"

                  ;; VECTO stuff.
                  (redraw-fn-of it)
                  (lambda (vc)
                    (vecto:set-rgb-stroke 0 1 0)
                    (vecto:set-line-width 10)
                    (vecto:rectangle 0 0 (inner-width-of vc) (inner-height-of vc))
                    (vecto:stroke))))
          :in (root)))
