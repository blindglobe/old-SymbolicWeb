(in-package #:sw)


(defclass vecto-2-app (application)
  ((font :allocation :class
         :initform (zpb-ttf:open-font-loader "/usr/share/fonts/truetype/msttcorefonts/times.ttf"))))

(set-uri 'vecto-2-app "/vecto-2")


(defmethod render-viewport ((viewport viewport) (app vecto-2-app))
  (insert (with1 (make-instance 'vecto)
                  ;; CSS stuff.
            (setf (position-of it) "absolute"
                  (width-of it) "50%"
                  (height-of it) "50%"
                  (border-of it) "1px solid black"

                  ;; VECTO stuff.
                  (filename-of it) "vecto-2-app"
                  (redraw-fn-of it)
                  (lambda (vc)
                    (vecto:set-rgb-stroke 0 1 0)
                    (vecto:set-line-width 10)
                    (vecto:rectangle 0 0 (inner-width-of vc) (inner-height-of vc))
                    (vecto:stroke)
                    (vecto-simple-draw-string "Hello Lisp World!"
                                              (slot-value app 'font)
                                              (inner-width-of vc)
                                              (inner-height-of vc)))))
          :in (root)))
