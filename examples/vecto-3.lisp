(in-package #:sw)


(defclass vecto-widget-3 (vecto)
  ((vc-font :reader vc-font-of
            :initform (zpb-ttf:open-font-loader "/usr/share/fonts/truetype/msttcorefonts/times.ttf")))

  (:default-initargs
   :filename "vecto-widget-3"))


(defmethod redraw ((vc vecto-widget-3))
  (vecto:set-rgb-stroke 0 1 0)
  (vecto:set-line-width 10)
  (vecto:rectangle 0 0 (inner-width-of vc) (inner-height-of vc))
  (vecto:stroke)
  (vecto:set-rgba-fill 1 0 0 0.25)
  (vecto-simple-draw-string "Hello Lisp World!"
                            (vc-font-of vc)
                            (inner-width-of vc)
                            (inner-height-of vc)
                            t))



(defclass vecto-3-app (application)
  ())

(set-uri 'vecto-3-app "vecto-3")


(defmethod render-viewport ((viewport viewport) (app vecto-3-app))
  (let ((vecto (make-instance 'vecto-widget-3)))
    (setf (position-of vecto) "absolute"
          (width-of vecto) "50%"
          (height-of vecto) "50%"
          (border-of vecto) "1px solid black")
    (with (mk-html ()
            (:div
              (:h3 "Hello World")
              (:p "Ok, this is a test of using multiple layers on top of a VECTO widget.")
              (:p "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Mauris in nisi eget ligula consectetur hendrerit. Nullam pellentesque tincidunt iaculis. Curabitur volutpat aliquam nisl, a aliquet diam consectetur ac. Suspendisse sit amet magna a lacus bibendum rhoncus sit amet id tortor. Ut risus sem, pharetra eget mattis eget, posuere ac risus. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Integer at eros id elit condimentum vestibulum at id leo. Cras a tellus eu nunc condimentum venenatis ac id lectus. Nam non tellus tellus. Quisque eget vestibulum elit. ")))
      (setf (position-of it) "absolute"
            (height-of it) "100%"
            (overflow-of it) "auto")
      (insert it :in vecto))
    (insert vecto :in (root))))
