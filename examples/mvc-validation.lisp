;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)


(defclass mvc-validation-model (self-ref)
  ((x :accessor x-of
      :initform (random 100))

   (y :accessor y-of
      :initform (random 100))

   (sum :reader sum-of
        :initform (with ↑λI(+ ¤x ¤y)
                    (setf (accepts-conditions-p-of it) t)
                    (as-formula it))))

  (:metaclass mvc-class))



(defclass mvc-validation-view (html-container)
  ((x :initform ↑(with1 (text-input (:model (cell-of (x-of ¤model))))
                   (setf (css-border-color-of it) :black)))
   (y :initform ↑(with1 (text-input (:model (cell-of (y-of ¤model))))
                   (setf (css-border-color-of it) :black)))
   (sum :initform ↑(span (:model (cell-of (sum-of ¤model))))))

  (:default-initargs
   :model (make-instance 'mvc-validation-model)))


(flet ((fe-handler (fe text-input)
         (if fe
             (setf (css-border-color-of text-input) :red
                   (tooltip-of text-input :show-p t) "Need valid number input.")
             (setf (css-border-color-of text-input) :black
                   (tooltip-of text-input) nil))))


  (defmethod set-model nconc ((view mvc-validation-view) (model mvc-validation-model))
    (with-object view
      (list (add-input-handler ¤x #'mk-number-parser)
            (add-input-handler ¤y #'mk-number-parser)
            (add-on-feedback ¤x (λ (fe) (fe-handler fe ¤x)))
            (add-on-feedback ¤y (λ (fe) (fe-handler fe ¤y)))))))


(defmethod generate-html ((view mvc-validation-view))
  (with-object view
    (who
      (:p "X: " (:sw ¤x) :br
          "Y: " (:sw ¤y) :br
          "X + Y = " (:sw ¤sum)))))



(defclass mvc-validation-app (application)
  ((model :initform (make-instance 'mvc-validation-model))))

(set-uri 'mvc-validation-app "mvc-validation")


(defmethod initialize-instance :after ((app mvc-validation-app) &key)
  (add-jquery-ui-resources :minified-p t))


(defmethod render-viewport ((viewport viewport) (app mvc-validation-app))
  (with-object app
    (insert
     (mk-html ()
       (:div
         (:h1 "MVC-VALIDATION-APP")

         (:sw (make-instance 'mvc-validation-view :model ¤model))

         :hr
         (:a :href "http://gitorious.org/symbolicweb/symbolicweb/blobs/raw/master/examples/mvc-validation.lisp"
             "source code") :br))
     :in (root))))
