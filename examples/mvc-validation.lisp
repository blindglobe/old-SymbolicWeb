;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)


(defclass mvc-validation-model (self-ref)
  ((x :accessor x-of
      :initform (random 100))

   (y :accessor y-of
      :initform (random 100))

   (sum :reader sum-of
        :initform (with1 ↑λf(+ ¤x ¤y)
                    (setf (accepts-conditions-p-of ~it) t))))

  (:metaclass mvc-class))



(defclass mvc-validation-view (html-container)
  ((x :initform ↑(with1 (text-input (:model (cell-of (x-of ¤model))))
                   (setf (border-color-of it) :black)))
   (y :initform ↑(with1 (text-input (:model (cell-of (y-of ¤model))))
                   (setf (border-color-of it) :black)))
   (sum :initform ↑(span (:model (cell-of (sum-of ¤model))))))

  (:default-initargs
   :model (make-instance 'mvc-validation-model)))


(flet ((fe-handler (fe text-input)
         (if fe
             (setf (border-color-of text-input) :red
                   (tooltip-of text-input :show-p t) "Need valid number input.")
             (setf (border-color-of text-input) :black
                   (tooltip-of text-input) nil))))


  (defmethod (setf model-of) ((model mvc-validation-model) (view mvc-validation-view))
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
  ((view :initform (make-instance 'mvc-validation-view))))

(set-uri 'mvc-validation-app "mvc-validation")


(defmethod initialize-instance :after ((app mvc-validation-app) &key)
  ;; NOTE: These point to uncompressed development versions of the jQuery UI libraries.
  (add-resource app "jquery-ui-core" :css
                (mk-static-data-url app "jquery-ui-dev/themes/base/ui.all.css"))
  (add-resource app "jquery-ui-core" :js
                (mk-static-data-url app "jquery-ui-dev/ui/jquery.ui.core.js"))

  (add-resource app "jquery-ui-position" :js
                (mk-static-data-url app "jquery-ui-dev/ui/jquery.ui.position.js"))

  (add-resource app "jquery-ui-tooltip" :css
                (mk-static-data-url app "jquery-ui-dev/themes/base/ui.tooltip.css"))
  (add-resource app "jquery-ui-tooltip" :js
                (mk-static-data-url app "jquery-ui-dev/ui/jquery.ui.tooltip.js")))


(defmethod render-viewport ((viewport viewport) (app mvc-validation-app))
  (with-object app
    (insert
     (mk-html ()
       (:div
         (:h1 "MVC-VALIDATION-APP")

         (:sw ¤view)

         :hr
         (:a :href "http://gitorious.org/symbolicweb/symbolicweb/blobs/raw/master/examples/mvc-validation.lisp"
             "source code") :br
             "Hosted on a crummy ADSL line, and this app. currently pulls down a lot of uncompressed and not needed "
             "stuff."))
     :in (root))))
