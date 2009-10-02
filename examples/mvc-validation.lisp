;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass mvc-validation-model (self-ref)
  ((x :accessor x-of
      :initform (random 100))

   (y :accessor y-of
      :initform (random 100))

   (sum :reader sum-of
        :initform ↑λf(+ ¤x ¤y)))

  (:metaclass mvc-class))



(defclass mvc-validation-view (html-container)
  ((x :initform ↑(text-input (:model (cell-of (x-of ¤model)))))
   (y :initform ↑(text-input (:model (cell-of (y-of ¤model)))))
   (sum :initform ↑(span (:model (cell-of (sum-of ¤model))))))

  (:default-initargs
   :model (make-instance 'mvc-validation-model)))


;; TODO: This needs a name, some arguments and a proper place (not in this file).
(defun blah (view)
  (declare (view-base view))
  (let ((cell (cell-of ~view))
        (old nil))
    #λ(if-let ((fe (feedback-event-of cell)))
        (when-commit ()
          (setf old fe)
          (setf (border-color-of view) :red)
          (setf (tooltip-of view :show-p t) "This needs to be a number."))
        (when-commit ()
          ;;(when old ;; ..for black border.
          (nilf old)
          (setf (border-color-of view) :black)
          (nilf (tooltip-of view))))))


(defmethod (setf model-of) ((model mvc-validation-model) (view mvc-validation-view))
  (with-object view
    (list (add-input-handler ¤x #'mk-number-parser)
          (add-input-handler ¤y #'mk-number-parser)
          (blah ¤x)
          (blah ¤y)
          ;;#λ(dbg-prin1 ~~¤sum)
          )))


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
         #|(:a :href "http://gitorious.org/symbolicweb/symbolicweb/blobs/raw/master/examples/mvc-validation.lisp"
             "source code")|# :br
         ;;"Hosted on a crummy ADSL line..."
         ))
     :in (root))))
