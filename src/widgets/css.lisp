;;;; http://sw.nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations))


#.(maybe-inline 'css)
(defun css (property widget &optional dom-cache-reader-fn)
  (declare (string property)
           (widget widget))
  (if *js-code-only-p*
      (js-get-css (id-of widget) property)
      (when dom-cache-reader-fn (funcall (the function dom-cache-reader-fn)))))
(export 'css)


#.(maybe-inline '(setf css))
(defun (setf css) (new-value property widget &key dom-cache-writer-fn server-only-p)
  (declare (string new-value property)
           (widget widget))
  (flet ((js-code ()
           (js-set-css (id-of widget) property new-value)))
    (declare (inline js-code))
    (if *js-code-only-p*
        (js-code)
        (progn
          (when dom-cache-writer-fn (funcall (the function dom-cache-writer-fn)))
          (unless server-only-p (run (js-code) widget))))))
(export 'css)



(defmacro gen-dom-css-class (name &key
                             (reader-value-on-no-entry nil reader-value-on-no-entry-supplied-p)
                             (writer-marshaller '(princ-to-string value)))
  (let* ((slot-name (symbolicate (string-upcase name)))
         (accessor (symbolicate slot-name "-OF"))
         (base-code
          `(def-dom-class ,slot-name css ,name
                          :writer-check-for-value-designating-removal-code (eq value nil)
                          :writer-value-marshaller-code ,writer-marshaller
                          :accessor ,accessor)))
    (when reader-value-on-no-entry-supplied-p
      (appendf base-code (list :reader-value-on-no-entry reader-value-on-no-entry)))
    base-code))


;;; CSS1

;; Font properties

(gen-dom-css-class "font-family")
(gen-dom-css-class "font-style")
(gen-dom-css-class "font-variant")
(gen-dom-css-class "font-weight")
(gen-dom-css-class "font-size")
(gen-dom-css-class "font")


;; Color and background properties

(gen-dom-css-class "color")
(gen-dom-css-class "background-color")
(gen-dom-css-class "background-image")
(gen-dom-css-class "background-repeat")
(gen-dom-css-class "background-attachment")
(gen-dom-css-class "background-position")
(gen-dom-css-class "background")


;; Text properties

(gen-dom-css-class "word-spacing")
(gen-dom-css-class "letter-spacing")
(gen-dom-css-class "text-decoration")
(gen-dom-css-class "vertical-align")
(gen-dom-css-class "text-transform")
(gen-dom-css-class "text-align")
(gen-dom-css-class "text-indent")
(gen-dom-css-class "line-height")


;; Box properties

(gen-dom-css-class "margin-top")
(gen-dom-css-class "margin-right")
(gen-dom-css-class "margin-bottom")
(gen-dom-css-class "margin-left")
(gen-dom-css-class "margin")
(gen-dom-css-class "padding-top")
(gen-dom-css-class "padding-right")
(gen-dom-css-class "padding-bottom")
(gen-dom-css-class "padding-left")
(gen-dom-css-class "padding")
(gen-dom-css-class "border-top-width")
(gen-dom-css-class "border-right-width")
(gen-dom-css-class "border-bottom-width")
(gen-dom-css-class "border-left-width")
(gen-dom-css-class "border-width")
(gen-dom-css-class "border-color")
(gen-dom-css-class "border-style")
(gen-dom-css-class "border-top")
(gen-dom-css-class "border-top")
(gen-dom-css-class "border-right")
(gen-dom-css-class "border-bottom")
(gen-dom-css-class "border-left")
(gen-dom-css-class "width" :reader-value-on-no-entry :auto)
(gen-dom-css-class "height")
(gen-dom-css-class "float")
(gen-dom-css-class "clear")


;; Classification properties

(gen-dom-css-class "display")
(gen-dom-css-class "white-space")
(gen-dom-css-class "list-style-type")
(gen-dom-css-class "list-style-image")
(gen-dom-css-class "list-style-position")
(gen-dom-css-class "list-style")


;;; CSS21

(gen-dom-css-class "border-top-color")
(gen-dom-css-class "border-right-color")
(gen-dom-css-class "border-bottom-color")
(gen-dom-css-class "border-left-color")
(gen-dom-css-class "border-top")
(gen-dom-css-class "border-right")
(gen-dom-css-class "border-bottom")
(gen-dom-css-class "border-left")
(gen-dom-css-class "border")
(gen-dom-css-class "position")
(gen-dom-css-class "overflow")
(gen-dom-css-class "cursor")
(gen-dom-css-class "top")
(gen-dom-css-class "right")
(gen-dom-css-class "bottom")
(gen-dom-css-class "left")
(gen-dom-css-class "z-index")
(gen-dom-css-class "direction")
(gen-dom-css-class "unicode-bidi")
(gen-dom-css-class "visibility")
