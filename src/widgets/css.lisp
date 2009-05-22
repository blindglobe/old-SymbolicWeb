;;;; http://sw.nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations))


#.(maybe-inline 'css)
(defun css (property widget
            &optional dom-cache-reader-fn)
  (declare (string property)
           (widget widget)
           (optimize speed))
  (if *js-code-only-p*
      (js-get-css (id-of widget) property)
      (when dom-cache-reader-fn (funcall (the function dom-cache-reader-fn)))))
(export 'css)


#.(maybe-inline '(setf css))
(defun (setf css) (new-value property widget &key
                   dom-cache-writer-fn server-only-p)
  (declare (string new-value property)
           (widget widget)
           (optimize speed))
  (flet ((js-code ()
           (js-set-css (id-of widget) property new-value)))
    (declare (inline js-code))
    (if *js-code-only-p*
        (js-code)
        (progn
          (when dom-cache-writer-fn (funcall (the function dom-cache-writer-fn)))
          (unless server-only-p (run (js-code) widget))))))
(export 'css)



(defmacro gen-dom-css-class (name &key (reader-value-on-no-entry nil reader-value-on-no-entry-supplied-p))
  (let* (;;(class-name (symbolicate (string-upcase (catstr "dom-css-" name))))
         (slot-name (symbolicate (string-upcase name)))
         (accessor (symbolicate slot-name "-OF"))
         ;;(initarg (format-symbol :keyword (string-upcase name)))
         (base-code
          `(def-dom-class #|,class-name|# ,slot-name css ,name
                          :writer-check-for-value-designating-removal-code (eq value nil)
                          :writer-value-marshaller-code (princ-to-string value)
                          :accessor ,accessor
                          #|:initarg ,initarg|#)))
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

#|
(defclass dom-all-css-font-style-properties (dom-css-font-family
                                             dom-css-font-style
                                             dom-css-font-variant
                                             dom-css-font-weight
                                             dom-css-font-size
                                             dom-css-font)
  ())
(export 'dom-all-css-font-style-properties)
|#


;; Color and background properties

(gen-dom-css-class "color")
(gen-dom-css-class "background-color")
(gen-dom-css-class "background-image")
(gen-dom-css-class "background-repeat")
(gen-dom-css-class "background-attachment")
(gen-dom-css-class "background-position")
(gen-dom-css-class "background")

#|
(defclass dom-all-css-color-and-background-properties (dom-css-color
                                                       dom-css-background-color
                                                       dom-css-background-image
                                                       dom-css-background-repeat
                                                       dom-css-background-attachment
                                                       dom-css-background-position
                                                       dom-css-background)
  ())
(export 'dom-all-css-color-and-background-properties)
|#


;; Text properties

(gen-dom-css-class "word-spacing")
(gen-dom-css-class "letter-spacing")
(gen-dom-css-class "text-decoration")
(gen-dom-css-class "vertical-align")
(gen-dom-css-class "text-transform")
(gen-dom-css-class "text-align")
(gen-dom-css-class "text-indent")
(gen-dom-css-class "line-height")

#|
(defclass dom-all-css-text-style-properties (dom-css-word-spacing
                                             dom-css-letter-spacing
                                             dom-css-text-decoration
                                             dom-css-vertical-align
                                             dom-css-text-transform
                                             dom-css-text-align
                                             dom-css-text-indent
                                             dom-css-line-height)
  ())
(export 'dom-all-css-text-style-properties)
|#


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

#|
(defclass dom-all-css-box-style-properties (dom-css-margin-top
                                            dom-css-margin-right
                                            dom-css-margin-bottom
                                            dom-css-margin-left
                                            dom-css-margin
                                            dom-css-padding-top
                                            dom-css-padding-right
                                            dom-css-padding-bottom
                                            dom-css-padding-left
                                            dom-css-padding
                                            dom-css-border-top-width
                                            dom-css-border-right-width
                                            dom-css-border-bottom-width
                                            dom-css-border-left-width
                                            dom-css-border-width
                                            dom-css-border-color
                                            dom-css-border-style
                                            dom-css-border-top
                                            dom-css-border-right
                                            dom-css-border-bottom
                                            dom-css-border-left
                                            dom-css-width
                                            dom-css-height
                                            dom-css-float
                                            dom-css-clear)
  ())
(export 'dom-all-css-box-style-properties)
|#


;; Classification properties

(gen-dom-css-class "display")
(gen-dom-css-class "white-space")
(gen-dom-css-class "list-style-type")
(gen-dom-css-class "list-style-image")
(gen-dom-css-class "list-style-position")
(gen-dom-css-class "list-style")

#|
(defclass dom-all-css-classification-style-properties (dom-css-display
                                                       dom-css-white-space
                                                       dom-css-list-style-type
                                                       dom-css-list-style-image
                                                       dom-css-list-style-position
                                                       dom-css-list-style)
  ())
(export 'dom-all-css-classification-style-properies)
|#


;; All CSS1 properties

#|
(defclass dom-all-css1-properties (dom-all-css-font-style-properties
                                   dom-all-css-color-and-background-properties
                                   dom-all-css-text-style-properties
                                   dom-all-css-box-style-properties
                                   dom-all-css-classification-style-properties)
  ())
(export 'dom-all-css1-properties)
|#



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

#|
(defclass dom-all-css21-properties (dom-css-border-top-color
                                    dom-css-border-right-color
                                    dom-css-border-bottom-color
                                    dom-css-border-left-color
                                    dom-css-border-top
                                    dom-css-border-right
                                    dom-css-border-bottom
                                    dom-css-border-left
                                    dom-css-border
                                    dom-css-position
                                    dom-css-overflow
                                    dom-css-cursor
                                    dom-css-top
                                    dom-css-right
                                    dom-css-bottom
                                    dom-css-left
                                    dom-css-z-index
                                    dom-css-direction
                                    dom-css-unicode-bidi
                                    dom-css-visibility)
  ())
(export 'dom-all-css21-properties)
|#
              
#|              
(defclass dom-all-css-properties (dom-all-css1-properties dom-all-css21-properties)
  ())
(export 'dom-all-css-properties)
|#
  