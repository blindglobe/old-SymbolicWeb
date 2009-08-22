;;;; http://sw.nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/css.lisp))


#.(maybe-inline 'css)
(defun css (property widget)
  (declare (string property)
           (widget widget))
  (js-get-css (id-of widget) property))
(export 'css)


#.(maybe-inline '(setf css))
(defun (setf css) (new-value property widget &rest args)
  (declare (string new-value property)
           (widget widget)
           (dynamic-extent args))
  (flet ((js-code ()
           (js-set-css (id-of widget) property new-value)))
    (declare (inline js-code))
    (if *js-code-only-p*
        (js-code)
        (apply #'run (js-code) widget args))))
(export 'css)


(declaim (inline css-remove))
(defun css-remove (property widget &key server-only-p)
  (declare (string property)
           (widget widget))
  (write-line "TODO: CSS-REMOVE"))
(export 'css-remove)



(defmacro define-css-property (lisp-name dom-name &body args)
  `(progn
     (define-dom-property ',lisp-name
         :dom-client-writer (lambda (new-value widget &rest args)
                              (declare (inline (setf css)))
                              (setf (apply #'css ,dom-name widget args) new-value))
         :dom-client-reader (lambda (widget)
                              (declare (inline css))
                              (css ,dom-name widget))
         :dom-client-remover (lambda (widget &rest args)
                               (declare (inline css-remove))
                               (apply #'css-remove ,dom-name widget args))
         ,@args)
     (export ',lisp-name)))



;;; CSS1

;; Font properties

(define-css-property font-family-of "font-family")
(define-css-property font-style-of "font-style")
(define-css-property font-variant-of "font-variant")
(define-css-property font-weight-of "font-weight")
(define-css-property font-size-of "font-size")
(define-css-property font-of "font")


;; Color and background properties

(define-css-property color-of "color")
(define-css-property background-color-of "background-color")
(define-css-property background-image-of "background-image")
(define-css-property background-repeat-of "background-repeat")
(define-css-property background-attachment-of "background-attachment")
(define-css-property background-position-of "background-position")
(define-css-property background-of "background")


;; Text properties

(define-css-property word-spacing-of "word-spacing")
(define-css-property letter-spacing-of "letter-spacing")
(define-css-property text-decoration-of "text-decoration")
(define-css-property vertical-align-of "vertical-align")
(define-css-property text-transform-of "text-transform")
(define-css-property text-align-of "text-align")
(define-css-property text-indent-of "text-indent")
(define-css-property line-height-of "line-height")


;; Box properties

(define-css-property margin-top-of "margin-top")
(define-css-property margin-right-of "margin-right")
(define-css-property margin-bottom-of "margin-bottom")
(define-css-property margin-left-of "margin-left")
(define-css-property margin-of "margin")
(define-css-property padding-top-of "padding-top")
(define-css-property padding-right-of "padding-right")
(define-css-property padding-bottom-of "padding-bottom")
(define-css-property padding-left-of "padding-left")
(define-css-property padding-of "padding")
(define-css-property border-top-width-of "border-top-width")
(define-css-property border-right-width-of "border-right-width")
(define-css-property border-bottom-width-of "border-bottom-width")
(define-css-property border-left-width-of "border-left-width")
(define-css-property border-width-of "border-width")
(define-css-property border-color-of "border-color")
(define-css-property border-style-of "border-style")
(define-css-property border-top-of "border-top")
(define-css-property border-right-of "border-right")
(define-css-property border-bottom-of "border-bottom")
(define-css-property border-left-of "border-left")
(define-css-property width-of "width"
  :dom-server-reader-fallback-value :auto)
(define-css-property height-of "height"
  :dom-server-reader-fallback-value :auto)
(define-css-property float-of "float")
(define-css-property clear-of "clear"
  :dom-server-reader-fallback-value :none)


;; Classification properties

(define-css-property display-of "display")
(define-css-property white-space-of "white-space"
  :dom-server-reader-fallback-value :normal)
(define-css-property list-style-type-of "list-style-type")
(define-css-property list-style-image-of "list-style-image")
(define-css-property list-style-position-of "list-style-position")
(define-css-property list-style-of "list-style")


;;; CSS21

(define-css-property border-top-color-of "border-top-color")
(define-css-property border-right-color-of "border-right-color")
(define-css-property border-bottom-color-of "border-bottom-color")
(define-css-property border-left-color-of "border-left-color")
(define-css-property border-top-of "border-top")
(define-css-property border-right-of "border-right")
(define-css-property border-bottom-of "border-bottom")
(define-css-property border-left-of "border-left")
(define-css-property border-of "border")
(define-css-property position-of "position"
  :dom-server-reader-fallback-value :static)
(define-css-property overflow-of "overflow"
  :dom-server-reader-fallback-value :visible)
(define-css-property cursor-of "cursor"
  :dom-server-reader-fallback-value :auto)
(define-css-property top-of "top"
  :dom-server-reader-fallback-value :auto)
(define-css-property right-of "right"
  :dom-server-reader-fallback-value :auto)
(define-css-property bottom-of "bottom"
  :dom-server-reader-fallback-value :auto)
(define-css-property left-of "left"
  :dom-server-reader-fallback-value :auto)
(define-css-property z-index-of "z-index"
  :dom-server-reader-fallback-value :auto)
(define-css-property direction-of "direction")
(define-css-property unicode-bidi-of "unicode-bidi")
(define-css-property visibility-of "visibility")
