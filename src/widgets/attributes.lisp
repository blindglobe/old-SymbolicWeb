;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations))


(defun attribute (attribute widget)
  (declare (string attribute)
           (widget widget)
           (inline js-get-attribute))
  (js-get-attribute (id-of widget) attribute))
(export 'attribute)


(defun (setf attribute) (new-value attribute widget &key server-only-p)
  (declare (string new-value attribute)
           (widget widget)
           (inline js-set-attribute))
  (flet ((js-code ()
           (js-set-attribute (id-of widget) attribute new-value)))
    (declare (inline js-code))
    (if *js-code-only-p*
        (js-code)
        (unless server-only-p (run (js-code) widget)))))
(export 'attribute)


(defun attribute-remove (attribute widget &key server-only-p)
  (declare (string attribute)
           (widget widget))
  (write-line "TODO: ATTRIBUTE-REMOVE"))
(export 'attribute-remove)


(defmacro define-attribute-property (lisp-name dom-name &body args)
  `(progn
     (define-dom-property ',lisp-name ,dom-name #'(setf attribute) #'attribute #'attribute-remove
                          ,@args)
     (export ',lisp-name)))


(define-attribute-property value-of "value"
  :dom-server-reader-fallback-value "")


(define-attribute-property css-class-of "class"
  :value-marshaller (lambda (value) (format nil "~{~A~^ ~}" value)))


(defun add-class (widget class-name &key server-only-p)
  (declare (widget widget)
           (string class-name))
  (let ((classes (let ((*js-code-only-p* nil))
                   (css-class-of widget))))
    (pushnew class-name classes :test #'string=)
    (setf (css-class-of widget :server-only-p server-only-p) classes)))
(export 'add-class)


(defun remove-class (widget class-name &key server-only-p)
  (declare (widget widget)
           (string class-name))
  (let ((classes (let ((*js-code-only-p* nil))
                   (css-class-of widget))))
    (deletef classes class-name :test #'string=)
    (setf (css-class-of widget :server-only-p server-only-p) classes)))
(export 'remove-class)


(define-attribute-property title-of "title")


(define-attribute-property readonly-p-of "readonly"
  :value-marshaller (lambda (value) (if value "readonly" "")))


(define-attribute-property disabled-p-of "disabled"
  :value-marshaller (lambda (value) (if value "disabled" "")))


(define-attribute-property name-of "name")


(define-attribute-property checked-p-of "checked"
  :value-marshaller (lambda (value) (if value "checked" "")))


(define-attribute-property href-of "href")


(define-attribute-property src-of "src")


;; Used by the TEXT-AREA widget (text-area.lisp).
(define-attribute-property cols-of "cols")


;; Used by the TEXT-AREA widget (text-area.lisp).
(define-attribute-property rows-of "rows")


;; Used by the TABLE widget (table-container.lisp).
(define-attribute-property colspan-of "colspan")

;; Used by the TABLE widget (table-container.lisp).
(define-attribute-property rowspan-of "rowspan")


(define-attribute-property tabindex-of "tabindex"
  :dom-server-reader-fallback-value 0)
