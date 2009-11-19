;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/attributes.lisp))


#.(maybe-inline 'attribute)
(defun attribute (attribute widget)
  (declare (string attribute)
           (widget widget))
  (js-get-attribute (id-of widget) attribute))
(export 'attribute)


#.(maybe-inline '(setf attribute))
(defun (setf attribute) (new-value attribute widget &rest args)
  (declare (string new-value attribute)
           (widget widget)
           (dynamic-extent args))
  (flet ((js-code ()
           (js-set-attribute (id-of widget) attribute new-value)))
    (declare (inline js-code))
    (if *js-code-only-p*
        (js-code)
        (apply #'run (js-code) widget args))))
(export 'attribute)


(declaim (inline attribute-remove))
(defun attribute-remove (attribute widget &rest args)
  (declare (string attribute)
           (widget widget)
           (dynamic-extent args))
  (flet ((js-code ()
           (js-remove-attribute (id-of widget) attribute)))
    (declare (inline js-code))
    (if *js-code-only-p*
        (js-code)
        (apply #'run (js-code) widget args))))
(export 'attribute-remove)



(defmacro define-attribute-property (lisp-name dom-name &body args)
  `(progn
     (define-dom-property ',lisp-name
         :dom-client-writer (lambda (new-value widget &rest args)
                              (declare (inline (setf attribute)))
                              (setf (apply #'attribute ,dom-name widget args) new-value))
         :dom-client-reader (lambda (widget)
                              (declare (inline attribute))
                              (attribute ,dom-name widget))
         :dom-client-remover (lambda (widget &rest args)
                               (declare (inline attribute-remove))
                               (apply #'attribute-remove ,dom-name widget args))
         ,@args)
     (export ',lisp-name)))


(define-attribute-property value-of "value"
  :dom-server-reader-fallback-value "")


(define-attribute-property css-class-of "class"
  :value-marshaller (lambda (value) (format nil "~{~A~^ ~}" value))
  :value-removal-checker nil)


(defun add-class (widget class-name &key server-only-p)
  (declare (widget widget))
  "Add a CSS class to WIDGET."
  (setf class-name (string-downcase class-name))
  (let ((classes (let ((*js-code-only-p* nil))
                   (css-class-of widget))))
    (pushnew class-name classes :test #'string=)
    (setf (css-class-of widget :server-only-p server-only-p) classes)))
(export 'add-class)


(defun remove-class (widget class-name &key server-only-p)
  (declare (widget widget))
  "Remove a CSS class from WIDGET."
  (setf class-name (string-downcase class-name))
  (let ((classes (let ((*js-code-only-p* nil))
                   (css-class-of widget))))
    (deletef classes class-name :test #'string=)
    (setf (css-class-of widget :server-only-p server-only-p) classes)))
(export 'remove-class)


(define-attribute-property title-of "title")


(define-attribute-property readonly-p-of "readonly"
  :value-marshaller (lambda (value) (if value "readonly" ""))
  :value-removal-checker (lambda (value) (eq :dom-property-remove value)))


(define-attribute-property disabled-p-of "disabled"
  :value-marshaller (lambda (value) (if value "disabled" ""))
  :value-removal-checker (lambda (value) (eq :dom-property-remove value)))


(define-attribute-property selected-p-of "selected"
  :value-marshaller (lambda (value) (if value "selected" ""))
  :value-removal-checker (lambda (value) (eq :dom-property-remove value)))


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
