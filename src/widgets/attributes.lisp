;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations))


#.(maybe-inline 'attribute)
(defun attribute (attribute widget
                  &optional dom-cache-reader-fn)
  (declare (string attribute)
           (widget widget))
  (if *js-code-only-p*
      (js-get-attribute (id-of widget) attribute)
      (when dom-cache-reader-fn (funcall (the function dom-cache-reader-fn)))))
(export 'attribute)


#.(maybe-inline '(setf attribute))
(defun (setf attribute) (new-value attribute widget &key
                         dom-cache-writer-fn server-only-p)
  (declare (string new-value attribute)
           (widget widget))
  (flet ((js-code ()
           (js-set-attribute (id-of widget) attribute new-value)))
    (declare (inline js-code))
    (if *js-code-only-p*
        (js-code)
        (progn
          (when dom-cache-writer-fn (funcall (the function dom-cache-writer-fn)))
          (unless server-only-p (run (js-code) widget))))))
(export 'attribute)



(def-dom-class value attribute "value"
               :reader-value-on-no-entry ""
               :writer-check-for-value-designating-removal-code (eq value nil)
               :writer-value-marshaller-code (princ-to-string value))
(export '(dom-value-of))


;;(def-dom-class dom-attribute-selected-p selected-p attribute "selected"
;;               :writer-value-marshaller-code (if value "selected" ""))
;;(export '(dom-attribute-selected-p selected-p-of))


(def-dom-class css-class attribute "class"
               :writer-value-marshaller-code (format nil "~{~A~^ ~}" value)
               :writer-check-for-value-designating-removal-code (eq value nil))
;;(export '(css-class-of))


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


(def-dom-class title attribute "title"
               :writer-check-for-value-designating-removal-code (eq value nil))
(export '(title-of))


(def-dom-class readonly-p attribute "readonly"
               :writer-value-marshaller-code (if value "readonly" ""))
(export '(readonly-p-of))


(def-dom-class disabled-p attribute "disabled"
               :writer-value-marshaller-code (if value "disabled" ""))
(export '(dom-attribute-disabled-p disabled-p-of))


(def-dom-class name attribute "name"
               :writer-check-for-value-designating-removal-code (eq value nil))
(export '(name-of))


(def-dom-class checked-p attribute "checked"
               :writer-value-marshaller-code (if value "checked" ""))
(export '(checked-p-of))


(def-dom-class href attribute "href"
               :writer-check-for-value-designating-removal-code (eq value nil))
(export '(href-of))


(def-dom-class src attribute "src"
               :writer-check-for-value-designating-removal-code (eq value nil))
(export '(src-of))


;; Used by the TEXT-AREA widget (text-area.lisp).
(def-dom-class cols attribute "cols"
               :writer-value-marshaller-code (princ-to-string value))
(export '(cols-of))

;; Used by the TEXT-AREA widget (text-area.lisp).
(def-dom-class rows attribute "rows"
               :writer-value-marshaller-code (princ-to-string value))
(export '(rows-of))


;; Used by the TABLE widget (table-container.lisp).
(def-dom-class colspan attribute "colspan")
(export '(colspan-of))

;; Used by the TABLE widget (table-container.lisp).
(def-dom-class rowspan attribute "rowspan")
(export '(rowspan-of))


(def-dom-class tabindex attribute "tabindex"
               :reader-value-on-no-entry 0
               :writer-check-for-value-designating-removal-code (eq value nil)
               :writer-value-marshaller-code (princ-to-string value))
(export '(tabindex-of))
