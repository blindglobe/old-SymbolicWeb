;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/html-element.lisp))


(defclass html-element (widget)
  ((html-content :accessor html-content-of :initarg :html-content
                 :initform "")))


(flet ((update-html (html-element new-html)
         (with-object html-element
           (run (setf (js-html-of ¤id) (html<- new-html html-element))
                html-element))))
  (declare (inline update-html))


  (defmethod render ((html-element html-element))
    (update-html html-element (html-content-of html-element)))


  (defmethod (setf html-content-of) :after (new-html (html-element html-element))
    (update-html html-element new-html)))


(defmethod (setf model-of) ((model single-value-model) (html-element html-element))
  (add-formula html-element
               λ(when-commit ()
                  (setf (html-content-of html-element)
                        ~model))))


(defmacro mk-elt (element-type &rest args)
  (let ((element-type (string-downcase (princ-to-string element-type))))
    (if (listp args)
        `(make-instance 'html-element
                        :element-type ,element-type
                        ,@(if (member :model args)
                              args
                              `(:html-content ,(first args) ,@(rest args))))
        `(make-instance 'html-element
                        :element-type ,element-type
                        :html-content ,args))))
(export 'mk-elt)


(defmacro mk-div (&rest args)
  `(mk-elt :div ,@args))
(export 'mk-div)
