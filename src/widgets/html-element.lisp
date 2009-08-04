;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/html-element.lisp))


(defclass html-element (widget)
  ((html-content :reader html-content-of :initarg :html-content
                 :initform "")

   (escapep :accessor escapep-of :initarg :escapep
            :initform t
            :documentation "
If this is T (default), HTML-CONTENT will be escaped as text and thus not rendered as HTML.
If this is NIL, HTML-CONTENT will be renedered as HTML.")))


(flet ((update-html (html-element new-html)
         (declare (html-element html-element)
                  (string new-html))
         (with-object html-element
           (run (setf (js-html-of ¤id) new-html)
                html-element))))
  (declare (inline update-html))


  (defmethod render ((html-element html-element))
    (update-html html-element (html-content-of html-element)))


  (defmethod (setf html-content-of) (new-html (html-element html-element))
    (update-html html-element
                 (if (escapep-of html-element)
                     (escape-for-html (html<- new-html html-element))
                     (html<- new-html html-element)))))


(defmethod (setf model-of) ((model cell) (html-element html-element))
  #λ(let ((model-value ~model))
      (when-commit ()
        (setf (html-content-of html-element) model-value))))


(defmacro mk-elt (args &body html-content)
  "First element of ARGS should be ELEMENT-TYPE. If it is not supplied
:DIV is silently assumed."
  (setf args (mklst args))
  (let ((element-type (first args)))
    (unless element-type
      (setf element-type :div))
    `(make-instance 'html-element
                    :element-type ,(if (constantp element-type)
                                       (string-downcase element-type)
                                       `(string-downcase ,element-type))
                    ,@(rest args)
                    ,@(when (and (not (member :html-content args))
                                 (not (member :model args))
                                 html-content)
                       `(:html-content ,@html-content)))))
(export 'mk-elt)


(defmacro mk-div (args &body html-content)
  `(mk-elt (:div ,@args)
     ,@html-content))
(export 'mk-div)


(defmacro mk-span (args &body html-content)
  `(mk-elt (:span ,@args)
     ,@html-content))
(export 'mk-span)