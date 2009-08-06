;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/html-element.lisp))


(defclass html-element (widget)
  ((nil-denotes-empty-string-p :accessor nil-denotes-empty-string-p-of :initarg :nil-denotes-empty-string-p
                               :initform t)

   (escapep :accessor escapep-of :initarg :escapep
            :initform t
            :documentation "
If this is T (default), the renedring of HTML-ELEMENT will be escaped as text instead of HTML.
If this is NIL, HTML-ELEMENT will be renedered as HTML."))

  (:default-initargs
   :model #λ""))


(flet ((update-html (html-element new-html)
         (declare (html-element html-element))
         (run (setf (js-html-of (id-of html-element))
                    (if (or new-html (not (nil-denotes-empty-string-p-of html-element)))
                        (with (string<- new-html)
                          (if (escapep-of html-element)
                              (escape-for-html it)
                              it))
                        ""))
              html-element)))
  (declare (inline update-html))


  (defmethod render ((html-element html-element))
    (update-html html-element ~~html-element))


  (defmethod (setf model-of) ((model cell) (html-element html-element))
    #λ(let ((model-value ~model))
        (when-commit ()
          (update-html html-element model-value)))))



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
                    ,@(when (and (not (member :model args))
                                 html-content)
                     `(:model #λ,@html-content)))))
(export 'mk-elt)


(defmacro mk-div (args &body html-content)
  `(mk-elt (:div ,@args)
     ,@html-content))
(export 'mk-div)


(defmacro mk-span (args &body html-content)
  `(mk-elt (:span ,@args)
     ,@html-content))
(export 'mk-span)
