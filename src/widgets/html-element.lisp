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


(defmethod (setf model-of) ((model cell) (html-element html-element))
  (add-to html-element
    λ(let ((model-value ~model))
       ;;(dbg-prin1 model-value (fmtn "~A" html-element))
       (when-commit ()
         (setf (html-content-of html-element)
               model-value)))))


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
