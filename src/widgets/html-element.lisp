;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations))


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
  (with-object html-element
    (setf ¤formula
          #λ(when-commit ()
              (setf (html-content-of html-element)
                    ~model)))))


(defmacro mk-div (args)
  (if (listp args)
      `(make-instance 'html-element
                      :element-type "div"
                      ,@args)
      `(make-instance 'html-element
                      :element-type "div"
                      :html-content ,args)))
(export 'mk-div)
