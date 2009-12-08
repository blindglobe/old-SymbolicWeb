;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)

(declaim #.(optimizations :widgets/html-element.lisp))


(defclass html-element (widget)
  ((html-content :accessor html-content-of
                 :documentation "
Not meant to be manipulated directly; use the MODEL instead.")

   (nil-denotes-empty-string-p :accessor nil-denotes-empty-string-p-of :initarg :nil-denotes-empty-string-p
                               :initform t)

   (escapep :accessor escapep-of :initarg :escapep
            :initform t
            :documentation "
If this is T (default), the renedring of HTML-ELEMENT will be escaped as text instead of HTML.
If this is NIL, HTML-ELEMENT will be renedered as HTML."))

  (:default-initargs
   :model λV""))


(flet ((update-html (html-element new-html)
         (declare (html-element html-element))
         (run (setf (js-html-of (id-of html-element))
                    (if (or new-html (not (nil-denotes-empty-string-p-of html-element)))
                        (if (escapep-of html-element)
                            (htmlize new-html)
                            (string<- new-html))
                        ""))
              html-element)))
  (declare (inline update-html))


  (defmethod render ((html-element html-element))
    (update-html html-element (html-content-of html-element)))


  (defmethod (setf html-content-of) :after (new-html (html-element html-element))
    (update-html html-element new-html))


  (defmethod set-model nconc ((html-element html-element) model)
    (list λI(with ~model
              (when-commit ()
                (setf (html-content-of html-element) it))))))




;; TODO: Finish and improve all this stuff:

(defmacro mk-elt (args &body html-content)
  "First element of ARGS should be ELEMENT-TYPE. If it is not supplied
:DIV is silently assumed."
  (setf args (ensure-list args))
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


(defmacro def-elt (element-type &key (xml-p t))
  (declare (ignore xml-p))
  `(progn
     (defmacro ,element-type (args &body html-content)
       `(mk-elt (,(make-keyword ',element-type) ,@(when (listp args) args))
          ,@(if (listp args)
                html-content
                (list args))))))


;; *GRR* the syntax is (DIV (:MODEL #λ42))
(def-elt div)
(def-elt span)
(def-elt b)
(def-elt em)
(def-elt h1) (def-elt h2) (def-elt h3)
(def-elt h4) (def-elt h5) (def-elt h6)
(def-elt li)

;; NOTE: The A element should probably have its own widget-type (LINK).