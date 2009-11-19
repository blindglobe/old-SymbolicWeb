;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :widgets/combo-box.lisp))



(defclass combo-box (container)
  ()

  (:default-initargs
   :element-type "select"
   :model (make-instance 'sw-mvc:container-with-1-active-item)))
(export 'combo-box)



(defclass combo-box-option (html-element)
  ()

  (:default-initargs
   :element-type "option"
   :model λV""))
(export 'combo-box-option)


(defmethod render :after ((combo-box-option combo-box-option))
  (setf (value-of combo-box-option)
        (id-of combo-box-option)))


(defmethod view-constructor ((combo-box combo-box) (model cell))
  (make-instance 'combo-box-option :model model))


(defmethod set-model nconc ((combo-box combo-box) (model container-with-1-active-item))
  (list λI(when-let (new-active-item (on-combo-box-change-of combo-box))
            (setf (active-item-of model)
                  new-active-item))))


(define-event-property
    (on-combo-box-change-of "change" :callback-data (list (cons "value" (js-code-of (value-of widget))))))


(defmethod initialize-callback-box ((combo-box combo-box) (lisp-accessor-name (eql 'on-combo-box-change-of))
                                    (callback-box callback-box))
  (setf (argument-parser-of callback-box)
        (let ((model ~combo-box))
          (lambda (args)
            (setf (active-item-of model)
                  (model-of (get-widget (cdr (assoc "value" args :test #'string=)))))))))


#|(progn
  (remove-all (root))
  (with (make-instance 'combo-box)
    (let ((b λV"b"))
      (insert λV"a" :in it)
      (insert b :in it)
      (insert λV"c" :in it))
    (insert it :in (root))))|#
