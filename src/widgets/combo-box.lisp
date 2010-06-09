;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :widgets/combo-box.lisp))


#| TODO: Implement support for the `multiple' attribute. I'll need another back-end Model for this (SW-MVC:CONTAINER-
WITH-N-ACTIVE-ITEMS). |#


(defclass combo-box (container)
  ((selected-option :reader selected-option-of
                    :initform nil)

   (model :type container-proxy))

  (:default-initargs
   :element-type "select"
   :model (make-instance 'sw-mvc:container-with-1-active-item
                         :fallback-item +null-model+
                         :model (dlist))))



(defclass combo-box-option (html-element)
  ((combo-box :reader combo-box-of :initarg :combo-box
              :initform (error ":COMBO-BOX needed.")))

  (:default-initargs
   :element-type "option"
   :model λV""))


(defmethod initialize-instance :after ((combo-box-option combo-box-option) &key)
  (setf (attribute-value-of combo-box-option)
        (id-of combo-box-option)))


(defmethod view-constructor ((combo-box combo-box) (model cell))
  (make-instance 'combo-box-option :combo-box combo-box :model model))


(defmethod (setf html-content-of) :around (new-html (combo-box-option combo-box-option))
  "We'd like +NULL-MODEL+ to be rendered in a custom fashion."
  (if (eq +null-model+ (model-of combo-box-option))
      (call-next-method "<none selected>" combo-box-option)
      (call-next-method)))


(defmethod set-model nconc ((combo-box combo-box) (model container-with-1-active-item))
  (dbg-prin1 model "(SET-MODEL COMBO-BOX CONTAINER-WITH-1-ACTIVE-ITEM)")
  (list λI(let* ((item-model (active-item-of model))
                 (item-view (when item-model (view-in-context-of combo-box item-model))))
            ;; Model → View.
            (when-commit ()
              (let ((old-item-view (selected-option-of combo-box)))
                (unless (eq item-view old-item-view)
                  (when item-view
                    (tf (attribute-selected-p-of item-view)))
                  (when old-item-view
                    (nilf (attribute-selected-p-of old-item-view)))
                  (setf (slot-value combo-box 'selected-option) item-view)))))

        (with-event (item-view) (on-event-combo-box-change combo-box)
          (when item-view
            (let ((item-model (model-of item-view)))
              ;; View → Model.
              (when-commit ()
                (let ((old-item-view (selected-option-of combo-box)))
                  (unless (eq item-view old-item-view)
                    (tf (attribute-selected-p-of item-view))
                    (when old-item-view
                      (nilf (attribute-selected-p-of (selected-option-of combo-box))))
                    (setf (slot-value combo-box 'selected-option) item-view))))
              (setf (active-item-of model) item-model))))))



(define-event-property
    (combo-box-change "change" :callback-data (list (cons "value"
                                                          (js-code-of (attribute-value-of widget))))))


(defmethod initialize-callback-box ((combo-box combo-box) (lisp-accessor-name (eql 'combo-box-change))
                                    (callback-box callback-box))
  (setf (argument-parser-of callback-box)
        (lambda (args)
          (list :item-view (get-widget (cdr (assoc "value" args :test #'string=)))))))





#|(progn
  (remove-all (root))
  (with (make-instance 'combo-box :id "blah")
    (let ((p2 (make-instance 'container-with-1-active-item :model (dbg-prin1 ~~it)))
          (b λV"b"))
      (insert λV"a" :in it)
      (insert b :in it)
      (insert λV"c" :in it)
      (setf (active-item-of ~it) b)
      (setf (active-item-of p2) b)
      (dbg-prin1 ~(active-item-of p2))
      (remove b it)
      (insert it :in (root))
      (dbg-prin1 ~(active-item-of p2))
    )))|#

#|(define-symbol-macro =blah=  ~(get-widget "blah"))|#

#|(remove (active-item-of =blah=) =blah=)|#