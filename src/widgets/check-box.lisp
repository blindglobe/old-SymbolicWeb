;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :widgets/check-box.lisp))


(defclass check-box (widget)
  ()

  (:default-initargs
   :element-type "input"
   :model λVnil))


(define-event-property
    (check-box-change "change" :callback-data (list (cons "checked"
                                                          (js-code-of (attribute-checked-p-of widget))))))


(defmethod initialize-instance :before ((check-box check-box) &key)
  (push (cons "type" "checkbox")
        (slot-value check-box 'static-attributes)))


(defmethod set-model nconc ((check-box check-box) (model cell))
  (list λI(with (value-of model)
            ;; Model → View.
            (when-commit ()
              (unless (eq it (attribute-checked-p-of check-box))
                (setf (attribute-checked-p-of check-box) it))))

        (with-event (new-state) (on-event-check-box-change check-box)
          ;; View → Model.
          (with (car new-state)
            (when-commit ()
              (setf (attribute-checked-p-of check-box :server-only-p t) it))
            (setf (value-of model) it)))))


(defmethod initialize-callback-box ((check-box check-box) (lisp-accessor-name (eql 'check-box-change))
                                    (callback-box callback-box))
  (setf (argument-parser-of callback-box)
        (lambda (args)
          #| Wrap it in a LIST since SW-MVC:PULSE will "use NIL already". The recieving end must take care to
          unpack this (CAR). |#
          (list (string= "true" (cdr (assoc "checked" args :test #'string=)))))))



#|(progn
  (remove-all (root))
  (insert (make-instance 'check-box) :in (root)))|#


#|(progn
  (defvar *check-box-model* λVnil)
  (defmethod render-viewport ((viewport viewport) (app empty-page-app))
    (insert (make-instance 'check-box :model *check-box-model*)
            :in (root))))|#
