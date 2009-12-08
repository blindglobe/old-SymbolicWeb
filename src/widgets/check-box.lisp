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
    (check-box-change "change"
                      :callback-data (list (cons "checked"
                                                 (js-code-of (attribute-checked-p-of widget))))))


(defmethod initialize-instance :before ((check-box check-box) &key)
  (push (cons "type" "checkbox")
        (slot-value check-box 'static-attributes)))


(defmethod set-model nconc ((check-box check-box) (model cell))
  ;; View → Model.
  (defmethod on-check-box-change ((widget (eql check-box)) &key checked-p)
    (setf ~model checked-p))
  (activate-event 'check-box-change check-box)

  ;; Model → View.
  (list λI(with ~model
            (when-commit ()
              (setf (attribute-checked-p-of check-box)
                    it)))))


(defmethod initialize-callback-box ((check-box check-box) (lisp-accessor-name (eql 'check-box-change))
                                    (callback-box callback-box))
  (setf (argument-parser-of callback-box)
        (lambda (args)
          (list :checked-p (string= "true" (cdr (assoc "checked" args :test #'string=)))))))


#|(with (make-instance 'check-box)
  (remove-all (root))
  (insert it :in (root)))|#