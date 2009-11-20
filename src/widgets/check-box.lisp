;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :widgets/check-box.lisp))


(defclass check-box (widget)
  ()

  (:default-initargs
   :element-type "input"))
(export 'check-box)


(defmethod initialize-instance :before ((check-box check-box) &key)
  (push (cons "type" "checkbox")
        (slot-value check-box 'static-attributes)))


(defmethod set-model nconc ((check-box check-box) (model cell))
  (list λI(with (value-of model)
            ;; Model → View.
            (when-commit ()
              (unless (eq it (checked-p-of check-box))
                (setf (checked-p-of check-box) it))))

        λI(when-let ((res (on-check-box-change-of check-box)))
            ;; View → Model.
            (with (car res)
              (when-commit ()
                (setf (checked-p-of check-box :server-only-p t) it))
              (setf ~~check-box it)))))


(define-event-property
    (on-check-box-change-of "change" :callback-data (list (cons "checked" (js-code-of (checked-p-of widget))))))


(defmethod initialize-callback-box ((check-box check-box) (lisp-accessor-name (eql 'on-check-box-change-of))
                                    (callback-box callback-box))
  (setf (argument-parser-of callback-box)
        (lambda (args)
          #| Wrap it in a LIST since SW-MVC:PULSE will "use NIL already". The recieving end must take care to
          unpack this (CAR). |#
          (list (with1 (string= "true" (cdr (assoc "checked" args :test #'string=))))))))



#|(progn
  (remove-all (root))
  (insert (make-instance 'check-box) :in (root)))|#
