;;;; http://nostdal.org/ ;;;;

(in-package :symbolicweb)
(in-readtable symbolicweb)
(declaim #.(optimizations :widgets/dialog.lisp))


(defclass dialog-model (dlist)
  ((title :accessor title-of
          :initform nil))

  (:metaclass mvc-class))


(defclass dialog (container)
  ()

  (:default-initargs
   :model (make-instance 'dialog-model)))


(define-event-property (on-dialog-close "dialogclose"))


(defmethod initialize-instance :after ((dialog dialog) &key)
  (with-event (on-dialog-close dialog)
    (remove dialog (parent-of dialog))))


(flet ((set-option (dialog option-name value)
         (run (fmtn "$('#~A').dialog('option', '~A', decodeURIComponent('~A'));~%"
                    (id-of dialog)
                    option-name
                    (htmlize value))
              dialog)))


  (defmethod set-model nconc ((dialog dialog) (model dialog-model))
    (list λI(when-let (title (title-of model))
              (set-option dialog "title" title)))))


(defmethod render :after ((dialog dialog))
  (run (fmtn "$('#~A').dialog();~%" (id-of dialog))
       dialog))
