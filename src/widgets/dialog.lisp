;;;; http://nostdal.org/ ;;;;

(in-package :symbolicweb)
(in-readtable symbolicweb)
(declaim #.(optimizations :widgets/dialog.lisp))


(defclass dialog (container)
  ())


(define-event-property (on-dialog-close "dialogclose"))


(defmethod initialize-instance :after ((dialog dialog) &key)
  (with-event (on-dialog-close dialog)
    (remove dialog (parent-of dialog))))


(defmethod render :after ((dialog dialog))
  (run (fmtn "$('#~A').dialog();" (id-of dialog))
       dialog))
