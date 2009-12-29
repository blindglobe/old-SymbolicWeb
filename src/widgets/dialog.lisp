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


(define-event-property (dialog-close "dialogclose"))


(defmethod js-before-check ((dialog dialog) (lisp-accessor-name (eql 'dialog-close)))
  (fmtn "if($('#~A').data('dialog-close-event-p')) return(true); else return(false);"
        (id-of dialog)))


(defmethod initialize-instance :after ((dialog dialog) &key)
  (with-event nil (on-event-dialog-close dialog)
    (remove dialog (parent-of dialog))))


(defmethod render :after ((dialog dialog))
  (let ((id (id-of dialog)))
    (run (fmtn "$('#~A').data('gc-fn', function(){ $('#~A').dialog('destroy').remove(); });~%" id id)
         dialog)
    (run (fmtn "$('#~A').dialog({ autoOpen: false });~%" id)
         dialog)
    ;; So DIALOGs don't "jump around" when the DOM tree is manipulated. TODO: This should probably be removed later.
    (run (fmtn "$('#~A').parent().css('position', 'fixed');~%" id)
         dialog)))


(flet ((set-option (dialog option-name value)
         (if (in-dom-p-of dialog)
             (run (fmtn "$('#~A').dialog('option', '~A', decodeURIComponent('~A'));~%"
                        (id-of dialog)
                        option-name
                        (htmlize value))
                  dialog)
             (error "TODO: Do the ADD-DELAYED-OPERATION thing here."))))


  (defmethod set-model nconc ((dialog dialog) (model dialog-model))
    (list λI(when-let (title (title-of model))
              (when-commit ()
                (set-option dialog "title" title))))))


;; TODO: It sucks having to repeat some of the tricky code from ABSTRACT-CONTAINER here.
(defmethod handle-model-event ((dialog-model dialog-model) container (event sw-mvc:container-insert))
  (let ((dialog-view (view-in-context-of container dialog-model t)))
    (check-type dialog-view dialog)
    (when-commit ()
      (amx:insert dialog-view ↺(slot-value container 'children) :last-p t)
      (when (visible-p-of container)
        (propagate-for-add dialog-view container)
        (unless (in-dom-p-of dialog-view)
          (run (js-iappend (shtml-of dialog-view) (id-of container)) container)
          (render dialog-view)
          (ensure-in-client-dom dialog-view))
        (run (fmtn "$('#~A').data('dialog-close-event-p', true).dialog('open');~%" (id-of dialog-view))
             dialog-view)))))


(defmethod handle-model-event ((dialog-model dialog-model) container (event sw-mvc:container-remove))
  (let ((dialog-view (view-in-context-of container dialog-model)))
    (check-type dialog-view dialog)
    (when-commit ()
      (when (visible-p-of container)
        (run (fmtn "$('#~A').data('dialog-close-event-p', false).dialog('close');~%" (id-of dialog-view))
             dialog-view)
        (propagate-for-remove dialog-view)))))
