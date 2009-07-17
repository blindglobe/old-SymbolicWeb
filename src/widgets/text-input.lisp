;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/text-input.lisp))


#| NOTE: Before wasting time here trying to ensure that the client that sent the update does not get a useless update
in return, make sure that you understand:

  * A single widget instance can be visible in multiple VIEWPORTs at the same time.
|#



(defclass text-input (widget focussable)
  ((enterpress-state :initform #λnil)
   (view-value :initform #λ""))

  (:default-initargs
   :element-type "input"
   :model #λ""))
(export 'text-input)


(defmethod initialize-instance :before ((text-input text-input) &key password-p)
  (push (cons "type" (if password-p "password" "text"))
        (slot-value text-input 'static-attributes)))


(defmethod initialize-instance :after ((text-input text-input) &key
                                       (sync-on-blur-p t)
                                       (sync-on-enterpress-p t))
  (when sync-on-blur-p
    (with-formula text-input
      (when-let (value (on-blur-of text-input))
        (setf ~~text-input value))))

  (when sync-on-enterpress-p
    (with-formula text-input
      (when-let (value (on-keyup-of text-input))
        (setf ~~text-input value)
        (pulse ~(slot-value text-input 'enterpress-state)
               (or ~~text-input t))))))


;; TODO: Think about this.
(flet ((parse-client-args (args)
         (cdr (assoc "value" args :test #'string=))))
  (let ((before-check
         (catstr "if(event.currentTarget.sw_text_input_value == event.currentTarget.value){"
                 " return(false);"
                 "}else{"
                 " event.currentTarget.sw_text_input_value = event.currentTarget.value;"
                 " return true;"
                 "}")))


    (defmethod initialize-callback-box ((text-input text-input) (lisp-accessor-name (eql 'on-blur-of)) callback-box)
      (setf (callback-data-of callback-box) `((:value . ,(js-code-of (value-of text-input))))
            (argument-parser-of callback-box) #'parse-client-args
            (js-before-of callback-box)
            before-check))


    (defmethod initialize-callback-box ((text-input text-input) (lisp-accessor-name (eql 'on-keyup-of)) callback-box)
      (setf (callback-data-of callback-box) `((:value . ,(js-code-of (value-of text-input))))
            (argument-parser-of callback-box) #'parse-client-args
            (js-before-of callback-box)
            (catstr "if(event.which == 13){ " before-check "}")))))


(defmethod enterpress-state-of ((text-input text-input))
  ~(slot-value text-input 'enterpress-state))
(export 'enterpress-state-of)


(defmethod (setf model-of) ((model cell) (text-input text-input))
  (let ((value-marshaller (value-marshaller-of 'value-of)))
    #λ(let ((value ~model))
        ;; Dodge cases where we can with 100% certainty determine that things are in sync at all viewports.
        (unless (string= (funcall value-marshaller value)
                         (funcall value-marshaller (value-of text-input)))
          (when-commit ()
            (setf (value-of text-input) value))))))



(defmacro mk-text-input ((&rest args) &optional (value nil value-supplied-p))
  (with-gensyms (text-input)
    `(letp1 ((,text-input (make-instance 'text-input ,@args)))
       ,(when value-supplied-p
         `(setf (deref (model-of ,text-input)) ,value)))))
(export 'mk-text-input)
