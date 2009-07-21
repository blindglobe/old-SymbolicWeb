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
        (nilf (flow-back-to-origin-p-of *current-event*))
        (setf ~~text-input value))))

  (when sync-on-enterpress-p
    (with-formula text-input
      (when-let (value (on-keyup-of text-input))
        (nilf (flow-back-to-origin-p-of *current-event*))
        (setf ~~text-input value)
        (pulse ~(slot-value text-input 'enterpress-state)
               (or ~~text-input t))))))


;; TODO: Think about this.
(flet ((parse-client-args (args)
         (cdr (assoc "value" args :test #'string=))))
  (let ((before-check
         ;; TODO: This should probably be placed in or somewhere around the dom-cache.lisp stuff.
         """
if(event.currentTarget.sw_text_input_value == event.currentTarget.value){
  return(false);
}else{
  event.currentTarget.sw_text_input_value = event.currentTarget.value;
  return true;
}"""))


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


(defmethod render ((text-input text-input))
  (declare (optimize speed (safety 2)))
  ;; TODO: This code is repeated in (SETF MODEL-OF) below and should probably be placed in or somewhere around the
  ;; dom-cache.lisp stuff.
  (run (catstr "$('#" (id-of text-input) "')[0].sw_text_input_value = \""
               (funcall (the function (value-marshaller-of 'value-of)) (value-of text-input)) "\";")
       text-input))


(defmethod enterpress-state-of ((text-input text-input))
  ~(slot-value text-input 'enterpress-state))
(export 'enterpress-state-of)


(defmethod (setf model-of) ((model cell) (text-input text-input))
  (declare (optimize speed (safety 2)))
  (fflet ((value-marshaller (the function (value-marshaller-of 'value-of))))
    ;; NOTE: We do not assign anything to (EQUAL-P-FN-OF MODEL) here because objects that have the same printed
    ;; representation (TEXT-INPUTs VALUE-MARSHALLER is really just PRINC-TO-STRING) might not actually be equal
    ;; at all wrt. other stuff depending on MODEL. We do the check (STRING=) below, or later, instead.
    #λ(let ((new-value (value-marshaller ~model)))
        (unless (string= new-value (value-marshaller (value-of text-input)))
          (let ((except-viewport (withp (maybe-except-viewport text-input)
                                   (muffle-compiler-note
                                     ;; TODO: This is a weak point in the API. We don't know what event this actually
                                     ;; is, so we don't know what PARSED-ARGS-OF will return if the user defines his
                                     ;; own events for his TEXT-INPUT instance (or creates a subclass etc.).
                                     (string= new-value (parsed-args-of *current-event*))))))
            (when-commit ()
              (setf (value-of text-input :except-viewport except-viewport) new-value)
              (run (catstr "$('#" (id-of text-input) "')[0].sw_text_input_value = \"" new-value "\";")
                   text-input :except-viewport except-viewport)))))))



(defmacro mk-text-input ((&rest args) &optional (value nil value-supplied-p))
  (with-gensyms (text-input)
    `(letp1 ((,text-input (make-instance 'text-input ,@args)))
       ,(when value-supplied-p
         `(setf (deref (model-of ,text-input)) ,value)))))
(export 'mk-text-input)
