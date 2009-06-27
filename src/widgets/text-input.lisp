;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/text-input.lisp))


(defclass text-input (widget focussable)
  (#|(previous-value :initform "")|#
   (enterpress-state :initform #~nil))

  (:default-initargs
   :element-type "input"
   :model #~""))
(export 'text-input)


(defmethod initialize-instance :before ((text-input text-input) &key password-p)
  (push (cons "type" (if password-p "password" "text"))
        (slot-value text-input 'static-attributes)))


(defmethod initialize-instance :after ((text-input text-input) &key)
  (flet ((handle-input (value)
           (setf (value-of text-input :server-only-p t) value)
           ;; TODO: Move this to the Model end?
           (let ((model ~text-input))
             (unless (equal ~model (if-let (input-translator (input-translator-of model))
                                     (setf value ;; For return value of HANDLE-INPUT; used by PULSE below.
                                           (funcall input-translator value))
                                     value))
               (setf ~model value)))
           value))
    λ(when-let (value (on-blur-of text-input))
       (handle-input value))
    λ(when-let (value (on-keyup-of text-input))
       (pulse ~(slot-value text-input 'enterpress-state)
              (handle-input value)))))


;; TODO: Think about this.
(flet ((parse-client-args (args)
         (cdr (assoc "value" args :test #'string=))))

  (defmethod initialize-callback-box ((text-input text-input) (lisp-accessor-name (eql 'on-blur-of)) callback-box)
    (setf (callback-data-of callback-box) `((:value . ,(js-code-of (value-of text-input))))
          (argument-parser-of callback-box) #'parse-client-args))

  (defmethod initialize-callback-box ((text-input text-input) (lisp-accessor-name (eql 'on-keyup-of)) callback-box)
    (setf (callback-data-of callback-box) `((:value . ,(js-code-of (value-of text-input))))
          (js-before-of callback-box) "if(event.which == 13) return true;"
          (argument-parser-of callback-box) #'parse-client-args)))


(defmethod enterpress-state-of ((text-input text-input))
  ~(slot-value text-input 'enterpress-state))
(export 'enterpress-state-of)


#|(defmethod changed-p-of ((text-input text-input))
  "This will return T if TEXT-INPUT has changed since last time this function was called."
  (let ((current-value (value-of text-input)))
    (with-slots (previous-value) text-input
      (when (not (string= current-value previous-value))
        (setf previous-value current-value)))))|#
#|(export 'changed-p-of)|#


#|(defmethod (setf value-of) :after (new-value (text-input text-input) &rest args)
  (declare (ignore args))
  (when (not (string= *old-value* new-value)) ;; TODO: Magic function STRING=.
    (handle-address-bar text-input)))|#
#|(export 'value-of)|#


#|(defmethod uri-value<-state ((text-input text-input))
  (html<- (value-of text-input) text-input))|#


#|(defmethod state<-uri-value ((uri-value string) (text-input text-input) from-browser-history-p)
  (setf (value-of text-input) uri-value))|#


(defmethod (setf model-of) ((model single-value-model) (text-input text-input))
  (add-formula text-input
               λ(let ((value ~model))
                  (when-commit ()
                    (setf (value-of text-input) value)))))


(defmacro mk-text-input ((&rest args) &optional (value nil value-supplied-p))
  (with-gensyms (text-input)
    `(letp1 ((,text-input (make-instance 'text-input ,@args)))
       ,(when value-supplied-p
         `(setf (deref (model-of ,text-input)) ,value)))))
(export 'mk-text-input)
