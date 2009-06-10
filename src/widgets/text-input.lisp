;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations))


(defclass text-input (widget)
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
  (setf (on-blur-of text-input
                    :callback-data `((:value . ,(js-code-of (value-of text-input)))))
        (mk-cb (text-input value)
          (unless (equal ~~text-input value)
            (setf ~~text-input value)))

        (on-keyup-of text-input
                     :js-before "if(event.which == 13) return true;"
                     :callback-data `((:value . ,(js-code-of (value-of text-input)))))
        (mk-cb (text-input value)
          (unless (equal ~~text-input value)
            (setf ~~text-input value))
          (setf ~(slot-value text-input 'enterpress-state) t
                ~(slot-value text-input 'enterpress-state) nil))))


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
  (with-object text-input
    (setf ¤formula
          #λ(let ((value ~model))
              (when-commit ()
                (setf (value-of text-input) value))))))


(defmacro mk-text-input ((&rest args) &optional (value nil value-supplied-p))
  (with-gensyms (text-input)
    `(letp1 ((,text-input (make-instance 'text-input ,@args)))
       ,(when value-supplied-p
         `(setf (deref (model-of ,text-input)) ,value)))))
(export 'mk-text-input)
