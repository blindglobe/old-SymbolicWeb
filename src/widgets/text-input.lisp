;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/text-input.lisp))


(defclass text-input (widget focussable)
  ((enterpress-state :initform #~nil))

  (:default-initargs
   :element-type "input"
   :model #~""))
(export 'text-input)


(defmethod initialize-instance :before ((text-input text-input) &key password-p)
  (push (cons "type" (if password-p "password" "text"))
        (slot-value text-input 'static-attributes)))


(defmethod initialize-instance :after ((text-input text-input) &key
                                       (sync-on-blur-p t)
                                       (sync-on-enterpress-p t))
  (when sync-on-blur-p
    (with-lifetime text-input
      #λ(when-let (value (on-blur-of text-input))
          (setf (value-of text-input :server-only-p t) value)
          (setf ~~text-input value))))

  (when sync-on-enterpress-p
    (with-lifetime text-input
      #λ(when-let (value (on-keyup-of text-input))
          (setf (value-of text-input :server-only-p t) value)
          (let ((model-value (setf ~~text-input value)))
            (pulse ~(slot-value text-input 'enterpress-state)
                   (or model-value t)))))))


;; TODO: Think about this.
(flet ((parse-client-args (args)
         (cdr (assoc "value" args :test #'string=))))

  (defmethod initialize-callback-box ((text-input text-input) (lisp-accessor-name (eql 'on-blur-of)) callback-box)
    ;; TODO: Client side closure to avoid submitting something that haven't changed.
    (setf (callback-data-of callback-box) `((:value . ,(js-code-of (value-of text-input))))
          (argument-parser-of callback-box) #'parse-client-args))

  (defmethod initialize-callback-box ((text-input text-input) (lisp-accessor-name (eql 'on-keyup-of)) callback-box)
    ;; TODO: Client side closure to avoid submitting something that haven't changed.
    (setf (callback-data-of callback-box) `((:value . ,(js-code-of (value-of text-input))))
          (js-before-of callback-box) "if(event.which == 13) return true;"
          (argument-parser-of callback-box) #'parse-client-args)))


(defmethod enterpress-state-of ((text-input text-input))
  ~(slot-value text-input 'enterpress-state))
(export 'enterpress-state-of)


(defmethod (setf model-of) ((model cell) (text-input text-input))
  #λ(let ((model-value ~model))
      (when-commit ()
        (setf (value-of text-input) model-value))))


(defmacro mk-text-input ((&rest args) &optional (value nil value-supplied-p))
  (with-gensyms (text-input)
    `(letp1 ((,text-input (make-instance 'text-input ,@args)))
       ,(when value-supplied-p
         `(setf (deref (model-of ,text-input)) ,value)))))
(export 'mk-text-input)
