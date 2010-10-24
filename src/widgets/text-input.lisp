;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :widgets/text-input.lisp))


#| TODO:
As the user has begun to make changes to a TEXT-INPUT in his browser, it would be an interesting and useful feature
if his changes where not overwritten by messages from the server without his consent.

Some sort of feedback-mechanism would be useful here. An ability to let the user know things have changed since he
started editing -- and a way for him to update the TEXT-INPUT and drop his own current work in progress.
|#



(defclass text-input (widget focussable)
  ((clear-on-enterpress-p :accessor clear-on-enterpress-p-of :initarg :clear-on-enterpress-p
                          :type (member t nil)
                          :initform nil)

   (sync-on-blur-p :reader sync-on-blur-p-of :initarg :sync-on-blur-p
                   :initform ↑(not ¤clear-on-enterpress-p))

   (sync-on-enterpress-p :reader sync-on-enterpress-p-of :initarg :sync-on-enterpress-p
                         :initform t))

  (:default-initargs
   :element-type "input"
   :model λV""))


(defmethod initialize-instance :before ((text-input text-input) &key password-p)
  (push (cons "type" (if password-p "password" "text"))
        (slot-value text-input 'static-attributes)))


(define-event-property
    (enterpress "keyup"
                :callback-data (list (cons "value" (js-code-of (attribute-value-of widget))))))


(define-event-property
    (text-input-blur "blur"
                     :callback-data (list (cons "value" (js-code-of (attribute-value-of widget))))))


(defmethod js-before-check ((text-input text-input) (lisp-accessor-name (eql 'enterpress)))
  ;; No equality check done here; the enterpress event might be directly observed.
  "if(event.which == 13){ return true; } else { return false; }")


(defmethod js-before-check ((text-input text-input) (lisp-accessor-name (eql 'text-input-blur)))
  ;; Check if client-side content of TEXT-INPUT really has changed before sending update/event to the server.
  (catstr "if(event.currentTarget.sw_text_input_value == encodeURIComponent(event.currentTarget.value)){"
          "return false;"
          "}else{"
          "event.currentTarget.sw_text_input_value = encodeURIComponent(event.currentTarget.value);"
          "return true;"
          "}"))


(flet ((parse-client-args (args)
         (with (cdr (assoc "value" args :test #'string=))
           (check-type it string)
           (list :value it))))


  (defmethod initialize-callback-box ((text-input text-input) (lisp-accessor-name (eql 'text-input-blur))
                                      (callback-box callback-box))
    (setf (argument-parser-of callback-box) #'parse-client-args))


  (defmethod initialize-callback-box ((text-input text-input) (lisp-accessor-name (eql 'enterpress))
                                      (callback-box callback-box))
    (setf (argument-parser-of callback-box) #'parse-client-args)))


(defun text-input-update-client-cache (value-str text-input)
  (declare (string value-str)
           (text-input text-input))
  (run (catstr "$('#" (id-of text-input) "')[0].sw_text_input_value = \"" (url-encode value-str) "\";" +lf+)
       text-input))


(fflet ((value-marshaller (the function (value-marshaller-of 'attribute-value-of))))

  (defmethod set-model nconc ((text-input text-input) (model cell))
    (collecting
      ;; View → Model
      (when (sync-on-enterpress-p-of text-input)
        (collect
            (with-event (value) (on-event-enterpress text-input)
              (if (clear-on-enterpress-p-of text-input)
                  (pulse ~model value)
                  (setf ~model value)))))

      (when (sync-on-blur-p-of text-input)
        (collect
            (with-event (value) (on-event-text-input-blur text-input)
              (if (clear-on-enterpress-p-of text-input)
                  (pulse ~model value)
                  (setf ~model value)))))

      ;; Model → View
      (collect
          λI(let ((value-str (value-marshaller ~model)))
              (when-commit ()
                ;; TODO: To do this proper a maybe-update-client type thing + client side merge is needed.
                (setf (attribute-value-of text-input) value-str)
                (text-input-update-client-cache value-str text-input)))))))


(defmacro text-input (args &optional (value nil value-supplied-p))
  (with-gensyms (text-input)
    `(letp1 ((,text-input (make-instance 'text-input ,@args)))
       ,(when value-supplied-p
         `(setf (deref (model-of ,text-input)) ,value)))))