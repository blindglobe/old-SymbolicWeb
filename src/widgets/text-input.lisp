;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

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
                          :initform nil))

  (:default-initargs
   :element-type "input"
   :model #λ""))
(export 'text-input)


(define-event-property
    (on-enterpress-of "keyup" :callback-data (list (cons "value" (js-code-of (value-of widget))))))

(define-event-property
    (on-text-input-blur-of "blur" :callback-data (list (cons "value" (js-code-of (value-of widget))))))


(defmethod initialize-instance :before ((text-input text-input) &key password-p)
  (push (cons "type" (if password-p "password" "text"))
        (slot-value text-input 'static-attributes)))


(defmethod initialize-instance :after ((text-input text-input) &key
                                       (sync-on-blur-p (not (clear-on-enterpress-p-of text-input)))
                                       (sync-on-enterpress-p t))
  (when sync-on-blur-p
    (with-formula text-input
      (when-let (value (on-text-input-blur-of text-input))
        (setf ~~text-input value))))

  (when sync-on-enterpress-p
    (with-formula text-input
      (when-let (value (on-enterpress-of text-input))
        (setf ~~text-input value)

        (when (clear-on-enterpress-p-of text-input)
          (setf (value-of text-input :client-only-p t) "")
          (text-input-update-client-cache "" text-input))))))


(let ((js ;; Check if client-side content of TEXT-INPUT really has changed before sending update to the server.
       (catstr "if(event.currentTarget.sw_text_input_value == encodeURIComponent(event.currentTarget.value)){"
               "return false;"
               "}else{"
               "event.currentTarget.sw_text_input_value = encodeURIComponent(event.currentTarget.value);"
               "return true;"
               "}")))
  (defmethod js-before-check ((text-input text-input) (lisp-accessor-name (eql 'on-text-input-blur-of)))
    js)


  (defmethod js-before-check ((text-input text-input) (lisp-accessor-name (eql 'on-enterpress-of)))
    (catstr "if(event.which != 13){ return false; }"
            js)))


(flet ((parse-client-args (args)
         (cdr (assoc "value" args :test #'string=))))


  (defmethod initialize-callback-box ((text-input text-input) (lisp-accessor-name (eql 'on-text-input-blur-of))
                                      (callback-box callback-box))
    (setf (argument-parser-of callback-box) #'parse-client-args))


  (defmethod initialize-callback-box ((text-input text-input) (lisp-accessor-name (eql 'on-enterpress-of))
                                      (callback-box callback-box))
    (setf (argument-parser-of callback-box) #'parse-client-args)))


(defun text-input-update-client-cache (value-str text-input)
  (declare (string value-str)
           (text-input text-input))
  (declare (optimize speed (safety 2)))
  (run (catstr "$('#" (id-of text-input) "')[0].sw_text_input_value = \"" (url-encode value-str) "\";" +lf+)
       text-input))


(fflet ((value-marshaller (the function (value-marshaller-of 'value-of))))


  (defmethod render ((text-input text-input))
    (declare (optimize speed (safety 2)))
    (text-input-update-client-cache (value-marshaller (value-of text-input)) text-input))


  (defmethod (setf model-of) ((model cell) (text-input text-input))
    (declare (optimize speed (safety 2)))
    #| We do not assign anything to (EQUAL-P-FN-OF MODEL) here because objects that have the same printed
    representation (the VALUE-MARSHALLER of VALUE-OF is really just PRINC-TO-STRING) might not actually be equal
    at all wrt. other stuff (CELLS) depending on MODEL. We do the check (STRING=) below, or later, instead. |#
    #λ(let* ((value ~model)
             (value-str (value-marshaller value)))
        (unless (muffle-compiler-note
                  (string= value-str (value-marshaller (value-of text-input))))
          (when-commit ()
            (setf (value-of text-input) value)
            (text-input-update-client-cache value-str text-input))))))



(defmacro text-input (args &optional (value nil value-supplied-p))
  (with-gensyms (text-input)
    `(letp1 ((,text-input (make-instance 'text-input ,@args)))
       ,(when value-supplied-p
         `(setf (deref (model-of ,text-input)) ,value)))))