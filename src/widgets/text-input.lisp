;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/text-input.lisp))


(defclass text-input (widget focussable)
  ((clear-on-enterpress-p :accessor clear-on-enterpress-p-of :initarg :clear-on-enterpress-p
                          :type (member t nil)
                          :initform nil))

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
      (when-let (value (on-text-input-blur-of text-input))
        (setf ~~text-input value))))

  (when sync-on-enterpress-p
    (with-formula text-input
      (when-let (value (on-enterpress-of text-input))
        (setf ~~text-input value)
        (when (clear-on-enterpress-p-of text-input)
          (push (lambda () (setf ~~text-input ""))
                (do-at-end-of *viewport*)))))))



(let ((js ;; Check if client-side content of TEXT-INPUT really has changed before sending update to the server.
       '(if (= (slot-value (slot-value event 'current-target) 'sw_text_input_value)
               (slot-value (slot-value event 'current-target) 'value))
         (return false)
         (progn
           (setf (slot-value (slot-value event 'current-target) 'sw_text_input_value)
                 (slot-value (slot-value event 'current-target) 'value))
           (return t)))))
  (defmethod js-before-check ((text-input text-input) (lisp-accessor-name (eql 'on-text-input-blur-of)))
    `(lambda () ,js))

  (defmethod js-before-check ((text-input text-input) (lisp-accessor-name (eql 'on-enterpress-of)))
    `(lambda ()
       (unless (= (slot-value event 'which) 13)
         (return false))
       ,js)))


(define-event-property
    (on-enterpress-of "keyup" :callback-data (list (cons "value" (js-code-of (value-of widget))))))

(define-event-property
    (on-text-input-blur-of "blur" :callback-data (list (cons "value" (js-code-of (value-of widget))))))


(flet ((parse-client-args (args)
         (cdr (assoc "value" args :test #'string=))))

  (defmethod initialize-callback-box ((text-input text-input) (lisp-accessor-name (eql 'on-text-input-blur-of))
                                      (callback-box callback-box))
    (setf (argument-parser-of callback-box) #'parse-client-args))

  (defmethod initialize-callback-box ((text-input text-input) (lisp-accessor-name (eql 'on-enterpress-of))
                                      (callback-box callback-box))
    (setf (argument-parser-of callback-box) #'parse-client-args)))


(flet ((update-client-cache (new-value widget)
         (run (catstr "$('#" (id-of widget) "')[0].sw_text_input_value = \"" new-value "\";")
              widget)))
  (declare (inline update-client-cache))
  (fflet ((value-marshaller (the function (value-marshaller-of 'value-of))))

    (defmethod render ((text-input text-input))
      (declare (optimize speed (safety 2)))
      (update-client-cache (value-marshaller (value-of text-input)) text-input))

    (defmethod (setf model-of) ((model cell) (text-input text-input))
      (declare (optimize speed (safety 2)))
      #| We do not assign anything to (EQUAL-P-FN-OF MODEL) here because objects that have the same printed
      representation (the VALUE-MARSHALLER of VALUE-OF is really just PRINC-TO-STRING) might not actually be equal
      at all wrt. other stuff (CELLS) depending on MODEL. We do the check (STRING=) below, or later, instead. |#
      #λ(let ((new-value (value-marshaller ~model)))
          (unless (string= new-value (value-marshaller (value-of text-input)))
            (when-commit ()
              ;; Update UI.
              (setf (value-of text-input) new-value)
              (update-client-cache new-value text-input)))))))



(defmacro mk-text-input ((&rest args) &optional (value nil value-supplied-p))
  (with-gensyms (text-input)
    `(letp1 ((,text-input (make-instance 'text-input ,@args)))
       ,(when value-supplied-p
         `(setf (deref (model-of ,text-input)) ,value)))))
(export 'mk-text-input)
