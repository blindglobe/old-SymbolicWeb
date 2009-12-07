;;;; http://nostdal.org/ ;;;;

(in-package :sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :widgets/events.lisp))


(defclass event ()
  ((callback-box :reader callback-box-of :initarg :callback-box
                 :type callback-box)

   (parsed-args :reader parsed-args-of :initarg :parsed-args))

  (:documentation "
Instances of this is bound to *CURRENT-EVENT*."))


(defmethod widget-of ((event event))
  (widget-of (callback-box-of event)))


(defmethod id-of ((event event))
  (id-of (callback-box-of event)))


(defmacro with-event (bindings event-extr &body handler)
  "Usage:
  (with-event (value) (on-event-enterpress it)
    (dbg-prin1 value))"
  (with-gensyms (args)
    `(with-observer ((,args ,event-extr))
       (when ,args
         (destructuring-bind (&key ,@bindings &allow-other-keys) ,args
           ,@handler)))))


(defclass callback-box ()
  ((id :reader id-of :initarg :id
       :type string
       :initform (id-generator-next-str -id-generator-))

   (widget :reader widget-of :initarg :widget
           :type widget-base
           :initform (error ":WIDGET needed."))

   (event-name :reader event-name-of :initarg :event-name
               :type symbol
               :initform (error ":EVENT-NAME needed."))

   (observer-cell :reader observer-cell-of :initarg :observer-cell
                  :type cell
                  :initform (error ":OBSERVER-CELL needed."))

   (event-cell :reader event-cell-of :initarg :event-cell
               :type cell
               :initform λVnil)

   (argument-parser :accessor argument-parser-of :initarg :argument-parser
                    :type function
                    :initform #'identity)

   (code :accessor code-of :initarg :code
         :type (or null string)
         :initform nil)))


(defmethod initialize-instance :after ((callback-box callback-box) &key)
  ;; STORE-CALLBACK-BOX
  (setf (gethash (id-of callback-box) (callbacks-of (widget-of callback-box)))
        callback-box))


(defun remove-callback-box (callback-box widget)
  (declare (callback-box callback-box)
           (widget-base widget))
  (remhash (id-of callback-box) (callbacks-of widget)))


(defun find-callback-box (widget-id callback-id app)
  (declare (string widget-id callback-id)
           (application app))
  (or (when-let (widget (gethash widget-id (widgets-of app)))
        (gethash callback-id (callbacks-of widget)))
      (gethash callback-id (callbacks-of *viewport*))))


;; TODO: I think I need to implement something using this http://docs.jquery.com/Namespaced_Events
;; to do this proper.
#|(defun event-remove (event-type widget &key server-only-p)
  (declare (string event-type)
           (widget widget)
           (ignore server-only-p))
  (declare (ignore event-type widget))
  #|(setf (event event-type widget) nil)|#
  #|(remove-callback-box event-type widget)|#)|#


(defmethod js-before-check ((widget widget-base) (lisp-accessor-name symbol))
  "return true;")


(defmethod initialize-callback-box ((widget widget-base) (lisp-accessor-name symbol) (callback-box callback-box))
  )


(defgeneric on-event (event widget &rest args)
  (:method-combination progn))


(defmethod on-event :around (event widget &rest args)
  ;; Forward to something that might want to use another method-combination than the "brute force" PROGN used here.
  (let* ((gf-name (intern (format nil "ON-~A" event) (symbol-package event)))
         (gf (and (fboundp gf-name)
                  (symbol-function gf-name))))
    (when gf
      (apply gf widget (append args '(:allow-other-keys t)))))
  (call-next-method))


;; So the APPLY in EXECUTE-CALLBACK (below) always finds at least on applicable method.
(defmethod on-event progn (event widget &rest args)
  (declare (ignore args)))


(defun execute-callback (callback-box args)
  (declare (callback-box callback-box)
           (list args))
  (let ((*current-event* (make-instance 'event :callback-box callback-box)))
    (pulse (cell-deref (event-cell-of callback-box))
           (with1 (or (funcall (argument-parser-of callback-box) args)
                      ;; (&KEY &ALLOW-OTHER-KEYS) in WITH-EVENT needs this as a default fallback.
                      '(:dummy t))
             (setf (slot-value *current-event* 'parsed-args) it)
             (apply #'on-event (event-name-of callback-box) (widget-of callback-box)
                    it)))))


;; (SETF EVENT)
(defun event-dom-client-writer (widget cb js-code-fn &rest args &key lisp-name)
  (declare (widget-base widget)
           (callback-box cb)
           ((or null symbol) lisp-name))
  (let ((js-code (or (code-of cb)
                     (setf (code-of cb) (funcall js-code-fn)))))
    (if *js-code-only-p*
        js-code
        (if (in-dom-p-of widget)
            (apply #'run js-code widget args)
            (progn
              (assert lisp-name)
              (add-delayed-operation widget lisp-name
                                     λλ(apply #'run js-code widget args)))))))


(defun event-dom-server-reader (widget lisp-accessor-name event-target-gf-name)
  (declare (widget-base widget)
           (symbol lisp-accessor-name event-target-gf-name))
  (if (init-evalp-of =cell=)
      (let ((callback-box (callback-box-of *current-event*)))
        (assert (eq =cell= (observer-cell-of callback-box)))
        (values (cell-deref (event-cell-of callback-box))
                t))
      (let ((callback-box (make-instance 'callback-box
                                         :widget widget :observer-cell =cell=
                                         :event-name event-target-gf-name)))
        (initialize-callback-box widget event-target-gf-name callback-box)
        (funcall (fdefinition `(setf ,lisp-accessor-name)) callback-box widget
                 :lisp-name lisp-accessor-name)
        (values (cell-deref (event-cell-of callback-box))
                nil))))


#|(defmethod add-method ((gf (eql #'on-click)) method)
  (let ((widget nil)) ;; TODO: Check if we're specializing on a widget instance.
    (when widget
      (activate-event 'click widget)))
  (call-next-method))|#


#|(defmethod add-method ((gf (eql #'on-event)) method)
  (let ((widget nil)) ;; TODO: Check if we're specializing on a widget instance.
    (when widget
      (let ((event nil)) ;; TODO: Check if we're specializing on a particular event type.
        (when (symbolp event)
          (activate-event event widget)))))
  (call-next-method))|#


(defmacro define-event-property ((lisp-name event-type &rest js-msg-args) &rest args) ;; CLICK
  (let ((event-target-gf-name (symbolicate 'on- lisp-name))) ;; ON-CLICK
    `(progn

       #| TODO: ಠ_ಠ, and anyways the need for "manually" calling (an improved version of) this could probably
       be replaced by ADD-METHOD methods. |#
       (defmethod activate-event ((event (eql ',lisp-name)) (widget widget-base))
         (with-event nil (,(symbolicate 'on-event- lisp-name) widget)
           #|(write-line "dummy")|#))

       (define-dom-property ',(symbolicate 'on-event- lisp-name) ;; ON-EVENT-CLICK
         :dom-client-writer
         (lambda (cb widget &rest args)
           (declare (callback-box cb)
                    (widget-base widget))
           (apply #'event-dom-client-writer widget cb
                  (lambda ()
                    (js-bind (id-of widget) ,event-type
                             (js-msg (id-of widget) (id-of cb)
                                     :context-sym 'event
                                     :js-before (js-before-check widget ',lisp-name)
                                     ,@js-msg-args)))
                  args))

         :dom-server-reader
         (lambda (widget lisp-accessor-name)
           (funcall 'event-dom-server-reader
                    widget lisp-accessor-name ',lisp-name))

         :value-marshaller nil
         :value-removal-checker nil
         ,@args))))



(define-event-property (blur "blur"))
(define-event-property (change "change"))
(define-event-property (click "click"))
(define-event-property (dblclick "dblclick"))
(define-event-property (focus "focus"))

(let ((js-which "event.which"))
  (define-event-property (keyup "keydown" :callback-data (list (cons "which" js-which))))
  (define-event-property (keyup "keypress" :callback-data (list (cons "which" js-which))))
  (define-event-property (keyup "keyup" :callback-data (list (cons "which" js-which)))))

(define-event-property (load "load"))
(define-event-property (mousedown "mousedown"))
(define-event-property (mousemove "mousemove"))
(define-event-property (mouseout "mouseout"))
(define-event-property (mouseover "mouseover"))
(define-event-property (mouseup "mouseup"))
(define-event-property (resize "resize"))
(define-event-property (scroll "scroll"))
(define-event-property (select "select"))
(define-event-property (unload "unload"))
