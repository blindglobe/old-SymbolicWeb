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


(defmacro with-event (binding-event &body handler)
  "Syntax:

  (with-event (on-click-of some-button)
    ..)

  (with-event ((value (on-enterpress-of some-text-input)))
    ..)"
  `(with-observer ,binding-event
     ,@handler))



(defclass callback-box ()
  ((id :reader id-of :initarg :id
       :type string
       :initform (id-generator-next-str -id-generator-))

   (widget :reader widget-of :initarg :widget
           :type widget-base
           :initform (error ":WIDGET needed."))

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


(defun execute-callback (callback-box args)
  (declare (callback-box callback-box)
           (list args))
  (let ((*current-event* (make-instance 'event :callback-box callback-box)))
    (pulse (cell-deref (event-cell-of callback-box))
           (with1 (or (funcall (argument-parser-of callback-box) args) t)
             (setf (slot-value *current-event* 'parsed-args) it)))))


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


;; (SETF EVENT)
(defun event-dom-client-writer (widget cb js-code-fn &rest args)
  (let ((js-code (or (code-of cb)
                     (setf (code-of cb) (funcall js-code-fn)))))
    (if *js-code-only-p*
        js-code
        (if (in-dom-p-of widget)
            (apply #'run js-code widget args)
            (add-delayed-operation widget :event "click"
                                   λλ(apply #'run js-code widget args))))))


(defmethod initialize-callback-box ((widget widget-base) (lisp-accessor-name symbol) (callback-box callback-box))
  )


(defun event-dom-server-reader (widget lisp-accessor-name)
  (declare (widget-base widget)
           (symbol lisp-accessor-name))
  (if (init-evalp-of =cell=)
      (let ((callback-box (callback-box-of *current-event*)))
        (assert (eq =cell= (observer-cell-of callback-box)))
        (values (cell-deref (event-cell-of callback-box))
                t))
      (let ((callback-box (make-instance 'callback-box :widget widget :observer-cell =cell=)))
        (initialize-callback-box widget lisp-accessor-name callback-box)
        (funcall (fdefinition `(setf ,lisp-accessor-name)) callback-box widget)
        (values (cell-deref (event-cell-of callback-box))
                nil))))


(defmacro define-event-property ((lisp-name event-type &rest js-msg-args) &rest args)
  `(progn
     (define-dom-property ',lisp-name
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
       :dom-server-reader #'event-dom-server-reader
       :value-marshaller nil
       :value-removal-checker nil
       ,@args)))


(define-event-property (on-blur-of "blur"))
(define-event-property (on-change-of "change"))
(define-event-property (on-click-of "click"))
(define-event-property (on-dblclick-of "dblclick"))
(define-event-property (on-focus-of "focus"))

(let ((js-which "event.which"))
  (define-event-property (on-keyup-of "keydown" :callback-data (list (cons "which" js-which))))
  (define-event-property (on-keyup-of "keypress" :callback-data (list (cons "which" js-which))))
  (define-event-property (on-keyup-of "keyup" :callback-data (list (cons "which" js-which)))))

(define-event-property (on-load-of "load"))
(define-event-property (on-mousedown-of "mousedown"))
(define-event-property (on-mousemove-of "mousemove"))
(define-event-property (on-mouseout-of "mouseout"))
(define-event-property (on-mouseover-of "mouseover"))
(define-event-property (on-mouseup-of "mouseup"))
(define-event-property (on-resize-of "resize"))
(define-event-property (on-scroll-of "scroll"))
(define-event-property (on-select-of "select"))
(define-event-property (on-unload "unload"))
