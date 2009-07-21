;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/events.lisp))


(defclass event ()
  ((callback-box :reader callback-box-of :initarg :callback-box
                 :type callback-box)

   (viewport :reader viewport-of :initarg :viewport
             :type viewport)

   (parsed-args :reader parsed-args-of :initarg :parsed-args))

  (:documentation "
Instances of this is bound to *CURRENT-EVENT*."))


(defmethod widget-of ((event event))
  (widget-of (callback-box-of event)))


(defun maybe-except-viewport (widget)
  "If WIDGET is the widget that caused the event (e.g. user-input) then this'll
return the VIEWPORT where that event originated from. This is meant to be passed
to the :EXCEPT-VIEWPORT keyarg of RUN and is useful to avoid having user-input
wrt. a TEXT-INPUT widget \"race with itself\" as it comes back from the Model
end."
  (declare (widget widget))
  (the (values (or null viewport) &optional)
    (with *current-event*
      (and it
           (eq widget (widget-of it))
           (viewport-of it)))))



(defclass callback-box ()
  ((id :reader id-of)

   (widget :accessor widget-of
           :type widget)

   (event-cell :reader event-cell-of :initarg :event-cell
               :type cell
               :initform #~nil)

   (event-type :reader event-type-of :initarg :event-type
               :type string
               :initform (error ":EVENT-TYPE needed."))

   (argument-parser :accessor argument-parser-of :initarg :argument-parser
                    :type function
                    :initform (lambda (args) args))

   (callback :reader callback-of :initarg :callback
             :initform (iambda))

   (callback-data :accessor callback-data-of :initarg :callback-data
                  :initform nil)

   (js-before :accessor js-before-of :initarg :js-before
              :initform *js-before*)

   (js-after :accessor js-after-of :initarg :js-after
             :initform *js-after*)

   (browser-default-action-p :accessor browser-default-action-p-of :initarg :browser-default-action-p
                             :initform t)))


(defmethod initialize-instance :after ((callback-box callback-box) &key widget)
  (when widget
    (setf (widget-of callback-box) widget)))


(defmethod (setf widget-of) :after ((widget widget) (callback-box callback-box))
  (setf (slot-value callback-box 'id)
        (js-callback-id-of (id-of (widget-of callback-box))
                           (event-type-of callback-box))))


(defmethod has-client-side-side-effects-p-of ((callback-box callback-box))
  (and (callback-data-of callback-box)
       ;; TODO: Add some :AFTER methods for JS-BEFORE and JS-AFTER that set a flag
       ;; instead of checking the strings every time like this.
       (string= *js-before* (js-before-of callback-box))
       (string= *js-after* (js-after-of callback-box))))
(export 'has-client-side-side-effects-p-of)


(defun store-callback-box (callback-box widget)
  (declare (callback-box callback-box)
           (widget widget))
  (let ((id (id-of callback-box)))
    ;; Weak links.
    (with-visible-contexts-of widget viewport
      (setf (gethash id (callbacks-of viewport))
            callback-box))
    ;; Hard links.
    (setf (gethash id (callbacks-of widget))
          callback-box)))


(defun remove-callback-box (event-type widget)
  (declare (string event-type)
           (widget widget))
  (let ((id (js-callback-id-of (id-of widget) event-type)))
    (with-visible-contexts-of widget viewport
      (remhash id (callbacks-of viewport)))
    (remhash id (callbacks-of widget))))


(defun find-callback-box (id viewport)
  (declare (string id)
           (viewport viewport))
  (gethash id (callbacks-of viewport)))


(defmethod trigger ((callback-box callback-box) &rest args)
  (let ((js-code (js-trigger (id-of (widget-of callback-box))
                             (event-type-of callback-box))))
    (if *js-code-only-p*
        js-code
        (if (and (null args)
                 (has-client-side-side-effects-p-of callback-box))
            (run js-code (widget-of callback-box))
            (execute-callback callback-box args)))))
(export 'trigger)


(defun execute-callback (callback-box args)
  (declare (callback-box callback-box)
           (list args))
  (let ((*current-event* (make-instance 'event
                                        :callback-box callback-box
                                        :viewport *viewport*)))
    (pulse ~(event-cell-of callback-box)
           (with1 (or (funcall (argument-parser-of callback-box) args) t)
             (setf (slot-value *current-event* 'parsed-args) it)))))


(defmethod (setf js-before-of) :after (new-js (callback-box callback-box))
  (declare (ignore new-js))
  (bind-widget (widget-of callback-box) (event-type-of callback-box) callback-box))

(defmethod (setf js-after-of) :after (new-js (callback-box callback-box))
  (declare (ignore new-js))
  (bind-widget (widget-of callback-box) (event-type-of callback-box) callback-box))

(defmethod (setf callback-data-of) :after (new-data (callback-box callback-box))
  (declare (ignore new-data))
  (bind-widget (widget-of callback-box) (event-type-of callback-box) callback-box))

(defmethod (setf browser-default-action-p-of) :after (pred (callback-box callback-box))
  (declare (ignore pred))
  (bind-widget (widget-of callback-box) (event-type-of callback-box) callback-box))


(defun bind-widget (widget event-type callback)
  (declare (widget widget)
           (string event-type)
           ((or callback-box string null) callback))
  (let ((js-code (if callback
                     (js-bind (id-of widget) (event-type-of callback) (id-of callback)
                              :client-side-only-p (when (stringp (callback-of callback))
                                                    (callback-of callback))
                              :callback-data (callback-data-of callback)
                              :js-before (js-before-of callback)
                              :js-after (js-after-of callback)
                              :browser-default-action-p (browser-default-action-p-of callback))
                     (js-unbind (id-of widget) event-type))))
    (if *js-code-only-p*
        js-code
        (run js-code widget))))


(defun (setf event) (callback event-type widget &key
                     server-only-p
                     js-before js-after callback-data
                     (browser-default-action-p t))
  (declare (callback-box callback)
           (string event-type)
           (widget widget)
           ((or string null) js-before js-after)
           ((or list null) callback-data))
  (setf (widget-of callback) widget)
  (when js-before
    (setf (slot-value callback 'js-before) js-before))
  (when js-after
    (setf (slot-value callback 'js-after) js-after))
  (when callback-data
    (setf (slot-value callback 'callback-data) callback-data))
  (when (typep callback 'callback-box)
    (setf (slot-value callback 'browser-default-action-p) browser-default-action-p))

  ;; setup @ server side
  (store-callback-box callback widget)

  ;; setup @ client side
  (unless server-only-p (bind-widget widget event-type callback)))
(export 'event)


;; TODO: Finish this.
(defmethod event ((event-type string) (widget widget) &key
                  server-only-p
                  js-before js-after callback-data
                  (browser-default-action-p t))
  (declare (ignorable server-only-p js-before js-after callback-data browser-default-action-p))
  #|(assert *formula*)|#
  (write-line "TODO: event"))
(export 'event)


(defun event-remove (event-type widget &key server-only-p)
  (declare (string event-type)
           (widget widget)
           (ignore server-only-p))
  (declare (ignore event-type widget))
  #|(setf (event event-type widget) nil)|#
  #|(remove-callback-box event-type widget)|#)
(export 'event-remove)



(defmacro mk-cb ((widget-sym &rest args) &body body)
  "\"Make callback.\". Creates the Lisp side function or callback for
DOM-events."
  `(lambda (,widget-sym &key ,@args)
     ,@body))
(export 'mk-cb)


(defun event-dom-server-reader (dom-mirror lisp-accessor-name event-type)
  (declare (dom-mirror dom-mirror)
           (symbol lisp-accessor-name)
           (string event-type))
  (unless =cell=
    (return-from event-dom-server-reader
      (gethash lisp-accessor-name (dom-mirror-data-of dom-mirror))))
  (let ((dom-mirror-data (dom-mirror-data-of dom-mirror)))
    (sb-ext:with-locked-hash-table (dom-mirror-data)
      (multiple-value-bind (callback-box found-p)
          (dom-server-reader dom-mirror lisp-accessor-name)
        (unless found-p
          (setf callback-box (make-instance 'callback-box :widget dom-mirror :event-type event-type)))
        (multiple-value-prog1 (values ~(event-cell-of callback-box) t)
          ;; Ensure that the (possibly) anonymous CELL isn't GCed too early.
          #|(with-lifetime dom-mirror =cell=)|#
          (unless found-p
            (initialize-callback-box dom-mirror lisp-accessor-name callback-box)
            ;; Trigger client side magic; (setf (on-click-of widget) callback-box).
            (funcall (fdefinition `(setf ,lisp-accessor-name)) callback-box dom-mirror)))))))


(defmethod initialize-callback-box ((dom-mirror dom-mirror) (lisp-accessor-name symbol) (callback-box callback-box))
  )
(export 'initialize-callback-box)


(defmacro define-event-property (lisp-name event-type &body args)
  `(progn
     (define-dom-property ',lisp-name ,event-type #'(setf event) #'event #'event-remove
                          :value-removal-checker
                          nil

                          :dom-server-reader
                          (lambda (dom-mirror lisp-accessor-name)
                            (event-dom-server-reader dom-mirror lisp-accessor-name ,event-type))

                          :value-marshaller
                          (lambda (callback)
                            (typecase callback
                              ;; TODO: It might not make sense to accept a FUNCTION anymore.
                              #|((or function string)
                               (make-instance 'callback-box
                                              :event-type ,event-type
                                              :callback callback))|#

                              (callback-box
                               callback)

                              (t
                               (error "Don't know what to do with ~S." callback))))
                          ,@args)
     (export ',lisp-name)))



(define-event-property on-blur-of "blur")
(define-event-property on-change-of "change")
(define-event-property on-click-of "click")
(define-event-property on-dblclick-of "dblclick")
(define-event-property on-focus-of "focus")
(define-event-property on-keydown-of "keydown")
(define-event-property on-keypress-of "keypress")
(define-event-property on-keyup-of "keyup")
(define-event-property on-load-of "load")
(define-event-property on-mousedown-of "mousedown")
(define-event-property on-mousemove-of "mousemove")
(define-event-property on-mouseout-of "mouseout")
(define-event-property on-mouseover-of "mouseover")
(define-event-property on-mouseup-of "mouseup")
(define-event-property on-resize-of "resize")
(define-event-property on-scroll-of "scroll")
(define-event-property on-select-of "select")
(define-event-property on-unload "unload")
