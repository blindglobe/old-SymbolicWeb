;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

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



(defclass callback-box ()
  ((id :reader id-of :initarg :id
       :type string
       :initform (id-generator-next-str -id-generator-))

   (widget :reader widget-of :initarg :widget
           :type dom-mirror
           :initform (error ":WIDGET needed"))

   (event-cell :reader event-cell-of :initarg :event-cell
               :type cell
               :initform #~nil)

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


(defmethod deref ((callback-box callback-box))
  (sw-mvc::cell-deref (event-cell-of callback-box)))


(defun remove-callback-box (callback-box widget)
  (declare (callback-box callback-box)
           (dom-mirror widget))
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
    (pulse ~(event-cell-of callback-box)
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
#|(export 'event-remove)|#


(defmethod js-before-check ((widget dom-mirror) (lisp-accessor-name symbol))
  "return true;")


;; (SETF EVENT)
(defun event-dom-client-writer (widget cb js-code-fn &rest args)
  (let ((js-code (or (code-of cb)
                     (setf (code-of cb) (funcall js-code-fn)))))
    (if *js-code-only-p*
        js-code
        (apply #'run js-code widget args))))


(defmethod initialize-callback-box ((dom-mirror dom-mirror) (lisp-accessor-name symbol) (callback-box callback-box))
  )
(export 'initialize-callback-box)


(defun event-dom-server-reader (dom-mirror lisp-accessor-name)
  (declare (dom-mirror dom-mirror)
           (symbol lisp-accessor-name))
  (let ((dom-mirror-data (dom-mirror-data-of dom-mirror)))
    (sb-ext:with-locked-hash-table (dom-mirror-data)
      (multiple-value-bind (callback-box found-p)
          (dom-server-reader dom-mirror lisp-accessor-name)
        (if =cell=
            (progn
              (unless found-p
                (setf callback-box (make-instance 'callback-box :widget dom-mirror)))
              (multiple-value-prog1 (values ~callback-box t)
                (unless found-p
                  (initialize-callback-box dom-mirror lisp-accessor-name callback-box)
                  (funcall (fdefinition `(setf ,lisp-accessor-name)) callback-box dom-mirror))))
            (values callback-box found-p))))))


(defmacro define-event-property ((lisp-name event-type &rest js-msg-args) &rest args)
  `(progn
     (define-dom-property ',lisp-name
       :dom-client-writer
       (lambda (cb widget &rest args)
         (declare (callback-box cb)
                  (dom-mirror widget))
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
       ,@args)
     (export ',lisp-name)))



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

#|
$("#sw-root").append(decodeURIComponent("%3Cinput%20id%3D'TEXT-INPUT-9'%20type%3D'text'%3E%3C%2Finput%3E"));
$('#TEXT-INPUT-9')[0].sw_text_input_value = "";
$('#TEXT-INPUT-9').bind('blur', (function (event) {
    swMsg("TEXT-INPUT-9", "A", (function () {
        if (event.currentTarget.sw_text_input_value == event.currentTarget.value) {
            return false
        } else {
            event.currentTarget.sw_text_input_value = event.currentTarget.value
            return true
        }
    }), "value=" + encodeURIComponent((function(){ return $("#TEXT-INPUT-9").attr("value");\n })()) + "", (function (data, textStatus) {
    }))
    return true
}));
$('#TEXT-INPUT-9').bind('keyup', (function (event) {
    swMsg("TEXT-INPUT-9", "B", (function () {
        if (event.which != 13) {
            return false
        }
        if (event.currentTarget.sw_text_input_value == event.currentTarget.value) {
            return false
        } else {
            event.currentTarget.sw_text_input_value = event.currentTarget.value
            return true
        }
    }), "value=" + encodeURIComponent((function(){ return $("#TEXT-INPUT-9").attr("value");\n })()) + "", (function (data, textStatus) {
    }))
    return true
}));
sw_comet_response = true;
|#