;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations))


(defclass callback-box ()
  ((id :reader id-of)

   (widget :accessor widget-of :initarg :widget
           :type widget)

   (event-type :reader event-type-of :initarg :event-type
               :type string
               :initform (error ":EVENT-TYPE needed."))

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


(defun store-callback-box (callback-box viewport)
  (declare (callback-box callback-box)
           (viewport viewport))
  (setf (gethash (id-of callback-box) (callbacks-of viewport))
        callback-box))


(defun remove-callback-box (widget event-type viewport)
  (declare (widget widget)
           (string event-type)
           (viewport viewport))
  (remhash (js-callback-id-of (id-of widget) event-type)
           (callbacks-of viewport)))


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
            (execute-callback callback-box :server-side-trigger args)))))
(export 'trigger)


(defun execute-callback (callback-box status args)
  (declare (callback-box callback-box)
           (symbol status)
           (list args))
  (let ((*current-event-widget* (widget-of callback-box)))
    (apply (the function (callback-of callback-box)) (widget-of callback-box)
           :status status :callback callback-box :allow-other-keys t
           args)))


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
           (string event-type))
  (let ((js-code (if callback
                     ;; It's an instance of CALLBACK-BOX.
                     (js-bind (id-of widget) (event-type-of callback) (id-of callback)
                              :client-side-only-p (when (stringp (callback-of callback))
                                                    (callback-of callback))
                              :callback-data (callback-data-of callback)
                              :js-before (js-before-of callback)
                              :js-after (js-after-of callback)
                              :browser-default-action-p (browser-default-action-p-of callback))
                     ;; CALLBACK argument for (SETF EVENT) was NIL.
                     (js-unbind (id-of widget) event-type))))
    (if *js-code-only-p*
        js-code
        (run js-code widget))))


(defun (setf event) (callback event-type widget &key
                     server-only-p
                     js-before js-after callback-data
                     (browser-default-action-p t))
  (declare (string event-type)
           (widget widget))
  (when callback
    (setf (widget-of callback) widget)
    (when js-before
      (setf (slot-value callback 'js-before) js-before))
    (when js-after
      (setf (slot-value callback 'js-after) js-after))
    (when callback-data
      (setf (slot-value callback 'callback-data) callback-data))
    (when (typep callback 'callback-box)
      (setf (slot-value callback 'browser-default-action-p) browser-default-action-p)))

  ;; setup @ server side
  (when callback
    (with-visible-contexts-of widget viewport
      (store-callback-box callback viewport)))

  ;; setup @ client side
  (unless server-only-p (bind-widget widget event-type callback)))
(export 'event)


;; TODO: Finish this.
(defmethod event ((event-type string) (widget widget) &key
                  dom-cache-writer-fn server-only-p
                  js-before js-after callback-data
                  (browser-default-action-p t))
  (assert *formula*)
  (let ((event-router (event-router-of widget)))
    (sb-ext:with-locked-hash-table (event-router)
      (multiple-value-bind (state-cell found-p)
          (gethash event-type event-router)
        (if found-p
            (prog1 ~(car state-cell)
                   (unless (find *formula* (cdr state-cell) :key (lambda (elt)
                                                                   (formula-of elt)))
                     (write-line "blah")
                     (push #~*formula* (cdr state-cell))))
            (let ((state-cell (cons #~nil (list #~*formula*))))
              (prog1 ~(car state-cell)
                     (setf (gethash event-type event-router) state-cell
                           (event event-type widget) (iambda (pulse ~(car state-cell) t))))))))))
(export 'event)


(defun event-remove (event-type widget &key server-only-p)
  (declare (string event-type)
           (widget widget)
           (ignore server-only-p))
  (setf (event event-type widget) nil)
  (with-visible-contexts-of widget viewport
    (remove-callback-box widget event-type viewport)))
(export 'event-remove)



(defmacro mk-cb ((widget-sym &rest args) &body body)
  "\"Make callback.\". Creates the Lisp side function or callback for
DOM-events."
  `(lambda (,widget-sym &key ,@args)
     ,@body))
(export 'mk-cb)



(defmacro define-event-property (lisp-name dom-name &body args)
  `(progn
     (define-dom-property ',lisp-name ,dom-name #'(setf event) #'event #'event-remove
                          :value-marshaller
                          (lambda (callback)
                            (when callback
                              (typecase callback
                                ((or function string)
                                 (make-instance 'callback-box
                                                :event-type ,dom-name
                                                :callback callback))

                                (callback-box
                                 callback)

                                (t
                                 (error "Don't know what to do with ~A" callback)))))
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















#|
(defmacro define-event-router ((name &key
                                     (dom-mirror-name 'widget)
                                     (accessor (symbolicate name '-of))
                                     (state 'state)
                                     (default-state nil))
                               &body body)
  (with-gensyms (event-router formula state-monitor found-p)
    `(defmethod ,accessor ((,dom-mirror-name dom-mirror) &key (store-in-view-p t))
       (symbol-macrolet ((,event-router (slot-value ,dom-mirror-name 'event-router)))
         (sb-ext:with-locked-hash-table (,event-router)
           (if-let ((,formula sw-mvc:*creating-formula*))
             (multiple-value-bind (,state-monitor ,found-p)
                 (gethash ',name ,event-router)
               (if ,found-p
                   (prog1 ~(car ,state-monitor)
                          (when store-in-view-p
                            (push #~,formula (cdr ,state-monitor))))
                   (let ((,state-monitor (cons #~,default-state nil)))
                     (prog1 ~(car ,state-monitor)
                            (setf (gethash ',name ,event-router) ,state-monitor)
                            (symbol-macrolet ((,state ~(car ,state-monitor)))
                              ,@body)
                            (when store-in-view-p
                              (setf (cdr ,state-monitor)
                                    (list #~,formula)))))))
             ~(car (gethash ',name ,event-router))))))))


(define-event-router (mouse-button-state)
  (setf (on-mousedown-of widget) (iambda (tf state))
        (on-mouseup-of widget)   (iambda (nilf state))))


(define-event-router (mouse-click-state)
  (setf (on-click-of widget) (iambda
                               (if (numberp state)
                                   (incf state)
                                   (setf state 0)))))
|#
