;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations))


(defclass callback-box ()
  ((id :reader id-of)

   (widget :reader widget-of :initarg :widget
           :type widget
           :initform (error ":WIDGET needed."))

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


(defmethod initialize-instance :after ((callback-box callback-box) &key)
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


#.(maybe-inline 'store-callback-box)
(defun store-callback-box (callback-box viewport)
  (declare (callback-box callback-box)
           (viewport viewport)
           (optimize speed))
  (setf (gethash (id-of callback-box) (callbacks-of viewport))
        callback-box))


#.(maybe-inline 'remove-callback-box)
(defun remove-callback-box (widget event-type viewport)
  (declare (widget widget)
           (string event-type)
           (viewport viewport)
           (optimize speed))
  (remhash (js-callback-id-of (id-of widget) event-type)
           (callbacks-of viewport)))


#.(maybe-inline 'find-callback-box)
(defun find-callback-box (id viewport)
  (declare (string id)
           (viewport viewport)
           (optimize speed))
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


#.(maybe-inline 'execute-callback)
(defun execute-callback (callback-box status args)
  (declare (callback-box callback-box)
           (symbol status)
           (list args)
           (optimize speed))
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


#.(maybe-inline 'bind-widget)
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


#.(maybe-inline '(setf event))
(defun (setf event) (callback event-type widget &key
                     dom-cache-writer-fn server-only-p
                     js-before js-after callback-data
                     (browser-default-action-p t))
  (declare (string event-type)
           (widget widget)
           (optimize speed))
  #| CALLBACK can be a function, a string representing JS code or NIL which means
that the event is to be unbound. |#
  (let ((callback (when callback
                    (if (typep callback 'callback-box)
                        callback
                        (make-instance 'callback-box
                                       :widget widget
                                       :event-type event-type
                                       :callback callback)))))
    (when js-before
      (setf (slot-value callback 'js-before) js-before))
    (when js-after
      (setf (slot-value callback 'js-after) js-after))
    (when callback-data
      (setf (slot-value callback 'callback-data) callback-data))
    (when (typep callback 'callback-box)
      (setf (slot-value callback 'browser-default-action-p) browser-default-action-p))

    ;; setup @ server side
    (when dom-cache-writer-fn
      (funcall (the function dom-cache-writer-fn) callback))
    (when callback
      (with-visible-contexts-of widget viewport
        (store-callback-box callback viewport)))

    ;; setup @ client side
    (unless server-only-p (bind-widget widget event-type callback))))
(export 'event)


(defmethod event ((event-type string) (widget widget)
                  &optional dom-cache-reader-fn)
  ;; If the user want JS code he should use: (js-code-of (trigger (on-click-of some-widget))).
  (when dom-cache-reader-fn (funcall dom-cache-reader-fn)))
(export 'event)


(defmacro mk-cb ((widget-sym &rest args) &body body)
  "\"Make callback.\". Creates the Lisp side function or callback for
DOM-events."
  `(lambda (,widget-sym &key ,@args)
     ,@body))
(export 'mk-cb)



(defmacro gen-dom-event-class (name)
  (let* ((lisp-class-accessor-name (symbolicate (string-upcase (catstr "on-" name))))
         (accessor (symbolicate (string-upcase (catstr "on-" name "-of")))))
    `(def-dom-class ,lisp-class-accessor-name event ,name
                    :writer-check-for-value-designating-removal-code (eq value nil)
                    :accessor ,accessor
                    :writer-extra-keyargs (js-before js-after callback-data (browser-default-action-p t))
                    :remover-code ((with-visible-contexts-of dom-mirror viewport
                                     (remove-callback-box dom-mirror ,name viewport))
                                   (remhash ',lisp-class-accessor-name (dom-mirror-data-of dom-mirror))))))


(gen-dom-event-class "blur")
(gen-dom-event-class "change")
(gen-dom-event-class "click")
(gen-dom-event-class "dblclick")
(gen-dom-event-class "focus")
(gen-dom-event-class "keydown")
(gen-dom-event-class "keypress")
(gen-dom-event-class "keyup")
(gen-dom-event-class "load")
(gen-dom-event-class "mousedown")
(gen-dom-event-class "mousemove")
(gen-dom-event-class "mouseout")
(gen-dom-event-class "mouseover")
(gen-dom-event-class "mouseup")
(gen-dom-event-class "resize")
(gen-dom-event-class "scroll")
(gen-dom-event-class "select")
;;(gen-dom-event-class "submit")
(gen-dom-event-class "unload")



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
