;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :widgets/widget.lisp))


(defclass widget (widget-base)
  ((element-type :reader element-type-of :initarg :element-type
                 :type string
                 :initform "div")

   (static-attributes :reader static-attributes-of
                      :type list)

   (viewport :accessor viewport-of
             :type (or viewport null)
             :initform nil)

   (focussable-p :reader focussable-p-of :initarg :focussable-p
                 :type (member t nil)
                 :initform nil))

  (:documentation "
Some notes about creating widgets
=================================

  * You do not want to 'pull' from the Model end in your RENDER method; let the
    Model end 'push' to the View end instead.

  * This also means that you do not want to use the WHEN-COMMIT facility in your
    RENDER methods or in code called by it as it is not unlikely that you're
    already within the dynamic scope of a WHEN-COMMIT form by the time RENDER
    is called. E.g., the CONTAINER-* methods in widgets/container.lisp wrap
    calls to RENDER in WHEN-COMMIT forms.
"))


(defmethod initialize-instance :around ((widget widget) &key)
  (setf (slot-value widget 'static-attributes) (list))
  (let ((*currently-constructing-widget* widget)
        ;; To avoid SHTML-OF to screw up when nesting HTML-CONTAINER and other stuff.
        (*creating-html-container-p* nil))
    (call-next-method)))


(defmethod print-slots progn ((widget widget) stream)
  (when (slot-boundp widget 'element-type)
    (format stream " ~A" (slot-value widget 'element-type))))


(defmethod shtml-of :around ((widget widget))
  (declare #.(optimizations :shtml-of))
  (when *creating-html-container-p*
    (push widget *html-container-children*))
  (call-next-method))


(defmethod shtml-of ((widget widget))
  (declare #.(optimizations :shtml-of))
  (let ((element-type (element-type-of widget)))
    (catstr "<" element-type " id='" (id-of widget) "'"
            (if (hidden-p-of widget) " class='sw-hide'" "")
            (with-output-to-string (ss)
              (dolist (key.value (static-attributes-of widget))
                (format ss " ~A='~A'"
                        (car key.value)
                        (princ-to-string (cdr key.value)))))
            "></" element-type ">")))


#.(maybe-inline 'currently-constructing-p)
(defun currently-constructing-p (widget)
  (declare (widget widget))
  (eq widget *currently-constructing-widget*))


(defun html<- (obj widget)
  (declare (widget widget))
  (the string
    (values
     (cond
       ((stringp obj) obj)

       ((ignore-errors (ensure-function obj))
        (html<- (funcall obj widget) widget))

       (t (princ-to-string obj))))))


(defmethod visible-p-of ((widget widget) &key
                         (app nil app-supplied-p)
                         (viewport nil viewport-supplied-p))
  "If VIEWPORT is supplied this will determine whether WIDGET is visible in that viewport.

If APP is supplied this will determine whether WIDGET is visible within any of
the viewports within that session.

If neither APP nor VIEWPORT is supplied this will determine wheter WIDGET is visible
in any session in any viewport."
  (declare (ignore app)
           ((or viewport null) viewport))
  (when app-supplied-p
    (error "TODO: Not implemented yet."))
  (if viewport-supplied-p
      (and (eq (viewport-of widget) viewport)
           (visible-p-of viewport))
      (withp (viewport-of widget) (visible-p-of it))))


(defmethod render ((widget widget))
  "These methods must never 'pull' from their MODEL ends; the MODEL should 'push' to the VIEW-side (slots in the
VIEW). These methods might be called from within the dynamic scope of a WHEN-COMMIT body (widgets/container.lisp)."
  )


(defmethod focus ((widget widget) &key server-only-p)
  (if *js-code-only-p*
      (js-focus (id-of widget))
      (progn
        (setf (slot-value (application-of (viewport-of widget)) 'last-focus)
              widget)
        (unless server-only-p
          (run (js-focus (id-of widget)) widget)))))


(defmethod scroll-to-bottom ((widget widget))
  (if *js-code-only-p*
      (js-scroll-to-bottom (id-of widget))
      (when-commit ()
        (run (js-scroll-to-bottom (id-of widget)) widget))))


(defmethod remove-widget-from-viewport ((widget widget) (viewport viewport))
  "Remove WIDGET from VIEWPORT, but only if it actually is part of VIEWPORT."
  ;; WIDGET -/-> VIEWPORT.
  (when (eq viewport (viewport-of widget))
    (nilf (viewport-of widget))))


(defmethod show ((widget widget) &key server-only-p)
  (remove-class widget "sw-hide" :server-only-p server-only-p))


(defmethod hide ((widget widget) &key server-only-p)
  (add-class widget "sw-hide" :server-only-p server-only-p))


(defmethod show-all ((container container) &key server-only-p)
  (with-each-widget-in-tree (:root container)
    (show widget :server-only-p server-only-p)))


(defmethod show-all ((widget widget) &key server-only-p)
  (show widget :server-only-p server-only-p))


(defmethod hide-all ((container container) &key server-only-p)
  (with-each-widget-in-tree (:root container)
    (hide widget :server-only-p server-only-p)))


(defmethod hide-all ((widget widget) &key server-only-p)
  (hide widget :server-only-p server-only-p))


(defmethod shown-p-of ((widget widget))
  "Returns T or NIL."
  (not (member "sw-hide" (attribute-class-of widget) :test #'string=)))


(defmethod hidden-p-of ((widget widget))
  "Returns T or NIL."
  (not (shown-p-of widget)))
