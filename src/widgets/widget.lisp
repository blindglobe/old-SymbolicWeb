;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/widget.lisp))


(defclass widget (widget-base)
  ((shtml :initarg :shtml
          :documentation "
\"Static\" outer HTML shell or wrapping of a widget. This HTML code usually
maintains or contains the ID attribute (stored in the ID-slot of the OBJECT
parent class of this class) so the server and client \"versions\" of the
widget can communicate or refer to each other.
See the SHTML-OF method.")

   (static-attributes :reader static-attributes-of
                      :type list)

   (viewports :reader viewports-of
              :type hash-table
              :initform (make-hash-table :test #'equal :weakness :value :synchronized t)
              :documentation "
The viewports, all browser tabs/windows regardless of user/session, the widget is part of
or visible in.")

   (visible-p :initform nil
              :documentation "
Use/see the VISIBLE-P-OF method.")))


(defmethod initialize-instance :around ((widget widget) &key)
  (setf (slot-value widget 'static-attributes) (list))
  ;; Let the RENDER method send stuff to the client when things are ready instead.
  (with-code-block (:execute-p nil)
    (let ((*currently-constructing-widget* widget)
          (*creating-html-container-p* nil)) ;; To avoid SHTML-OF to screw up when nesting WITH-HTML-CONTAINER type stuff.
      (call-next-method))))


(defmethod initialize-instance :after ((widget widget) &key
                                       (element-type "div")
                                       ;; NOTE: This gets special treatment to avoid "flashing" when rendering.
                                       (display nil display-supplied-p))
  (unless (slot-boundp widget 'shtml)
    (setf (slot-value widget 'shtml)
          (catstr "<" element-type " id='" (id-of widget) "'"
                  (if display-supplied-p
                      (progn
                        (setf (display-of widget :server-only-p t) display)
                        (catstr " style='display: " (princ-to-string display) ";'"))
                      "")
                  (if-let (attributes (static-attributes-of widget))
                    (with-output-to-string (ss)
                      (dolist (key.value attributes)
                        (format ss " ~A='~A'"
                                (car key.value)
                                (princ-to-string (cdr key.value)))))
                    "")
                  "></" element-type ">"))))


#.(maybe-inline 'currently-constructing-p)
(defun currently-constructing-p (widget)
  (declare (type widget widget))
  (eq widget *currently-constructing-widget*))


(defmethod shtml-of ((widget widget))
  (when *creating-html-container-p*
    (push widget *html-container-children*))
  (slot-value widget 'shtml))


(defun html<- (obj widget)
  (declare (type widget widget))
  (the string
    (values
     (cond
       ((stringp obj) obj)

       ((ignore-errors (ensure-function obj))
        (html<- (funcall obj widget) widget))

       (t (princ-to-string obj))))))


(defmethod visible-p-of ((widget widget) &key
                         (app nil app-supplied-p) (viewport nil viewport-supplied-p)
                         real-check-p)
  "If VIEWPORT is supplied this will determine whether WIDGET is visible in that viewport.
If APP is supplied this will determine whether WIDGET is visible within any of
the viewports within that session.
If neither APP nor VIEWPORT is supplied this will determine wheter WIDGET is visible
in any session in any viewport."
  (declare (ignore app))
  (when app-supplied-p
    (error "TODO: Not implemented yet."))
  (if real-check-p
      (if (and (visible-p-of widget)
               (zerop (hash-table-count (viewports-of widget))))
          ;; WIDGET isn't part of any viewport anymore.
          (nilf (slot-value widget 'visible-p))
          t)
      (and (slot-value widget 'visible-p)
           (if viewport-supplied-p
               (and (visible-p-of viewport)
                    ;; The widget and viewport supplied is visible, but is the widget really part of this viewport?
                    (gethash (id-of viewport) (viewports-of widget)))
               t))))


(defmethod render ((widget widget))
  )
(export 'render)


(defmethod focus ((widget widget))
  (if *js-code-only-p*
      (js-focus (id-of widget))
      (run (js-focus (id-of widget)) widget)))


(defmethod scroll-to-bottom ((widget widget))
  (if *js-code-only-p*
      (js-scroll-to-bottom (id-of widget))
      (run (js-scroll-to-bottom (id-of widget)) widget)))


#.(maybe-inline 'for-each-viewport-of-widget)
(defun for-each-viewport-of-widget (widget fn)
  "FN is a function that takes one argument; the viewport.
Also see FOR-EACH-VIEWPORT-IN-APP."
  (declare (type widget widget)
           (type (function (viewport)) fn))
  (maphash (lambda (%not-used viewport)
             (declare (ignore %not-used)
                      (type viewport viewport))
             (funcall fn viewport))
           (viewports-of widget)))


(defmethod shared-p-of ((widget widget))
  "Returns T if widget is visible in multiple contexts.
This might not be 100% accurate; it might return T when the widget is only
visible in one or even no context."
  (declare (inline visible-p-of))
  (and (visible-p-of widget)
       (< 1 (hash-table-count (viewports-of widget)))))


(defmethod remove-widget-from-viewport ((widget widget) (viewport viewport))
  ;; VIEWPORT -/-> WIDGET.
  (remhash (id-of widget) (widgets-of viewport))
  ;; WIDGET -/-> VIEWPORT.
  (remhash (id-of viewport) (viewports-of widget))
  (visible-p-of widget :real-check-p t))


(defmethod set-show-on-feedback ((widget widget) (cell cell))
  "WIDGET should only be visible when \"feedback\" from CELL is T."
  (add-formula widget
               λ(if (feedback-event-of cell)
                    (remove-class widget :sw-hide)
                    (add-class widget :sw-hide))))
(export 'set-show-on-feedback)
