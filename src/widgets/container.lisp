;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/container.lisp))


#| TODO: Get rid of the CHILDREN slot in CONTAINER-BASE. This is basically the same change done wrt. HTML-ELEMENT. |#


(defclass container (widget container-base)
  ()

  (:default-initargs
   :model (dlist)))


(defmethod view-constructor ((container container) model)
  (error "~S doesn't know how to make a View of ~S." container model))


(defmethod view-constructor ((container container) (model single-value-model))
  (make-instance 'html-element :model model))


(defmethod view-constructor ((container container) (model multiple-value-model))
  (make-instance 'container :model model))


(defmethod (setf model-of) ((model dlist) (container container))
  (prog1
      #λ(when-let (event (event-of model))
          ;; TODO: Why am I not using a method here?
          (typecase event
            (container-insert   (when-commit () (mvc-container-insert   container event)))
            (container-remove   (when-commit () (mvc-container-remove   container event)))
            (container-exchange (when-commit () (mvc-container-exchange container event)))))

      (do ((dlist-node (head-of model) (sw-mvc:right-of dlist-node)))
          ((null dlist-node))
        (when-commit ()
          (add (view-in-context-of container dlist-node) container)))))


(defun mvc-container-insert (container event)
  (let ((relative-position (relative-position-of event))
        (relative-object (relative-object-of event)))
    (if relative-object
        (let ((relative-widget (view-in-context-of container relative-object)))
          (dolist (object (objects-of event))
            (let ((new-widget (view-in-context-of container object)))
              (case relative-position
                (:before
                 (insert container new-widget :before relative-widget)
                 (setf relative-widget new-widget
                       relative-position :after))
                (:after
                 (insert container new-widget :after relative-widget)
                 (setf relative-widget new-widget))
                (otherwise
                 (error "Unknown relative-position: ~A" relative-position))))))
        (dolist (object (objects-of event))
          (let ((new-widget (view-in-context-of container object)))
            (add new-widget container))))))


(defun mvc-container-remove (container event)
  (dolist (object (objects-of event))
    (remove (view-in-context-of container object) container)))


(defun mvc-container-exchange (container event)
  (exchange container
            (view-in-context-of container (object-of event))
            (view-in-context-of container (target-position-of event))))


(defmethod render ((container container))
  (let ((container-id (id-of container)))
    ;; TODO: Consider adding a VIEWS-IN-CONTEXT-OF (plural) method in SW-MVC.
    (dolist (child-model ~~container)
      (let ((child-view (view-in-context-of container child-model)))
        (run (js-iappend (shtml-of child-view) container-id) container)
        (render child-view)))))


#.(maybe-inline 'propagate-for-add)
(defun propagate-for-add (widget container)
  (declare (widget widget)
           (container container))
  (let* ((viewport (viewport-of container))
         (widgets (widgets-of (application-of viewport))))
    (with-each-widget-in-tree (:root widget)
      (setf (gethash (id-of widget) widgets) widget
            (viewport-of widget) viewport))))


#.(maybe-inline 'propagate-for-remove)
(defun propagate-for-remove (widget container)
  (declare (widget widget)
           (container container)
           (ignore container))
  (let* ((viewport (viewport-of widget))
         (widgets (widgets-of (application-of viewport))))
    (with-each-widget-in-tree (:root widget)
      (remhash (id-of widget) widgets)
      (nilf (viewport-of widget)))))


(defmethod add ((widget widget) (container container))
  "Add or append WIDGET to CONTAINER.
Returns WIDGET."
  (prog1 widget
    (amx:insert widget ↺(slot-value container 'children) :last-p t)
    (when (visible-p-of container)
      (run (js-iappend (shtml-of widget) (id-of container)) container)
      (propagate-for-add widget container)
      (render widget))))


;; TODO: I don't think shadowing CL:REMOVE is such a great idea.
(muffle-compiler-note
(defmethod remove ((widget widget) &optional (container (error "CONTAINER needed.")))
  "Remove WIDGET.
Returns WIDGET."
  (declare (container container))
  (prog1 widget
    (deletef (slot-value container 'children) widget)
    (when (visible-p-of container)
      (propagate-for-remove widget container)
      (run (js-remove (id-of widget)) container)))))


(defmethod add-to* ((container container) widgets)
  "Adds or appends each widget in WIDGETS to CONTAINER in sequence.
Returns WIDGETS."
  (declare (list widgets))
  (prog1 widgets
    (nconcf (slot-value container 'children) widgets)
    (when (visible-p-of container)
      (dolist (widget widgets)
        (run (js-iappend (shtml-of widget) (id-of container)) container)
        (propagate-for-add widget container)
        (render widget)))))


(defmethod oadd ((container container) (widget widget) (left-widget widget))
  "Inserts WIDGET as a sibling right of LEFT-WIDGET. Does the \"same as\"
jQuery's 'after' function.
Returns WIDGET."
  (prog1 widget
    (amx:insert widget ↺(slot-value container 'children) :after left-widget)
    (when (visible-p-of container)
      (run (js-oappend (the string (shtml-of widget))
                       (the string (id-of left-widget)))
           container)
      (propagate-for-add widget container)
      (render widget))))


(defmethod insert ((container container) (new-widget widget) &key before after)
  "Inserts NEW-WIDGET \"left\" or \"right\" of an already existing widget
depending on whether :BEFORE or :AFTER is supplied."
  (declare (inline oadd oprepend))
  (cond
    (after  (oadd container new-widget after))
    (before (oprepend container new-widget before))
    (t (error ":AFTER or :BEFORE must be supplied."))))


#|(defmethod add ((container container) (widget widget) (left-widget widget))
  "Inserts WIDGET as a sibling right of LEFT-WIDGET. Does the \"same as\"
jQuery's 'after' function.
Returns WIDGET."
  (declare (inline oadd))
  (oadd container widget left-widget))|#


(defmethod prepend ((container container) (widget widget))
  "Prepend WIDGET to CONTAINER.
Returns WIDGET."
  (prog1 widget
    (setf (slot-value container 'children)
          (cons widget (slot-value container 'children)))
    (when (visible-p-of container)
      (run (js-iprepend (shtml-of widget) (id-of container)) container)
      (propagate-for-add widget container)
      (render widget))))


(defmethod oprepend ((container container) (widget widget) (right-widget widget))
  "Inserts WIDGET as a sibling left of RIGHT-WIDGET. Does the \"same as\"
jQuery's 'before' function.
Returns WIDGET."
  (prog1 widget
    (amx:insert widget ↺(slot-value container 'children) :before right-widget)
    (when (visible-p-of container)
      (run (js-oprepend (shtml-of widget) (id-of right-widget)) container)
      (propagate-for-add widget container)
      (render widget))))


#|(defmethod prepend ((container container) (widget widget) (right-widget widget))
  "Inserts WIDGET as a sibling left of RIGHT-WIDGET.
Returns WIDGET."
  (declare (inline oprepend))
  (oprepend container widget right-widget))|#


(defmethod prepend-to ((container container) &rest widgets)
  "Prepends each widget in WIDGETS to CONTAINER in sequence.
Returns WIDGETS."
  (prog1 widgets
    (when (visible-p-of container)
      (dolist (widget widgets)
        (run (js-iprepend (shtml-of widget) (id-of container)) container)
        (propagate-for-add widget container)
        (render widget)))
    ;; This mutates WIDGETS.
    (setf (slot-value container 'children)
          (nconc widgets (slot-value container 'children)))))


#|(defmethod replace ((container container) (old-widget widget) (new-widget widget))
  "Replace OLD-WIDGET with NEW-WIDGET.
returns new-widget."
  (error "SW: (REPLACE MODEL WIDGET WIDGET): Not implemented yet.")
  (let ((container (parent-of new-widget)))
    (when (visible-p-of container)
      (propagate-for-remove old-widget container)
      (run (js-replace-with (id-of old-widget) (shtml-of new-widget)) container)
      (propagate-for-add new-widget container)
      (render new-widget)))
  new-widget)|#


(defmethod remove-all ((container container)
                       &aux (old-children (slot-value container 'children)))
  "Remove all widgets (directly contained) in CONTAINER.
Returns the number of widgets removed."
  (nilf (slot-value container 'children))
  (prog1 (length old-children)
    (when (visible-p-of container)
      (dolist (child old-children)
        (propagate-for-remove child container))
      (run (js-empty (id-of container)) container))))


#|(defmethod (setf children-of) ((new-child-widgets list) (container container)
                               &aux (old-children (slot-value container 'children)))
  "Note that this will not remove NEW-CHILD-WIDGETS from any containers they might
already be in."
  (error "SW: (SETF CHILDREN-OF) Not implemented yet.")
  (when (visible-p-of container)
    (dolist (child old-children)
      (propagate-for-remove child container))
    (run (js-empty (id-of container)) container)
    (dolist (child new-child-widgets)
      (run (js-iappend (shtml-of child) (id-of container)) container)
      (propagate-for-add child container)
      (render child))))|#


(defmethod exchange ((container container) (widget-a widget) (widget-b widget))
  (with-object container
    (setf ¤children (amx:exchange widget-a widget-b ¤children)))
  (when (visible-p-of container)
    (run (js-exchange (id-of widget-a) (id-of widget-b)) container)))


(defmethod child-of ((container container))
  "If there is only one child in CONTAINER returns that child widget,
else return the same as CHILDREN-OF would.
\(second (child-of container)) ;; looks better than children-of"
  (let ((children (children-of container)))
    (if (cdr children)
        children
        (first children))))


#|(defmethod (setf child-of) ((new-child widget) (container container))
  "Set NEW-CHILD as single child of CONTAINER.
Returns NEW-CHILD."
  (setf (children-of container) (list new-child))
  new-child)|#


#|(defmethod html-of ((container container))
  (if *js-code-only-p*
      (js-html-of (id-of (child-of container)))
      (html-of (child-of container))))|#


#|(defmethod (setf html-of) (new-html (container container) &key server-only-p)
  (if *js-code-only-p*
      (setf (js-html-of (id-of (child-of container))) new-html)
      (setf (html-of (child-of container) :server-only-p server-only-p) new-html)))|#
