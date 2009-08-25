;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/container.lisp))

#| NOTE:
This should be thread safe because all changes of CONTAINER (View) should come from the Model end.
This is true even though the CHILDREN slot (from CONTAINER-BASE) isn't handled by SW-STM.
It also holds while CONTAINER is currently being rendered. |#


(defclass container (widget container-base)
  ()

  (:default-initargs
   :model (dlist)))


(defmethod view-constructor ((container container) (model cell))
  (make-instance 'html-element :model model))


(defmethod view-constructor ((container container) (model multiple-value-model))
  (make-instance 'container :model model))


(defmethod (setf model-of) ((model dlist) (container container))
  (prog1
      #λ(when-let (event (event-of model))
          (when-commit ()
            (handle-model-event container event)))

      (do ((dlist-node (head-of model) (sw-mvc:right-of dlist-node)))
          ((null dlist-node))
        (when-commit ()
          (container-add container (view-in-context-of container dlist-node))))))


(defmethod handle-model-event ((container container) (event sw-mvc:container-insert))
  (flet ((mk-view (object)
           (etypecase object
             (view-base
              object)

             (dlist-node
              (view-in-context-of container ~object t))

             (model
              (view-in-context-of container object t)))))

    (let ((relative-position (relative-position-of event))
          (relative-object (relative-object-of event)))
      (if relative-object
          (let ((relative-widget (view-in-context-of container relative-object)))
            (dolist (object (objects-of event))
              (let ((new-widget (mk-view object)))
                (ecase relative-position
                  (:before
                   (container-insert container new-widget :before relative-widget)
                   (setf relative-widget new-widget
                         relative-position :after))
                  (:after
                   (container-insert container new-widget :after relative-widget)
                   (setf relative-widget new-widget))))))

        (dolist (object (objects-of event))
          (container-add container (mk-view object)))))))


(defmethod handle-model-event ((container container) (event sw-mvc:container-remove))
  (dolist (object (objects-of event))
    (container-remove container
                      (etypecase object
                        (view-base
                         object)

                        (dlist-node
                         (view-in-context-of container ~object))

                        (model
                         (view-in-context-of container object))))))


(defmethod handle-model-event ((container container) (event sw-mvc:container-exchange))
  (container-exchange container
                      (view-in-context-of container (object-of event))
                      (view-in-context-of container (target-position-of event))))


(defmethod render :around ((container container))
  ;; We'd like to render a snapshot of the entire container Model; so we "lock" it (STM).
  ~~container
  (call-next-method))


(defmethod render ((container container))
  (let ((container-id (id-of container)))
    (dolist (child (children-of container))
      (run (js-iappend (shtml-of child) container-id) container)
      (render child))))


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


;; TODO: Get rid of this and add a :IN keyarg to CONTAINER-INSERT instead.
(defmethod container-add ((container container) (widget widget))
  "Add or append WIDGET to CONTAINER.
Returns WIDGET."
  (prog1 widget
    (amx:insert widget ↺(slot-value container 'children) :last-p t)
    (when (visible-p-of container)
      (run (js-iappend (shtml-of widget) (id-of container)) container)
      (propagate-for-add widget container)
      (render widget))))


(defmethod container-insert ((container container) (widget widget) &key before after)
  "Inserts NEW-WIDGET \"left\" or \"right\" of an already existing widget
depending on whether :BEFORE or :AFTER is supplied."
  #|(declare (inline oadd oprepend))|#
  (prog1 widget
    (cond
      (after
       (amx:insert widget ↺(slot-value container 'children) :after after)
       (when (visible-p-of container)
         (run (js-oappend (shtml-of widget) (id-of widget))
              container)
         (propagate-for-add widget container)
         (render widget)))

      (before
       (amx:insert widget ↺(slot-value container 'children) :before before)
       (when (visible-p-of container)
         (run (js-oprepend (shtml-of widget) (id-of widget)) container)
         (propagate-for-add widget container)
         (render widget)))

      (t
       (error ":AFTER or :BEFORE must be supplied.")))))


(defmethod container-exchange ((container container) (widget-a widget) (widget-b widget))
  (with-object container
    (setf ¤children (amx:exchange widget-a widget-b ¤children)))
  (when (visible-p-of container)
    (run (js-exchange (id-of widget-a) (id-of widget-b)) container)))


(defmethod container-remove ((container container) (widget widget))
  "Remove WIDGET.
Returns WIDGET."
  (prog1 widget
    (deletef (slot-value container 'children) widget)
    (when (visible-p-of container)
      (propagate-for-remove widget container)
      (run (js-remove (id-of widget)) container))))







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



#|(defmethod add ((container container) (widget widget) (left-widget widget))
  "Inserts WIDGET as a sibling right of LEFT-WIDGET. Does the \"same as\"
jQuery's 'after' function.
Returns WIDGET."
  (declare (inline oadd))
  (oadd container widget left-widget))|#


#|(defmethod prepend ((container container) (widget widget))
  "Prepend WIDGET to CONTAINER.
Returns WIDGET."
  (prog1 widget
    (setf (slot-value container 'children)
          (cons widget (slot-value container 'children)))
    (when (visible-p-of container)
      (run (js-iprepend (shtml-of widget) (id-of container)) container)
      (propagate-for-add widget container)
      (render widget))))|#


#|(defmethod prepend ((container container) (widget widget) (right-widget widget))
  "Inserts WIDGET as a sibling left of RIGHT-WIDGET.
Returns WIDGET."
  (declare (inline oprepend))
  (oprepend container widget right-widget))|#


#|(defmethod prepend-to ((container container) &rest widgets)
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
          (nconc widgets (slot-value container 'children)))))|#


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


#|(defmethod remove-all ((container container)
                       &aux (old-children (slot-value container 'children)))
  "Remove all widgets (directly contained) in CONTAINER.
Returns the number of widgets removed."
  (nilf (slot-value container 'children))
  (prog1 (length old-children)
    (when (visible-p-of container)
      (dolist (child old-children)
        (propagate-for-remove child container))
      (run (js-empty (id-of container)) container))))|#


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
