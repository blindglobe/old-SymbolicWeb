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
          (when (eq model (model-of (container-of event)))
            (handle-model-event container event)))

      (do ((dlist-node (head-of model) (sw-mvc:right-of dlist-node)))
          ((null dlist-node))
        (container-insert container (view-in-context-of container ~dlist-node t)))))


(defmethod handle-model-event ((container container) (event sw-mvc:container-insert))
  (flet ((mk-view (object)
           (etypecase object
             ;; TODO: With the change in VIEW-IN-CONTEXT-OF this clause might not be needed anymore.
             (view-base
              object)

             (dlist-node
              (view-in-context-of container ~object t))

             (model
              (view-in-context-of container object t)))))

    (let ((relative-object (relative-object-of event)))
      (if relative-object
          (let ((relative-widget (view-in-context-of container relative-object))
                (relative-position (relative-position-of event)))
            (dolist (object (objects-of event))
              (ecase relative-position
                  (:before
                   (let ((new-widget (mk-view object)))
                     (container-insert container new-widget :before relative-widget)
                     (setf relative-widget new-widget
                           relative-position :after)))

                  (:after
                   (let ((new-widget (mk-view object)))
                     (container-insert container new-widget :after relative-widget)
                     (setf relative-widget new-widget))))))
          (dolist (object (objects-of event))
            (container-insert container (mk-view object)))))))


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


(defmethod render ((container container))
  (let ((container-id (id-of container)))
    (dolist (child (children-of container))
      (run (js-iappend (shtml-of child) container-id) container)
      (render child))))


#.(maybe-inline 'propagate-for-add)
(defn propagate-for-add (null ((widget widget) (container container-base)))
  (let* ((viewport (viewport-of container))
         (widgets (widgets-of (application-of viewport))))
    (with-each-widget-in-tree (:root widget)
      (setf (gethash (id-of widget) widgets) widget
            (viewport-of widget) viewport))))


#.(maybe-inline 'propagate-for-remove)
(defn propagate-for-remove (null ((widget widget)))
  (let* ((viewport (viewport-of widget))
         (widgets (widgets-of (application-of viewport))))
    (with-each-widget-in-tree (:root widget)
      (remhash (id-of widget) widgets)
      (nilf (viewport-of widget)))))


(defmethod container-insert ((container container) (widget widget) &key before after)
  "Implements SW-MVC:INSERT for SW:CONTAINER widget."
  (when-let (it (or before after))
    (check-type it widget))
  (when-commit ()
    (cond
     (after
      (amx:insert widget ↺(slot-value container 'children) :after after)
      (when (visible-p-of container)
        (propagate-for-add widget container)
        (run (js-oappend (shtml-of widget) (id-of widget)) container)
        (render widget)))

     (before
      (amx:insert widget ↺(slot-value container 'children) :before before)
      (when (visible-p-of container)
        (propagate-for-add widget container)
        (run (js-oprepend (shtml-of widget) (id-of widget)) container)
        (render widget)))

     (t
      (amx:insert widget ↺(slot-value container 'children) :last-p t)
      (when (visible-p-of container)
        (propagate-for-add widget container)
        (run (js-iappend (shtml-of widget) (id-of container)) container)
        (render widget))))))


(defmethod container-remove ((container container) (widget widget))
  "Implements SW-MVC:REMOVE and SW-MVC:REMOVE-ALL for SW:CONTAINER."
  (when-commit ()
    (deletef (slot-value container 'children) widget)
    (when (visible-p-of container)
      (run (js-remove (id-of widget)) container)
      (propagate-for-remove widget))))


(defmethod container-exchange ((container container) (widget-a widget) (widget-b widget))
  (when-commit ()
    (with-object container
      (setf ¤children (amx:exchange widget-a widget-b ¤children)))
    (when (visible-p-of container)
      (run (js-exchange (id-of widget-a) (id-of widget-b)) container))))


(defmethod child-of ((container container))
  "If there is only one child in CONTAINER returns that child widget,
else return the same as CHILDREN-OF would.
\(second (child-of container)) ;; looks better than children-of"
  (let ((children (children-of container)))
    (if (cdr children)
        children
        (first children))))
