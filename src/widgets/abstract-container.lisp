;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :widgets/abstract-container.lisp))


(defclass abstract-container (widget container-base)
  ()

  (:default-initargs
   :model (sw-mvc:dlist))

  (:documentation "This class has no VIEW-CONSTRUCTOR methods defined for it, but is otherwise \"the same\" as a CONTAINER. The point here is that while other widgets like, e.g. TAB, still having \"container like\" behavior wrt. SW-MVC type events/operations, they probably _don't_ want the CONTAINER like (specific) behavior wrt. View construction."))


(defmethod set-model nconc ((container abstract-container) (model dlist))
  (prog1
      (list λI(when-let (event (event-of model))
                (when (eq model (container-of event))
                  (dolist (object (objects-of event))
                    (handle-model-event object container event)))))

    ;; TODO: Uh. Is this right? (switching models..)
    (unless (null (children-of container))
      (remove-all container))

    (do ((dlist-node (head-of model) (sw-mvc:right-of dlist-node)))
        ((null dlist-node))
      (container-insert container (view-in-context-of container ~dlist-node t)))))


(defmethod handle-model-event (object (container abstract-container) (event sw-mvc:container-insert))
  (let ((relative-node (relative-node-of event)))
    (if relative-node
        (let ((relative-view (view-in-context-of container (deref relative-node)))
              (relative-position (relative-position-of event)))
          (ecase relative-position
            (:before
             (let ((new-view (view-in-context-of container object t)))
               (container-insert container new-view :before relative-view)
               ;; This is sort of nasty, but I can't think of a better way to do this at the moment.
               (setf (slot-value event 'relative-node) (right-of relative-node)
                     (slot-value event 'relative-position) :after)))
            (:after
             (let ((new-view (view-in-context-of container object t)))
               (container-insert container new-view :after relative-view)
               ;; This is sort of nasty, but I can't think of a better way to do this at the moment.
               (setf (slot-value event 'relative-node) (right-of relative-node))))))
        (container-insert container (view-in-context-of container object t)))))


(defmethod handle-model-event (object (container abstract-container) (event sw-mvc:container-remove))
  (container-remove container (view-in-context-of container object)))


(defmethod handle-model-event (object (container abstract-container) (event sw-mvc:container-remove-all))
  (container-remove-all container))


(defmethod handle-model-event (object (container abstract-container) (event sw-mvc:container-exchange))
  (container-exchange container
                      (view-in-context-of container object)
                      (view-in-context-of container (target-position-of event))))


(defmethod render ((container abstract-container))
  (let ((container-id (id-of container)))
    (dolist (child (children-of container))
      (run (js-iappend (shtml-of child) container-id) container)
      (render child))))


(defn propagate-for-add (null ((widget widget) (container container-base)))
  (setf (slot-value widget 'parent) container)
  (let* ((viewport (viewport-of container))
         (widgets (widgets-of (application-of viewport))))
    (with-each-widget-in-tree (:root widget)
      (with (viewport-of widget)
        (when (and it (not (eq it viewport)))
          (warn "SW:PROPAGATE-FOR-ADD: ~A is already part of ~A. Moving it to ~A." widget viewport it)))
      (setf (gethash (id-of widget) widgets) widget
            (viewport-of widget) viewport))))


(defn propagate-for-remove (null ((widget widget)))
  (nilf (slot-value widget 'parent))
  (let* ((viewport (viewport-of widget))
         (widgets (widgets-of (application-of viewport))))
    (with-each-widget-in-tree (:root widget)
      (remhash (id-of widget) widgets)
      (nilf (viewport-of widget)))))


(let ((tmp-node "<div id='sw-tmp'></div>"))
  (flet ((replace-tmp-node (widget container)
           (run (fmtn "$('#sw-tmp').replaceWith($('#~A'));~%" (id-of widget))
                container)))
    (declare (inline replace-tmp-node))


    (defmethod container-insert ((container abstract-container) (widget widget) &key before after)
      "Implements SW-MVC:INSERT for SW:CONTAINER widget."
      (when-let (it (or before after))
        (check-type it widget))
      (when-commit ()
        (cond
          (after
           (amx:insert widget ↺(slot-value container 'children) :after after)
           (when (visible-p-of container)
             (propagate-for-add widget container)
             (if (in-dom-p-of widget)
                 (progn
                   (run (js-oappend tmp-node (id-of after)) container)
                   (replace-tmp-node widget container))
                 (progn
                   (run (js-oappend (shtml-of widget) (id-of after)) container)
                   (render widget)
                   (ensure-in-client-dom widget)))))

          (before
           (amx:insert widget ↺(slot-value container 'children) :before before)
           (when (visible-p-of container)
             (propagate-for-add widget container)
             (if (in-dom-p-of widget)
                 (progn
                   (run (js-oprepend tmp-node (id-of before)) container)
                   (replace-tmp-node widget container))
                 (progn
                   (run (js-oprepend (shtml-of widget) (id-of before)) container)
                   (render widget)
                   (ensure-in-client-dom widget)))))

          (t
           (amx:insert widget ↺(slot-value container 'children) :last-p t)
           (when (visible-p-of container)
             (propagate-for-add widget container)
             (if (in-dom-p-of widget)
                 (progn
                   (run (js-iappend tmp-node (id-of container)) container)
                   (replace-tmp-node widget container))
                 (progn
                   (run (js-iappend (shtml-of widget) (id-of container)) container)
                   (render widget)
                   (ensure-in-client-dom widget)))))))


      (defmethod container-remove ((container abstract-container) (widget widget))
        "Implements SW-MVC:REMOVE for SW:CONTAINER."
        (when-commit ()
          (deletef (slot-value container 'children) widget)
          (when (visible-p-of container)
            (run (js-iappend tmp-node "sw-recycler") container)
            (replace-tmp-node widget container)
            (propagate-for-remove widget))))


      (defmethod container-remove-all ((container abstract-container))
        "Implements SW-MVC:REMOVE-ALL for SW:CONTAINER."
        (when-commit ()
          (when (visible-p-of container)
            (dolist (child (children-of container))
              (run (js-iappend tmp-node "sw-recycler") container)
              (replace-tmp-node child container)
              (propagate-for-remove child)))
          ;; TODO: Hm, the order here is different from CONTAINER-REMOVE, above. Does this matter?
          (nilf (slot-value container 'children)))))))


(defmethod container-exchange ((container abstract-container) (widget-a widget) (widget-b widget))
  (when-commit ()
    (with-object container
      (setf ¤children (amx:exchange widget-a widget-b ¤children)))
    (when (visible-p-of container)
      (run (js-exchange (id-of widget-a) (id-of widget-b)) container))))


(defmethod child-of ((container abstract-container))
  "If there is only one child in CONTAINER returns that child widget,
else return the same as CHILDREN-OF would.
\(second (child-of container)) ;; looks better than children-of"
  (let ((children (children-of container)))
    (if (cdr children)
        children
        (first children))))
