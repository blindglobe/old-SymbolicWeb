;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(declaim (optimize speed))


(defclass container (widget
                     container-base)
  ()

  (:default-initargs
   :model (dlist)
   :view-constructor-fn (lambda (context-view model &key)
                          (declare (ignore context-view))
                          (assert (not (typep model 'sw-mvc:container)))
                          (make-instance 'html-element :model model)))

  (:metaclass mvc-stm-class)
  (:documentation "
This is a widget which contain other widgets. The child-widgets are always
rendered in a pre-determined order; \"left-to-right\". The visual outcome of
this can be changed by specifying CSS, or based on what ELEMENT-TYPE (initarg
from WIDGET) is used.
When more control of rendering or rendering order is needed, a HTML-CONTAINER
widget can be used - or you can create your own subclass of CONTAINER and
override behaviour in a custom RENDER method."))


(defmethod initialize-instance :after ((container container) &key
                                       (element-type "div")
                                       (display nil display-supplied-p))
  (unless (slot-boundp container 'shtml)
    (setf (slot-value container 'shtml)
          (catstr "<" element-type " id='" (id-of container) "'"
                  (if display-supplied-p
                      (catstr " style='display: " display ";'")
                      "")
                  "></" element-type ">"))))


(defmethod handle-view-set-object-model ((container container) (model dlist))
  (remove-all container)
  (do ((node (head-of model) (sw-mvc:right-of node)))
      ((null node))
    (add (view-in-context-of container node) container)))


(defmethod handle-model-event ((container container) (model dlist) (event container-insert))
  (with-slots (relative-position relative-object) event
    (let* ((object (object-of event))
           (new-widget (view-in-context-of container object)))
      (if relative-object
          (case relative-position
            (:before
             (if-let (left (sw-mvc:left-of relative-object))
               (insert new-widget :after (view-in-context-of container left))
               (prepend new-widget container)))
            
            (:after
             (insert new-widget :after (view-in-context-of container relative-object)))

            (otherwise
             (error "(HANDLE-MODEL-EVENT ~A ~A ~A): Unknown RELATIVE-POSITION: ~A"
                    container model event relative-position)))
          
          (add new-widget container)))))


;; TODO: specialize on EVENT..
(defmethod handle-model-event ((container container) (model dlist) event)
  (typecase event
    (container-remove
     (dolist (object (objects-of event))
       (remove (view-in-context-of container object) container)))
     
    (otherwise
     (error "(HANDLE-MODEL-EVENT ~A ~A ~A): No handler for event ~A for View ~A"
            container model event event container))))



(defmethod render :around ((container container))
  ;; Make sure we don't push JS code to sessions that might already be viewing CONTAINER.
  (with-code-block (:widget container)
    (call-next-method)))


(defmethod render ((container container))
  (let ((container-id (id-of container)))
    (dolist (child (children-of container))
      (run (js-iappend (shtml-of child) container-id) container)
      (render child))))


#.(maybe-inline 'propagate-for-add)
(defun propagate-for-add (widget container)
  (declare (widget widget)
           (container container)
           (optimize speed))
  (with-each-widget-in-tree (:root widget)
    ;; Add widget to viewports of CONTAINER.
    (with-each-viewport-of-widget (:widget container)
      ;; VIEWPORT -> WIDGET.
      (setf (gethash (id-of widget) (widgets-of viewport)) widget)
      ;; APPLICATION -> WIDGET (weak hash; we won't bother removing these "manually" later)
      (setf (gethash (id-of widget) (widgets-of (application-of viewport))) widget)
      ;; WIDGET -> VIEWPORT.
      (setf (gethash (id-of viewport) (viewports-of widget)) viewport))
                           
    (unless (visible-p-of widget)
      ;; Since we know CONTAINER is visible, make sure entire branch is marked as
      ;; visible also.
      (tf (slot-value widget 'visible-p))
      #|(do-visibility-change widget t)|#)))


#.(maybe-inline 'propagate-for-remove)
(defun propagate-for-remove (widget container)
  (declare (widget widget)
           (container container)
           (optimize speed))
  (with-each-widget-in-tree (:root widget)
    ;; Remove widget from viewports of CONTAINER.
    (with-each-viewport-of-widget (:widget container)
      ;; VIEWPORT -/-> WIDGET.
      (remhash (id-of widget) (widgets-of viewport))
      ;; WIDGET -/-> VIEWPORT.
      (remhash (id-of viewport) (viewports-of widget)))

    (visible-p-of widget :real-check-p t)))


(defmethod add ((widget widget) (container container))
  "Add or append WIDGET to CONTAINER.
Returns WIDGET."
  (when (visible-p-of container)
    (push widget (slot-value container 'children))
    (run (js-iappend (shtml-of widget) (id-of container)) container)            ;; #0: JS
    (propagate-for-add widget container)                                        ;; #1: ON-VISIBILITY-CHANGE-FNS (NOTE: This needs to be done before calling RENDER because of the WITH-VISIBLE-CONTEXTS-OF form in widgets/events.lisp)
    (with-code-block (:widget container)
      (render widget)))                                                         ;; #2: ON-RENDER-FNS
  widget)


(defmethod remove ((widget widget) &optional
                   (container (error "CONTAINER needed.")))
  "Remove WIDGET.
Returns WIDGET."
  (when (visible-p-of container)
    (propagate-for-remove widget container)                                     ;; #1: ON-VISIBILITY-CHANGE-FNS
    (run (js-remove (id-of widget)) container))                                 ;; #0: JS
  widget)


(defmethod add-to ((container container) &rest widgets)
  "Adds or appends each widget in WIDGETS to CONTAINER in sequence.
Returns WIDGETS."
  (when (visible-p-of container)
    (dolist (widget widgets)
      (push widget (slot-value container 'children))
      (run (js-iappend (shtml-of widget) (id-of container)) container)          ;; #0: JS
      (propagate-for-add widget container)                                      ;; #1: ON-VISIBILITY-CHANGE-FNS
      (with-code-block (:widget container)
        (render widget)))))                                                     ;; #2: ON-RENDER-FNS


(defmethod oadd ((widget widget) (left-widget widget))
  "Inserts WIDGET as a sibling right of LEFT-WIDGET. Does the \"same as\"
jQuery's 'after' function.
Returns WIDGET."
  (let ((container (parent-of left-widget)))
    (when (visible-p-of container)
      (run (js-oappend (shtml-of widget) (id-of left-widget)) container)        ;; #0: JS
      (propagate-for-add widget container)                                      ;; #1: ON-VISIBILITY-CHANGE-FNS
      (with-code-block (:widget container)
        (render widget))))                                                      ;; #2: ON-RENDER-FNS
  widget)


(defmethod insert ((new-widget widget) &key before after)
  "Inserts NEW-WIDGET \"left\" or \"right\" of an already existing widget
depending on whether :BEFORE or :AFTER is supplied."
  (declare (inline oadd oprepend))
  (cond
    (after  (oadd new-widget after))
    (before (oprepend new-widget before))
    (t (error ":AFTER or :BEFORE must be supplied."))))


(defmethod add ((widget widget) (left-widget widget))
  "Inserts WIDGET as a sibling right of LEFT-WIDGET.  Does the \"same as\"
jQuery's 'after' function.
Returns WIDGET."
  (declare (inline oadd))
  (oadd widget left-widget))


(defmethod prepend ((widget widget) (container container))
  "Prepend WIDGET to CONTAINER.
Returns WIDGET."
  (when (visible-p-of container)
    (run (js-iprepend (shtml-of widget) (id-of container)) container)           ;; #0: JS
    (propagate-for-add widget container)                                        ;; #1: ON-VISIBILITY-CHANGE-FNS
    (with-code-block (:widget container)
      (render widget)))                                                         ;; #2: ON-RENDER-FNS
  widget)


(defmethod oprepend ((widget widget) (right-widget widget))
  "Inserts WIDGET as a sibling left of RIGHT-WIDGET. Does the \"same as\"
jQuery's 'before' function.
Returns WIDGET."
  (let ((container (parent-of right-widget)))
    (when (visible-p-of container)
      (run (js-oprepend (shtml-of widget) (id-of right-widget)) container)      ;; #0: JS
      (propagate-for-add widget container)                                      ;; #1: ON-VISIBILITY-CHANGE-FNS 
      (with-code-block (:widget container)
        (render widget))))                                                      ;; #2: ON-RENDER-FNS
  widget)


(defmethod prepend ((widget widget) (right-widget widget))
  "Inserts WIDGET as a sibling left of RIGHT-WIDGET.
Returns WIDGET."
  (declare (inline oprepend))
  (oprepend widget right-widget))


(defmethod prepend-to ((container container) &rest widgets)
  "Prepends each widget in WIDGETS to CONTAINER in sequence.
Returns WIDGETS."
  (when (visible-p-of container)
    (dolist (widget widgets)
      (run (js-iprepend (shtml-of widget) (id-of container)) container)         ;; #0: JS
      (propagate-for-add widget container)                                      ;; #1: ON-VISIBILITY-CHANGE-FNS
      (with-code-block (:widget container)
        (render widget))))                                                      ;; #2: ON-RENDER-FNS
  widgets)


(defmethod replace ((old-widget widget) (new-widget widget))
  "Replace OLD-WIDGET with NEW-WIDGET.
Returns NEW-WIDGET."
  (let ((container (parent-of new-widget))) ;; At this point the model has updated so this is correct.
    (when (visible-p-of container)
      (propagate-for-remove old-widget container)                               ;; #1: ON-VISIBILITY-CHANGE-FNS
      (run (js-replace-with (id-of old-widget) (shtml-of new-widget)) container);; #0: JS
      (propagate-for-add new-widget container)                                  ;; #1: ON-VISIBILITY-CHANGE-FNS
      (with-code-block (:widget container)                                   
        (render new-widget))))                                                  ;; #2: ON-RENDER-FNS
  new-widget)


(defmethod remove-all ((container container)
                       &aux (old-children (slot-value container 'children)))
  "Remove all widgets (directly contained) in CONTAINER.
Returns number of widgets removed."
  (prog1 (length old-children)
    (when (visible-p-of container)
      (dolist (child old-children)
        (propagate-for-remove child container))                                 ;; #1: ON-VISIBILITY-CHANGE-FNS
      (run (js-empty (id-of container)) container))))                           ;; #0: JS


(defmethod (setf children-of) ((new-child-widgets list) (container container)
                               &aux (old-children (slot-value container 'children)))
  "Note that this will not remove NEW-CHILD-WIDGETS from any containers they might
already be in."
  (when (visible-p-of container)
    (dolist (child old-children)
      (propagate-for-remove child container))                                   ;; #1: ON-VISIBILITY-CHANGE-FNS 
    (run (js-empty (id-of container)) container)                                ;; #0: JS   
    (dolist (child new-child-widgets)
      (run (js-iappend (shtml-of child) (id-of container)) container)           ;; #0: JS
      (propagate-for-add child container)                                       ;; #1: ON-VISIBILITY-CHANGE-FNS
      (with-code-block (:widget container)
        (render child)))))                                                      ;; #2: ON-RENDER-FNS


(defmethod exchange ((widget-a widget) (widget-b widget))
  (let ((container (parent-of widget-a)))
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


(defmethod (setf child-of) ((new-child widget) (container container))
  "Set NEW-CHILD as single child of CONTAINER.
Returns NEW-CHILD."
  (setf (children-of container) (mklst new-child))
  new-child)


(defmethod html-of ((container container))
  (if *js-code-only-p*
      (js-html-of (id-of (child-of container)))
      (html-of (child-of container))))


(defmethod (setf html-of) (new-html (container container) &key server-only-p)
  (if *js-code-only-p*
      (setf (js-html-of (id-of (child-of container))) new-html)
      (setf (html-of (child-of container) :server-only-p server-only-p) new-html)))
      

#.(maybe-inline 'children-p)
(defun children-p (arg)
  "Will attempt to determine whether ARG is a valid argument for the :CHILDREN
initarg for CONTAINER. Returns T if it is.
If ARG is a list this will assume it is a list of widgets; it will not
traverse the list and check the type of each element."
  (or (widget-p arg) (listp arg)))


#.(maybe-inline 'children<-)
(defun children<- (arg)
  (if (children-p arg)
      arg
      ;; We don't know what HTML this is; it could be inline- or block-level,
      ;; so we place it in a DIV. PS: HTML is silly stuff. x)
      (mk-div arg)))


(defmethod print-object ((container container) stream)
  (format stream " :CHILDREN ~A" (children-of container)))


(declaim (inline mk-container))
(defun mk-container (children &rest initargs)
  (apply #'make-instance 'container
         :children (children<- children)
         initargs))  


