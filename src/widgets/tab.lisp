;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass tab-pane (container)
  ((tab :reader tab-of :initarg :tab
        :initform (error ":TAB needed."))))


(defmethod view-constructor ((tab-pane tab-pane) (model cell))
  (span (:model model)))


(defmethod view-constructor ((tab-pane tab-pane) (pair pair))
  (with1 (view-in-context-of tab-pane (sw-mvc:left-of pair) t)
    (setf (border-of it) "solid 1px green"
          (cursor-of it) "pointer")
    (with-formula it
      (when (on-click-of it)
        (setf (active-item-of ~(tab-of tab-pane)) pair)))))



(defclass tab (container-base widget)
  ((pane :reader pane-of
         :initform (make-instance 'tab-pane :tab (self)))

   (content :reader content-of
            :initform (make-instance 'container)))

  (:default-initargs
   :model (make-instance 'sw-mvc:container-with-1-active-item)))


(defmethod initialize-instance :after ((tab tab) &key)
  (setf (border-of tab) "1px solid red"))


(defmethod children-of ((tab tab))
  (list (pane-of tab) (content-of tab)))


(defmethod render ((tab tab))
  (let ((tab-id (id-of tab)))
    (run (js-iappend (shtml-of (pane-of tab)) tab-id) tab)
    (render (pane-of tab))
    (run (js-iappend (shtml-of (content-of tab)) tab-id) tab)
    (render (content-of tab))))


(defmethod (setf model-of) ((model container-with-1-active-item) (tab tab))
  (prog1
      (list
       #λ(withp (event-of model)
           (handle-model-event tab it))

       #λ(withp (active-item-of model)
           (let ((view (view-in-context-of (content-of tab) (sw-mvc:right-of ~it) t)))
             (remove-all (content-of tab))
             (insert view :in (content-of tab)))))

    (do ((dlist-node (head-of model) (sw-mvc:right-of dlist-node)))
        ((null dlist-node))
      (container-insert tab (view-in-context-of tab ~dlist-node t)))))


(defmethod handle-model-event ((tab tab) (event sw-mvc:container-insert))
  (handle-model-event (pane-of tab) event))


(defmethod handle-model-event ((tab tab) (event sw-mvc:container-remove))
  (handle-model-event (pane-of tab) event))





(terpri) (terpri)
(remove-all (root))
(with (make-instance 'tab :id "my-tab")
  (insert it :in (root))
  (insert (mk-pair #λ"label-1" (dlist #λ"content-1" #λ"more-content-1"))
          :in it)
  (insert (mk-pair (b "label-2") #λ"content-2")
          :in it))
