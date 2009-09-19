;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass tab-pane (container)
  ((tab :reader tab-of :initarg :tab
        :initform (error ":TAB needed.")))
  (:default-initargs
   :element-type "ul"))



(defmethod initialize-instance :after ((tab-pane tab-pane) &key)
  (add-class tab-pane "tab-pane")
  (with-formula tab-pane
    (withp ~(active-item-of ~(tab-of tab-pane))
      #|(dbg-prin1 (view-in-context-of tab-pane (sw-mvc:left-of it)))|#
       #|(dbg-prin1 ~~tab-pane)|#
       #|(dbg-prin1 (eq it ~tab-pane))|#)))


(defmethod view-constructor ((tab-pane tab-pane) (model cell))
  (with1 (make-instance 'container :element-type "li")
    (insert (with1 (make-instance 'html-element :element-type "a" :model model)
              (setf (href-of it) "#"))
            :in it)))




(defmethod view-constructor ((tab-pane tab-pane) (pair pair))
  (with1 (view-in-context-of tab-pane (sw-mvc:left-of pair) t)
    (with-formula it
      (when (on-click-of it)
        (setf (active-item-of ~(tab-of tab-pane)) pair)))))



(defclass tab (container)
  ((pane :reader pane-of)

   (content :reader content-of
            :initform (with1 (make-instance 'container)
                        (add-class it "tab-content")))

   (on-active-label :accessor on-active-label-of
                    :initform (lambda (label)
                                (add-class label "tab-pane-active")))

   (on-inactive-label :accessor on-inactive-label-of
                      :initform (lambda (label)
                                  (remove-class label "tab-pane-active"))))


  (:default-initargs
   :model (make-instance 'sw-mvc:container-with-1-active-item)))


(defmethod initialize-instance :after ((tab tab) &key)
  (add-class tab "tab")
  (setf (slot-value tab 'pane) (make-instance 'tab-pane :tab tab)
        (slot-value tab 'children) (list (pane-of tab) (content-of tab))))



(defmethod (setf model-of) ((model container-with-1-active-item) (tab tab))
  (prog1
      (list
       #λ(withp (event-of model)
           (when (eq model (model-of (container-of it)))
             (handle-model-event tab it)))

       (let ((old-active-item nil))
         #λ(withp ~(active-item-of model)
             (funcall (on-active-label-of tab) (view-in-context-of (pane-of tab) (sw-mvc:left-of it) t))
             (withp (and old-active-item
                         (view-in-context-of (pane-of tab) (sw-mvc:left-of old-active-item)))
               (funcall (on-inactive-label-of tab) it))
             (setf old-active-item it)
             (remove-all (content-of tab))
             (insert (view-in-context-of tab (sw-mvc:right-of it) t)
                     :in (content-of tab)))))

    (do ((dlist-node (head-of model) (sw-mvc:right-of dlist-node)))
        ((null dlist-node))
      (container-insert tab (view-in-context-of tab ~dlist-node t)))))


(defmethod handle-model-event ((tab tab) (event sw-mvc:container-insert))
  (handle-model-event (pane-of tab) event))


(defmethod handle-model-event ((tab tab) (event sw-mvc:container-remove))
  (handle-model-event (pane-of tab) event))





(terpri) (terpri)
(unload-css "some-css-id")
(load-css "some-css-id" "http://nostdal.org/sw-static/css/tab.css" :force-p t)
(remove-all (root))
(let ((tab (make-instance 'tab :id "my-tab")))
  (setf (margin-of tab) "10px")
  (let ((x (mk-pair #λ"label-1" (dlist #λ"content-1"
                                       (b "blah")
                                       #λ"more-content-1")))
        (y (mk-pair #λ"label-2" #λ"content-2"))
        (z (mk-pair (em "label-3") (b "content-3"))))
    (insert tab :in (root))
    (insert (list x y z) :in tab)))
