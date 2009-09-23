;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

#| TODO:
Update ACTIVE-ITEM slot when user selects a tab on the client end.
Update tab on the client end when ACTIVE-ITEM slot is changed.
|#


(progn
  (inject-css "jquery-ui-css"
              (catstr
               (read-file-into-string (catstr (static-data-fs-path-of *server*)
                                              "jquery-ui/themes/base/jquery-ui.css"))
               (read-file-into-string (catstr (static-data-fs-path-of *server*)
                                              "jquery-ui/themes/base/ui.tabs.css"))))

  (run (read-file-into-string (catstr (static-data-fs-path-of *server*)
                                      "jquery-ui/ui/minified/jquery-ui.min.js"))
       *viewport*)

  (run (read-file-into-string (catstr (static-data-fs-path-of *server*)
                                      "jquery-ui/ui/minified/ui.tabs.min.js"))
       *viewport*))



(defclass tab-pane (container)
  ((label :initarg :label
          :initform (error ":LABEL needed.")))

  (:metaclass mvc-class))


(defmethod label-of ((tab-pane tab-pane))
  (princ-to-string (slot-value tab-pane 'label)))


#|(defmethod initialize-instance :after ((tab-pane tab-pane) &key)
  ;; TODO: Finish this.
  (with-formula tab-pane
    (let ((label (label-of tab-pane)))
      )))|#


(defclass tab (container)
  ()
  (:default-initargs
   :model (make-instance 'sw-mvc:container-with-1-active-item)))


(defmethod view-constructor ((tab tab) (model cell))
  (error "..."))


(defmethod view-constructor ((tab tab) (model multiple-value-model))
  (error "..."))


(defmethod view-constructor ((tab tab) (pair pair))
  (make-instance 'tab-pane
                 :label (with (sw-mvc:left-of pair)
                          (typecase it
                            (pointer it)
                            (t (mk-ptr it))))
                 :model (sw-mvc:right-of pair)))


(defmethod render ((tab tab))
  (with (id-of tab)
    (run (js-iappend "<ul></ul>" it) tab)
    (run (fmtn "$('#~A').tabs();~%" it) tab)
    (dolist (child (children-of tab))
      (check-type child tab-pane)
      (run (fmtn "$(\"#~A\").tabs(\"add\", \"#~A\", decodeURIComponent(\"~A\"));~%"
                 it (id-of child) (url-encode (label-of child)))
           tab)
      (render child))))


(defmethod container-insert ((tab tab) (tab-pane tab-pane) &key before after)
  (when-let (it (or before after))
    (check-type it tab-pane))
  (when-commit ()
    (cond
     (after
      (amx:insert tab-pane ↺(slot-value tab 'children) :after after)
      (when (visible-p-of tab)
        (propagate-for-add tab-pane tab)
        (run (fmtn "$(\"#~A\").tabs(\"add\", \"#~A\", decodeURIComponent(\"~A\"), ~A);~%"
                   (id-of tab) (id-of tab-pane) (url-encode (label-of tab-pane))
                   (position tab-pane (slot-value tab 'children)))
             tab)
        (render tab-pane)))

     (before
      (amx:insert tab-pane ↺(slot-value tab 'children) :before before)
      (when (visible-p-of tab)
        (propagate-for-add tab-pane tab)
        (run (fmtn "$(\"#~A\").tabs(\"add\", \"#~A\", decodeURIComponent(\"~A\"), ~A);~%"
                   (id-of tab) (id-of tab-pane) (url-encode (label-of tab-pane))
                   (position tab-pane (slot-value tab 'children)))
             tab)
        (render tab-pane)))

     (t
      (amx:insert tab-pane ↺(slot-value tab 'children) :last-p t)
      (when (visible-p-of tab)
        (propagate-for-add tab-pane tab)
        (run (fmtn "$(\"#~A\").tabs(\"add\", \"#~A\", decodeURIComponent(\"~A\"));~%"
                   (id-of tab) (id-of tab-pane) (url-encode (label-of tab-pane)))
             tab)
        (render tab-pane))))))


(defmethod container-remove ((tab tab) (tab-pane tab-pane))
  (when-commit ()
    (let ((pos (position tab-pane (slot-value tab 'children))))
      (deletef (slot-value tab 'children) tab-pane)
      (when (visible-p-of tab)
        (run (fmtn "$(\"#~A\").tabs(\"remove\", ~A);~%"
                   (id-of tab) pos)
             tab)
        (propagate-for-remove tab-pane)))))



(remove-all (root))

(with (make-instance 'tab)
  (insert it :in (root))
  (insert (mk-pair λv"label-1" (dlist λv"content-1")) :in it)
  (let ((pane-2 (mk-pair λv"label-2" (dlist λv"content-2"))))
    (insert pane-2 :in it)
    (let ((c (make-instance 'container)))
      (insert (mk-pair λv"label-3" c) :in it)
      (insert (mk-html ()
                (:div
                 (:h1 "Hello World!")
                 (:p "Ok, this is a test. Blabla.")))
              :in c))
    #|(sleep 1)|#
    #|(remove pane-2 it)|#))






#|
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
|#