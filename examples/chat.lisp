;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)


(defclass conversation-area (container)
  ())


(defmethod initialize-instance :after ((conversation-area conversation-area) &key)
  (setf (css-overflow-of conversation-area) "auto"))


(defmethod handle-model-event :after (object (conversation-area conversation-area)
                                             (event sw-mvc:container-insert))
  "Make sure that any new message added to CONVERSATION-AREA scrolls into view."
  (scroll-to-bottom conversation-area))



(defclass chat-app (application)
  ((conversation-area :reader conversation-area-of
                      :allocation :class
                      :initform (dlist))))

(set-uri 'chat-app "chat")


(defmethod render-viewport ((viewport viewport) (app chat-app))
  (let ((conversation-area-view (with1 (make-instance 'conversation-area :model (conversation-area-of app))
                                  (setf (css-height-of it) "300px")))

        (text-input-view (with1 (make-instance 'text-input :clear-on-enterpress-p t)
                           (setf (css-width-of it) "100%")
                           (with-event (value) (on-event-enterpress it)
                             (insert λVvalue :in (conversation-area-of app))))))
    (insert
     (mk-html ()
       (:div
         (:h1 "CHAT-APP")
         (:sw conversation-area-view)
         (:sw text-input-view)
         (:a :href "http://github.com/lnostdal/SymbolicWeb/blob/master/examples/chat.lisp"
             "source code")))
     :in (root))

    (scroll-to-bottom conversation-area-view)))
