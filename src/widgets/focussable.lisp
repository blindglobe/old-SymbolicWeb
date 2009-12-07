;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/focussable.lisp))


#| TODO:
Add optional focus tracking using a custom ON-FOCUS event. (wait .. why or, what?)

To do this stuff proper (and move the LAST-FOCUS slot from the APPLICATION class to the VIEWPORT class) we must not
create a new VIEWPORT every time the user refreshes his page. To do this, I think (ok, I'm pretty sure; cookies won't
work for instance) the client needs to store the 'viewport-id' in the URL hash instead of storing it as a global
JS variable as done now.
|#


;; The machinery for this thing is in (RENDER-VIEWPORT :AFTER (VIEWPORT APPLICATION)) in viewport.lisp.
(defclass focussable ()
  ()
  (:default-initargs
   :focussable-p t))


(defmethod initialize-instance :after ((focussable focussable) &key)
  (with-event nil (on-event-focus focussable)
    (when (focussable-p-of focussable)
      (setf (slot-value *app* 'last-focus)
            focussable))))
