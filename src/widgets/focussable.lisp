;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/focussable.lisp))


(defclass focussable ()
  ()
  (:default-initargs
   :focussable-p t))


(defmethod initialize-instance :after ((widget focussable) &key)
  (with-lifetime widget
    ;; Keep server in sync with focus state on client.
    #λ(when (on-focus-of widget)
        (focus widget :server-only-p t))))