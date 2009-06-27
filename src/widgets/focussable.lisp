;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/focussable.lisp))


(defclass focussable ()
  ()
  (:default-initargs
   :focussable-p t))


(defmethod initialize-instance :after ((widget focussable) &key)
  λ(when (on-focus-of widget)
     (focus widget :server-only-p t)))