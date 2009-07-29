;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/focussable.lisp))


#| TODO:
Add optional focus tracking using a custom ON-FOCUS event.
|#

(defclass focussable ()
  ()
  (:default-initargs
   :focussable-p t))