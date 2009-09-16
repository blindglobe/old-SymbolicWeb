;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/focussable.lisp))


#| TODO:
Add optional focus tracking using a custom ON-FOCUS event.
|#

;; The machinery for this thing is in (RENDER-VIEWPORT :AFTER (VIEWPORT APPLICATION)) in viewport.lisp.
(defclass focussable ()
  ()
  (:default-initargs
   :focussable-p t))
