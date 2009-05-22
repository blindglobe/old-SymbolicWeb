;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(declaim (optimize speed))


(defclass html-element (widget)
  ((element-type :reader element-type-of :initarg :element-type
                 :initform "div"))

  (:metaclass mvc-stm-class)
  (:default-initargs
      :model #~""))


(defmethod initialize-instance :after ((html-element html-element) &key (display nil display-supplied-p))
  (with-object html-element
    (setf ¤shtml (catstr "<" ¤element-type " id='" ¤id "'"
                         (if display-supplied-p
                             (catstr " style='display: " display ";'")
                             "")
                         "></" ¤element-type ">"))))


;; Handle generic models of the SINGLE-VALUE-MODEL kind. E.g., CELL etc.
(flet ((update (html-element model)
         (muffle-compiler-note
           (run (setf (js-html-of (id-of html-element))
                      (html<- (full-deref model) html-element))
                html-element))))
  (declare (inline update))
  
  
  (defmethod handle-view-set-object-model ((html-element html-element) (model single-value-model))
    (update html-element model))
  
  
  (defmethod handle-model-slot-set-event ((html-element html-element) (model single-value-model) slot-name event)
    (update html-element model))
  
  
  (defmethod render-widget ((html-element html-element) (model single-value-model))
    (update html-element model)))
