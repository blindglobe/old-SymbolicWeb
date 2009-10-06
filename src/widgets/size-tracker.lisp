;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)


(defclass size-tracker ()
  ((inner-width ;;:type (or fixnum null)
                :initform λvnil)

   (inner-height ;;:type (or fixnum null)
                 :initform λvnil)))


(defmethod initialize-instance :after ((st size-tracker) &key)
  (assert (subtypep (type-of st) 'widget))
  (with (make-instance 'callback-box :widget st :id "resize")
    (with-formula st
      (when-let (event ~(event-cell-of it))
        (let* ((width-str (cdr (find "width" event :key #'car :test #'string=)))
               (width (parse-integer width-str))
               (height-str (cdr (find "height" event :key #'car :test #'string=)))
               (height (parse-integer height-str)))
          ;; TODO: 3k is probably too big, and it should be configurable anyways.
          (when (< 3000 width) (setf width 3000))
          (when (< 3000 height) (setf height 3000))
          (when (zerop width) (incf width))
          (when (zerop height) (incf height))
          (unless (and (eql (inner-width-of st) width)
                       (eql (inner-height-of st) height))
            (setf ~(slot-value st 'inner-width) width
                  ~(slot-value st 'inner-height) height)
            (redraw st)))))))


(defmethod inner-width-of ((st size-tracker))
  ~(slot-value st 'inner-width))


(defmethod inner-height-of ((st size-tracker))
  ~(slot-value st 'inner-width))


(flet ((resize-msg (st)
         (js-msg (id-of st) "resize"
                 :callback-data `(("width"  . ,(format nil "return $('#~A').innerWidth();" (id-of st)))
                                  ("height" . ,(format nil "return $('#~A').innerHeight();" (id-of st)))))))


  (defmethod render :after ((st size-tracker))
    (run (catstr "$(window).bind('resize." (id-of st) "', "
                 "$.debounce(" (resize-msg st) ", 250));" +lf+) ;; TODO: Hardcoded debounce-value.
         st)
    ;; TODO: Need a way to ensure that this is only triggered once pr. "round-trip".
    (run (catstr (resize-msg st) "();" +lf+)
         st)))


(defmethod container-remove :before (container (st size-tracker))
  (run (catstr "$(window).unbind('resize." (id-of st) "');" +lf+)
       st))


(defmethod redraw ((st size-tracker))
  )



(defclass container-st (size-tracker container)
  ())