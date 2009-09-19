;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/container.lisp)) ;; TODO: change this.


(defclass vecto (image)
  ((inner-width :reader inner-width-of
                :initform nil)

   (inner-height :reader inner-height-of
                 :initform nil)

   (filename :initarg :filename
             :initform nil)

   (redraw-fn :accessor redraw-fn-of :initarg :redraw-fn
              :initform (lambda (vc)
                          (declare (ignore vc))
                          (error "No REDRAW method defined and REDRAW-FN slot not set."))))

  (:documentation "
Possible ways to specify how to draw:

  * Override the REDRAW method.
  * Set the REDRAW-FN slot.
  * Define a VECTO-REDRAW method."))


(defmethod filename-of ((vc vecto))
  (or (slot-value vc 'filename)
      (string-downcase (type-of vc))))


(defmethod initialize-instance :after ((vc vecto) &key)
  (with (make-instance 'callback-box :widget vc :id "resize")
    (with-formula vc
      (when-let (event ~(event-cell-of it))
        #|(dbg-prin1 event (id-of vc))|#
        (let* ((width-str (cdr (find "width" event :key #'car :test #'string=)))
               (width (parse-integer width-str))
               (height-str (cdr (find "height" event :key #'car :test #'string=)))
               (height (parse-integer height-str))
               (filename (catstr (static-data-fs-path-of *app*)
                                 (string-downcase (filename-of vc)) "-"
                                 width-str "x" height-str ".png")))
          ;; TODO: 3k is probably too big, and it should be configurable anyways.
          (when (< 3000 width) (setf width 3000))
          (when (< 3000 height) (setf height 3000))
          (when (zerop width) (incf width))
          (when (zerop height) (incf height))
          (unless (and (eql (inner-width-of vc) width)
                       (eql (inner-height-of vc) height))
            (setf (slot-value vc 'inner-width) width
                  (slot-value vc 'inner-height) height)
            ;; TODO: Cache (hash-table?) the result here.
            ;; TODO: Use optipng (queue+background-thread?) to compress the result further.
            (unless (probe-file filename)
              (vecto:with-canvas (:width (inner-width-of vc) :height (inner-height-of vc))
                (redraw vc)
                (vecto:save-png filename)))
            (setf (src-of vc)
                  (mk-static-data-url *app* (catstr (string-downcase (filename-of vc)) "-"
                                                    width-str "x" height-str ".png")))))))))


(flet ((resize-msg (vc)
         (js-msg (id-of vc) "resize"
                 :callback-data `(("width"  . ,(format nil "return $('#~A').innerWidth();" (id-of vc)))
                                  ("height" . ,(format nil "return $('#~A').innerHeight();" (id-of vc)))))))


  (defmethod render ((vc vecto))
    #| TODO: The initial event should be instant, then we should start debouncing -- then go back to the initial
    state again where the initial one is instant. I think.. At least for quite fast connections this makes sense. |#
    #| TODO: The client should group messages from multiple VECTO instances together in a single bulk message! |#
    (run (catstr "$(window).bind('resize." (id-of vc) "', "
                 "$.debounce(" (resize-msg vc) ", 250));" +lf+) ;; TODO: Hardcoded debounce-value.
         vc))


  (defmethod render :after ((vc vecto))
    (run (catstr (resize-msg vc) "();" +lf+)
         vc)))


(defmethod container-remove :before (container (vc vecto))
  (run (catstr "$(window).unbind('resize." (id-of vc) "');" +lf+)
       vc))


(defmethod redraw ((vc vecto))
  (funcall (redraw-fn-of vc) vc))
