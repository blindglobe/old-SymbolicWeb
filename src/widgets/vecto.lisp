;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/container.lisp)) ;; TODO: change this.


(defclass vecto (image)
  ((inner-width :reader inner-width-of
                :initform nil)

   (inner-height :reader inner-height-of
                 :initform nil)

   (filename :accessor filename-of
             :initarg :filename
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


(defun calc-font-size (font string width height)
  "Returns three values:
  * font-size: something that'll fit inside the bounds requested by WIDTH and HEIGHT.
  * x-adjust and y-adjust: meant to be passed to VECTO:TRANSLATE."
  (declare (string string)
           (real width height))
  (let* ((string-vector (coerce string 'simple-vector))
         (font-size height)
         (last-good-size font-size)
         (xmin) (ymin) (xmax) (ymax)
         (direction nil))
    (declare ((member nil :up :down) direction))
    (flet ((calc-bounds ()
             (let ((bounds (vecto:string-bounding-box string-vector font-size font)))
               (setf xmin (svref bounds 0)
                     ymin (svref bounds 1)
                     xmax (svref bounds 2)
                     ymax (svref bounds 3)))))
      (loop
         (calc-bounds)
         #|(format t "~A ~A ~A ~A~%" xmin ymin xmax ymax)|#
         (let ((too-big-p (or (> (+ xmax (- xmin)) #|(ceiling (+ xmax (- xmin)))|#
                                 width)
                              (> (+ ymax (- ymin)) #|(ceiling (+ ymax (- ymin)))|#
                                 height))))
           (unless direction
             ;; TODO: Third state needed for when initial state is ok.
             (if too-big-p
                 (setf direction :down)
                 (setf direction :up)))
           (ecase direction
             (:up (if too-big-p
                      (progn
                        (setf font-size last-good-size)
                        (calc-bounds)
                        (return))
                      (setf last-good-size (incf font-size 0.1))))
             (:down (if too-big-p
                        (setf last-good-size (decf font-size 0.1))
                        (progn
                          (setf font-size last-good-size)
                          (calc-bounds)
                          (return))))))))
    (values font-size (- xmin) (- ymin))))


(defun vecto-simple-draw-string (string font max-width max-height)
  (declare (string string))
  (vecto:with-graphics-state
    (multiple-value-bind (font-size x-adjust y-adjust)
        (calc-font-size font string max-width max-height)
      (vecto:translate x-adjust y-adjust)
      (vecto:set-font font font-size)
      (vecto:draw-string 0 0 string))))