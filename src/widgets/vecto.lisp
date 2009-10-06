;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :widgets/container.lisp)) ;; TODO: change this.


(defclass vecto (container)
  ((inner-width :reader inner-width-of
                :type (or fixnum null)
                :initform nil)

   (inner-height :reader inner-height-of
                 :type (or fixnum null)
                 :initform nil)

   (image :reader image-of
          :initform (make-instance 'image))

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
  (declare (optimize speed))
  (insert (with1 (image-of vc)
            #| This'll cause the image to strech in realtime on the client-end before the debouncer times out
               causing the server to update things proper. |#
            (setf (position-of it) "absolute"
                  (width-of it) "100%"
                  (height-of it) "100%"
                  (z-index-of it) "-1"))
          :in vc)
  (with (make-instance 'callback-box :widget vc :id "resize")
    (with-formula vc
      (when-let (event ~(event-cell-of it))
        #|(dbg-prin1 event (id-of vc))|#
        (let* ((width-str (cdr (find "width" event :key #'car :test #'string=)))
               (width (parse-integer width-str))
               (height-str (cdr (find "height" event :key #'car :test #'string=)))
               (height (parse-integer height-str)))
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
            (let ((filename (catstr (static-data-fs-path-of *app*)
                                    (string-downcase (filename-of vc)) "-"
                                    width-str "x" height-str ".png")))
              (unless (probe-file filename)
                (vecto:with-canvas (:width (inner-width-of vc) :height (inner-height-of vc))
                  (redraw vc)
                  (vecto:save-png filename))))
            #| NOTE: Not using (background-image-of vc) here because this won't resize the image in real-time on the
            client end. |#
            (setf (src-of (image-of vc))
                  (mk-static-data-url *app* (catstr (string-downcase (filename-of vc)) "-"
                                                    width-str "x" height-str ".png")))))))))


(flet ((resize-msg (vc)
         (js-msg (id-of vc) "resize"
                 :callback-data `(("width"  . ,(format nil "return $('#~A').innerWidth();" (id-of vc)))
                                  ("height" . ,(format nil "return $('#~A').innerHeight();" (id-of vc)))))))


  (defmethod render :after ((vc vecto))
    (run (catstr "$(window).bind('resize." (id-of vc) "', "
                 "$.debounce(" (resize-msg vc) ", 250));" +lf+) ;; TODO: Hardcoded debounce-value.
         vc)
    (run (catstr (resize-msg vc) "();" +lf+)
         vc)))


(defmethod container-remove :before (container (vc vecto))
  (run (catstr "$(window).unbind('resize." (id-of vc) "');" +lf+)
       vc))


(defmethod redraw ((vc vecto))
  (funcall (redraw-fn-of vc) vc))


#| TODO: This could be optimized a great deal; defaulting to 0.1 wrt. the INCF and DECF calls is not ideal. |#
(defun calc-font-size (font string width height)
  "Returns three values:
  * font-size: something that'll fit inside the bounds requested by WIDTH and HEIGHT.
  * x-adjust and y-adjust: meant to be passed to VECTO:TRANSLATE."
  (declare (string string)
           (fixnum width height)
           (optimize speed))
  (let* ((string-vector (coerce string 'simple-vector))
         (font-size (coerce height 'double-float))
         (last-good-size font-size)
         (xmin 0d0) (ymin 0d0) (xmax 0d0) (ymax 0d0)
         (direction nil))
    (declare ((member nil :up :down) direction)
             (double-float font-size last-good-size xmin ymin xmax ymax))
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


(defun vecto-simple-draw-string (string font max-width max-height &optional centered-p)
  (declare (string string))
  (vecto:with-graphics-state
    (multiple-value-bind (font-size x-adjust y-adjust)
        (calc-font-size font string max-width max-height)
      (vecto:translate x-adjust y-adjust)
      (vecto:set-font font font-size)
      (if centered-p
          (vecto:draw-centered-string (/ max-width 2)
                                      (- (/ max-height 2)
                                         (/ font-size 2))
                                      string)
          (vecto:draw-string 0 0 string)))))