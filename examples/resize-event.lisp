;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass resize-event-app (application)
  ())

(set-uri 'resize-event-app "/resize-event-app")


(defmethod render-viewport ((viewport viewport) (app resize-event-app))
  (let ((font (zpb-ttf:open-font-loader "/usr/share/fonts/truetype/msttcorefonts/times.ttf")))
    ;; TODO: Move the event/binding stuff here to the DOM API.
    (with (make-instance 'callback-box :widget viewport :id "resize")
      (with-formula viewport
        (when-let (event ~(event-cell-of it))
          #|(dbg-prin1 event)|#
          ;; TODO: Handle (not (and (plusp width) (plusp height)))
          (let* ((width-str (cdr (find "width" event :key #'car :test #'string=)))
                 (width (parse-integer width-str))
                 (height-str (cdr (find "height" event :key #'car :test #'string=)))
                 (height (parse-integer height-str))
                 (filename (catstr (static-data-fs-path-of *server*)
                                   "vecto-test-" width-str "x" height-str ".png")))
            #| TODO: Think about this a bit; are these values reasonable? This should be configurable.
            They are probably way too big for when I start using this stuff for actual widgets.
            Also, the filenames are not in sync after I mutate these. |#
            (when (< 3000 width) (setf width 3000))
            (when (< 3000 height) (setf height 3000))
            (when (zerop width) (incf width))
            (when (zerop height) (incf height))
            #|(format t "~%(width ~A) (height ~A)~%" width height)|#
            ;; TODO: Cache (hash-table) the result here.
            ;; TODO: Use optipng (queue+background-thread?) to compress the result further.
            (unless (probe-file filename)
              (vecto:with-canvas (:width width :height height)
                (vecto:with-graphics-state
                  (let* ((font-size height)
                         (last-good-size font-size)
                         (xmin) (ymin) (xmax) (ymax)
                         (direction nil))
                    (declare ((member nil :up :down) direction))
                    (flet ((calc-bounds ()
                             (let ((bounds (vecto:string-bounding-box (coerce "Hello!" 'simple-vector)
                                                                      font-size font)))
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
                    #|(format t "~%(width ~A) (height ~A)~%" width height)|#
                    #|(format t "(xmin ~A) (xmax ~A) (ymin ~A) (ymax ~A)~%" xmin xmax ymin ymax)|#
                    #|(format t "(x ~A) (y ~A)~%" (+ xmax (- xmin)) (+ ymax (- ymin)))|#
                    #|(format t "(font-size ~A)~%" font-size)|#
                    (vecto:set-font font font-size)
                    #|(vecto:translate (+ (- xmin) (/ width 2)) (- ymin))|#
                    #|(vecto:draw-centered-string 0 0 (coerce "Hello!" 'simple-vector))|#
                    (vecto:translate (- xmin) #|(floor (- xmin))|#
                                     (- ymin) #|(ceiling (- ymin))|#)
                    (vecto:draw-string 0 0 (coerce "Hello!" 'simple-vector))
                    ))
                (vecto:set-rgba-stroke 0 1.0 0 1.0)
                (vecto:set-line-width 1)
                (vecto:rectangle 0 0 width height)
                (vecto:stroke)
                (vecto:save-png filename)))
            (setf (src-of (child-of (root)))
                  (mk-static-data-url app (catstr "vecto-test-" width-str "x" height-str ".png"))))))))

  ;; TODO: Move this to the DOM API.
  (run (catstr "$(window).resize("
               ;; You see; this is why JavaScript is nice.
               ;; TODO: Don't debounce on initial load (pass 'true' to the ASAP arg. of debounce, I think).
               "$.debounce("
               (js-msg (id-of viewport) "resize"
                       :callback-data `(("width"  . "return $('#sw-root').innerWidth();")
                                        ("height" . "return $('#sw-root').innerHeight();")))
               ", 250)"
               ");")
       viewport)

  (setf (position-of (root)) "absolute"
        (width-of (root)) "50%"
        (left-of (root)) "25%"
        (height-of (root)) "50%"
        (top-of (root)) "25%"
        (background-color-of (root)) "red")

  (insert (mk-image "") :in (root))

  ;; Boot things up. TODO: Move this.
  (run (catstr "$(window).resize();" +lf+) viewport))
