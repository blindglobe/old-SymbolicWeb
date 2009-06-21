;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :layout.lisp))


(defwidget layout-container (container)
  ()
  (:default-initargs
      :css-class "sw_base"))


(defclass temp-layout-container ()
  ((element-name :reader element-name :initarg :element-name
                 :initform (error ":ELEMENT-NAME needed."))
   
   (left  :initarg :left
          :initform (error ":LEFT needed."))
   (width :initarg :width
          :initform (error ":WIDTH needed."))
   (width-determined-p :initform nil)
   
   (top    :initarg :top
           :initform (error ":TOP needed."))
   (height :initarg :height
           :initform (error ":HEIGHT needed."))
   (height-determined-p :reader height-determined-p
                        :initform nil)))


(defun generate-layout-containers (layout-data &key
                                   (resolution-x (length (first layout-data)))
                                   (resolution-y (length layout-data)))
  (let ((layout-containers nil)
        (point-width  (/ 100 resolution-x))
        (point-height (/ 100 resolution-y)))
    (loop :for y-pos :from 0 :below (length layout-data)
          :for horizontal-line :in layout-data
       :do (let ((new-lcs-for-this-line nil)
                 (lcs-seen-for-this-line nil))
             (loop :for x-pos :from 0 :below (length horizontal-line)
                   :for element-name :in horizontal-line
                :do (if-let ((layout-container (find element-name layout-containers :key #'element-name)))
                      (with-slots (width width-determined-p height height-determined-p) layout-container
                        (push layout-container lcs-seen-for-this-line)
                        (unless width-determined-p
                          (incf width point-width)))
                      (let ((layout-container (make-instance 'temp-layout-container
                                                             :element-name element-name
                                                             :left (* x-pos point-width)  :width  point-width
                                                             :top  (* y-pos point-height) :height point-height)))
                        (push layout-container layout-containers)
                        (push layout-container lcs-seen-for-this-line)
                        (push layout-container new-lcs-for-this-line))))
             ;; We know the width already.
             (dolist (layout-container new-lcs-for-this-line)
               (with-slots (width-determined-p) layout-container
                 (setf width-determined-p t)))
             ;; We'll know the height when a previously seen layout-container isn't mentioned anymore.
             (dolist (layout-container (remove-if #'height-determined-p layout-containers))
               (with-slots (height height-determined-p) layout-container
                 (if (find layout-container lcs-seen-for-this-line)
                     (unless (= (1+ y-pos) resolution-y) ;; Not for the last line.
                       (incf height point-height))
                     (setf height-determined-p t
                           height (- height point-height)))))))
    (values (loop :for layout-container :in (reverse layout-containers)
              :collect (with-slots (left width top height) layout-container
                         ;; TODO: Having to do stuff like this sucks; a % type for CSS, in general, is needed.
                         (make-instance 'layout-container
                                        :left (format nil "~F%" left) :width  (format nil "~F%" width)
                                        :top  (format nil "~F%" top)  :height (format nil "~F%" height))))
           (reverse layout-containers))))
  


(defmacro with-layout ((&key (resolution-x 10) (resolution-y 10) bindings)
                       layout
                       &body body)
  (let ((layout-containers (generate-layout-containers layout
                                                       :resolution-x resolution-x
                                                       :resolution-y resolution-y)))
    `(let (,@(loop :for layout-container :in layout-containers
                :collect (with-slots (element-name left width top height) layout-container
                           `(,(cadr (find element-name bindings :key #'first))
                              (make-instance 'layout-container
                                             :left ,left :width  ,width
                                             :top  ,top  :height ,height)))))
       ,@body)))











(defun test ()
  (let ((layout-data `((a a b c)
                       (a a e e)
                       (g h i i))))
    (multiple-value-bind (lcs tlcs)
        (generate-layout-containers layout-data)
      (with-each-viewport-in-server ()
        (remove-all (root))
        (loop :for lc :in lcs
           :for tlc :in tlcs
           :do (add-to (root)
                       (mk-div (string (element-name tlc))
                               :background-color "gray" :border "1px solid black"
                               :position "absolute"
                               :left (left-of lc) :width (width-of lc)
                               :top  (top-of lc)  :height (height-of lc))))))))

(test)

(defmethod render-viewport ((viewport viewport) (app empty-page-app))
  (test))


