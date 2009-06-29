;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/html-container.lisp))


(defclass html-container (container)
  ((html-content :reader html-content-of
                 :type string))

  (:default-initargs
   :model nil))
(export 'html-container)


(defmethod initialize-instance :after ((html-container html-container) &key
                                       (html-content (error ":HTML-CONTENT needed.")))
  (check-type html-content function)
  (setf (html-content-of html-container)
        html-content))


(defmethod (setf html-content-of) ((html-content function) (html-container html-container))
  (setf (slot-value html-container 'html-content)
        (generate-html html-container html-content)))


(defmethod generate-html :around ((html-container html-container) closure)
  (let ((*creating-html-container-p* html-container)
        (*html-container-children* nil))
    (prog1 (call-next-method)
      (when-let ((additional-children (set-difference *html-container-children* (children-of html-container)
                                                      :test #'eq)))
        (add-to* html-container (reverse additional-children)))
      (when-let ((removed-children (set-difference (children-of html-container) *html-container-children*
                                                   :test #'eq)))
        (dolist (child removed-children)
          (remove child html-container))))))
(export 'generate-html)


(defmethod generate-html ((html-container html-container) closure)
  (funcall closure))
(export 'generate-html)


(defmethod update-client ((html-container html-container))
  (run (setf (js-html-of (id-of html-container))
             (html-content-of html-container))
       html-container)
  (dolist (child (children-of html-container))
    (render child)))
(export 'update-client)


(defmethod render ((html-container html-container))
  (update-client html-container))
(export 'render)


(defmacro mk-html (args &body html)
  (setf args (mklst args))
  (assert (sequence-of-length-p html 1) nil
          "MK-HTML: Need an \"outer\" HTML element wrapper like (:div ..),
\(:span ..) or (:p ..) etc.")
  (let ((res (cadr html)))
    (assert (and (or (listp res) (member res who:*html-empty-tags*))
                 (not (eq :sw (first (first html)))))
            nil
            "MK-HTML: The \"outer\" HTML element cannot have attributes and cannot be (:SW ..).
Use the functions in src/widgets/attributes.lisp and css.lisp instead.
Saw: ~S" res))
  (let ((html (first html)))
    ;;(assert (not (eq :sw (first html))))
    `(make-instance 'html-container
                    ,@args
                    :element-type ,(string-downcase (first html))
                    :html-content (lambda () (who ,@(rest html))))))
(export 'mk-html)