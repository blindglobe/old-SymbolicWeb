;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :widgets/html-container.lisp))


(defclass html-container (container)
  ((html-content :accessor html-content-of
                 :type string)

   (closure :type function))

  (:default-initargs
   :model nil))
(export 'html-container)


(defmethod initialize-instance :after ((html-container html-container) &key
                                       (html-content nil html-content-supplied-p))
  (if html-content-supplied-p
      (with-object html-container
        (setf ¤closure html-content)
        (generate-html html-container))
      ;; We assume the user has defined a custom GENERATE-HTML method.
      (generate-html html-container)))


(defmethod update-client ((html-container html-container))
  (run (setf (js-html-of (id-of html-container))
             (html-content-of html-container))
       html-container)
  (dolist (child (children-of html-container))
    (render child)))
(export 'update-client)


(defmethod render ((html-container html-container))
  (update-client html-container))


(flet ((generate-html-wrapper (html-container fn)
         (declare (html-container html-container)
                  (function fn))
         (let ((*creating-html-container-p* html-container)
               (*html-container-children* nil))
           (prog1 (setf (slot-value html-container 'html-content) (funcall fn))
             (when-let ((additional-children (set-difference *html-container-children* (children-of html-container)
                                                             :test #'eq)))
               (add-to* html-container (reverse additional-children)))
             (when-let ((removed-children (set-difference (children-of html-container) *html-container-children*
                                                          :test #'eq)))
               (dolist (child removed-children)
                 (remove child html-container)))))))
  (declare (inline generate-html-wrapper))


  (defmethod generate-html :around ((html-container html-container))
    (generate-html-wrapper html-container (lambda () (call-next-method))))


  (defmethod (setf html-content-of) ((html-content function) (html-container html-container))
    (prog1 html-content
      (with-object html-container
        (setf ¤closure html-content)
        ;; We assure that no user-defined GENERATE-HTML method is called.
        (generate-html-wrapper html-container html-content)))))


(defmethod (setf html-content-of) :after (html-content (html-container html-container))
  (update-client html-container))


(defmethod generate-html ((html-container html-container))
  (with-object html-container
    (funcall (truly-the function ¤closure))))
(export 'generate-html)


(defmethod (setf model-of) (model (html-container html-container))
  (error "HTML-CONTAINER is not meant to have a back end Model."))



;; TODO: This stuff is probably no good. "MK-HTML" is a poor name. etc...

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
                    ;; We pass a closure here so the :AROUND GENERATE-HTML method can capture or detect child
                    ;; widgets referred to in the WHO form.
                    :html-content (lambda () (who ,@(rest html))))))
(export 'mk-html)