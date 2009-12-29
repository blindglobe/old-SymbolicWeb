;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :widgets/html-container.lisp))


(defclass html-container (widget container-base)
  ((html-content :accessor html-content-of
                 :type string)

   (closure :type function)))


(defmethod initialize-instance :after ((html-container html-container) &key
                                       (html-content nil html-content-supplied-p))
  (if html-content-supplied-p
      (with-object html-container
        (setf ¤closure html-content)
        (generate-html html-container))
      ;; We assume the user has defined a custom GENERATE-HTML method.
      (generate-html html-container)))


(defmethod set-model nconc ((html-container html-container) (model model))
  ;; This widget/View doesn't care about Models or the MVC thing.
  )


(flet ((update-client (html-container)
         (declare (html-container html-container))
         (run (setf (js-html-of (id-of html-container)) (html-content-of html-container))
              html-container)
         (dolist (child (children-of html-container))
           (render child)))

       (generate-html-wrapper (html-container fn)
         (declare (html-container html-container)
                  (function fn)
                  (optimize (safety 2)))
         (let ((*creating-html-container-p* html-container)
               (*html-container-children* nil))
           (prog1 (setf (slot-value html-container 'html-content) (funcall fn))
             (when-let ((additional-children (set-difference *html-container-children* (children-of html-container)
                                                             :test #'eq)))
               (appendf (slot-value html-container 'children)
                        (reverse additional-children))
               (dolist (child additional-children)
                 (assert (not (parent-of child)))
                 (setf (slot-value child 'parent) html-container))
               (when (visible-p-of html-container)
                 (dolist (child additional-children)
                   (propagate-for-add child html-container))))

             (when-let ((removed-children (set-difference (children-of html-container) *html-container-children*
                                                          :test #'eq)))
               (dolist (child removed-children)
                 (deletef (slot-value html-container 'children) child))
               (when (visible-p-of html-container)
                 (dolist (child removed-children)
                   (propagate-for-remove child))))))))


  (defmethod render ((html-container html-container))
    (update-client html-container))


  (defmethod generate-html :around ((html-container html-container))
    (generate-html-wrapper html-container (lambda () (call-next-method))))


  (defmethod (setf html-content-of) ((html-content function) (html-container html-container))
    (prog1 html-content
      (with-object html-container
        (setf ¤closure html-content)
        ;; We assure that no user-defined GENERATE-HTML method is called.
        (generate-html-wrapper html-container html-content))))


  (defmethod (setf html-content-of) :after (html-content (html-container html-container))
    (update-client html-container)))


(defmethod generate-html ((html-container html-container))
  (with-object html-container
    (funcall (truly-the function ¤closure))))



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
