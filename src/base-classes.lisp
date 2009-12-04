;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :base-classes.lisp))


#| These are placed here so the compiler will know about them as early as
possible and be able to optimize type-checking code based on this. |#



;;; widget-base.lisp
;;;;;;;;;;;;;;;;;;;;

(defclass widget-base (id-mixin self-ref view-base)
  ((callbacks :reader callbacks-of
              :type hash-table
              :initform (make-hash-table :test #'equal :synchronized t)
              :documentation "Strong hash table; [ID -> CALLBACK-BOX]")

   (parent :reader parent-of
           :type (or widget-base null)
           :initform nil)

   (in-dom-p :accessor in-dom-p-of
             :type (member nil t)
             :initform nil)

   (delayed-operations :accessor delayed-operations-of
                       :type list
                       :initform nil
                       :documentation "List of closures executed as IN-DOM-P transits from NIL to T (below).")))


(defun ensure-in-client-dom (widget)
  (declare (widget-base widget))
  (with-each-widget-in-tree (:root widget)
    (unless (in-dom-p-of widget)
      (tf (in-dom-p-of widget))
      (dolist (operation (nreverse (delayed-operations-of widget)))
        (funcall (cdr operation)))
      (nilf (delayed-operations-of widget))
      ;; Doing a "back-flip" here to ensure that the finalization-closure doesn't refer to WIDGET-BASE (which would
      ;; inhibit GC of, well, WIDGET-BASE).
      (let ((id (id-of widget))
            (viewport (viewport-of widget)))
        (check-type id string) (check-type viewport viewport)
        (sb-ext:finalize widget
                         (lambda ()
                           (run (js-remove id) viewport)))))))


(defun add-delayed-operation (widget type key operation)
  (declare (widget-base widget)
           (symbol type)
           (string key)
           (function operation)
           (optimize speed))
  (let* ((signature (cons type key))
         (existing-entry (find signature (truly-the list (delayed-operations-of widget))
                               :key #'car :test #'equal)))
    (if existing-entry
        (setf (cdr existing-entry) operation) ;; Overwrite existing operation based on SIGNATURE being equal.
        (push (cons signature operation)
              (delayed-operations-of widget)))))




;;; container-base.lisp
;;;;;;;;;;;;;;;;;;;;;;;

(defclass container-base ()
  ((children :reader children-of
             :type list
             :initform nil
             :documentation "Contains View instances. Note that this is also the only hard link (GC) to the View part of a Model <-> View relationship.")))
