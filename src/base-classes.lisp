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

   ;; TODO: It does seem like this is a fairly important thing so perhaps the (SETF ATTRIBUTE) etc. functions should
   ;; promote :LISP-NAME to a primary argument?
   (delayed-operations :accessor delayed-operations-of
                       :type list
                       :initform nil
                       :documentation "List of closures executed as IN-DOM-P transits from NIL to T (below).")))


(defun ensure-in-client-dom (widget)
  (declare (widget-base widget))
  (with-each-widget-in-tree (:root widget)
    (unless (in-dom-p-of widget)
      (tf (in-dom-p-of widget))
      (dolist (operation (reverse (delayed-operations-of widget)))
        (funcall (cdr operation)))
      (nilf (delayed-operations-of widget))
      ;; Doing a "back-flip" here to ensure that the finalization-closure doesn't refer to WIDGET-BASE (which would
      ;; inhibit GC of, well, WIDGET-BASE).
      (let ((id (id-of widget))
            (viewport (viewport-of widget)))
        (check-type id string) (check-type viewport viewport)
        (sb-ext:finalize widget
                         (lambda ()
                           (run (fmtn "(function(){ var gc_fn = $('#~A').data('gc-fn');
                                       if(gc_fn) gc_fn(); else $('#~A').remove();  })();~%" id id)
                                viewport)))))))


(defun add-delayed-operation (widget lisp-accessor-name operation)
  "OPERATION is a closure to be executed as the IN-DOM-P slot of WIDGET transits from NIL to T."
  (declare (widget-base widget)
           (symbol lisp-accessor-name)
           (function operation)
           (optimize speed))
  (let ((existing-entry (assoc lisp-accessor-name (truly-the list (delayed-operations-of widget)) :test #'eq)))
    (if existing-entry
        (setf (cdr existing-entry) operation) ;; Overwrite existing operation based on LISP-ACCESSOR-NAME being EQ.
        (push (cons lisp-accessor-name operation)
              (delayed-operations-of widget)))))




;;; container-base.lisp
;;;;;;;;;;;;;;;;;;;;;;;

(defclass container-base ()
  ((children :reader children-of
             :type list
             :initform nil
             :documentation "Contains View instances. Note that this is also the only hard link (GC) to the View part of a Model <-> View relationship.")))
