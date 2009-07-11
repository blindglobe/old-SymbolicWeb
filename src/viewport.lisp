;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :viewport.lisp))


(defclass viewport (id-mixin)
  ((application :reader application-of :initarg :application
                :type application
                :initform (error ":APPLICATION must be supplied."))

   (root-widget :reader root-widget-of :initarg :root-widget
                :type (or symbol widget)
                :initform 'container)

   (widgets :reader widgets-of
            :type hash-table
            :initform (make-hash-table :test #'equal :weakness :value)
            :documentation "
A weak hash of ID->WIDGETs of all currently active widgets in viewport.")

   (callbacks :reader callbacks-of
              :type hash-table
              :initform (make-hash-table :test #'equal :weakness :value))

   (address-bar :reader address-bar-of)

   (dirty-p :reader dirty-p-of
            :initform nil)

   (do-at-end-mutex :reader do-at-end-mutex-of
                    :initform (make-lock "do-at-end-mutex"))
   (do-at-end :reader do-at-end-of
              :type list
              :initform nil
              :documentation "
List of closures to be executed at the end of _handling_ this round-trip.
Note that these are sure to be executed even if we are in a \"comet-context\"
which normally might have delayed the end (it might be sleeping).")

   (long-poll-frequency :accessor long-poll-frequency-of
                        :type fixnum
                        :initform -default-long-poll-frequency-)

   (comet-callback :accessor comet-callback-of
                   :initform nil)

   (last-ping-time :accessor last-ping-time-of
                   :type integer
                   :initform (get-universal-time))

   (last-user-activity-time :accessor last-user-activity-time-of
                            :initform (get-universal-time))

   (response-data :type (or null string) :initform nil)
   (prev-response-data :reader prev-response-data-of
                       :type (or null string)
                       :initform nil)
   (response-data-mutex :reader response-data-mutex-of
                        :initform (make-lock "response-data-mutex"))

   (code-id<->code :reader code-id<->code-of
                   :type hash-table
                   :initform (make-hash-table :test #'equal)))

  (:documentation "
Each instance of VIEWPORT represents a browser window or tab."))


(defmethod initialize-instance :after ((viewport viewport) &key)
  (with-slots (id root-widget address-bar application) viewport
    (assert (subtypep 'container root-widget)) ;; The reason for this is the (RENDER CONTAINER) :AROUND method in container.lisp. This is needed because of the (render *root*) call in comet.lisp. Update; I suppose a with-code-block here would work, but it doesn't matter much for now.
    (setf address-bar (make-instance 'address-bar :viewport viewport)
          root-widget (make-instance root-widget :id "sw-root")
          (slot-value root-widget 'visible-p) t
          (gethash id (viewports-of root-widget)) viewport
          (gethash id (viewports-of application)) viewport)))


(defmethod print-object ((viewport viewport) stream)
  (print-unreadable-object (viewport stream :type t :identity t)
    (format stream "(ID ~A)" (id-of viewport))))


#.(maybe-inline 'find-or-create-viewport)
(defun find-or-create-viewport (viewport-id app)
  "Find an existing or create a new viewport based on the viewport-id parameter sent from
the client.
Returns two values; the viewport and whether a new one was created or not."
  (declare (string viewport-id)
           (application app)
           (optimize speed))
  (with-locked-object app
    (if-let (viewport (gethash viewport-id (viewports-of app)))
      (values viewport nil)
      (values (make-instance 'viewport :id viewport-id :application app) t))))


(defmethod visible-p-of ((viewport viewport) &key)
  (> -viewport-visible-p-timeout-
     (- (get-universal-time) (last-ping-time-of viewport))))


#.(maybe-inline 'for-each-viewport-in-app)
(defun for-each-viewport-in-app (fn &optional (app *app*))
  "FN is a function that takes one argument; the viewport.
Also see FOR-EACH-VIEWPORT-OF-WIDGET."
  (declare (type application app)
           ((function (viewport)) fn)
           (optimize speed))
  (maphash (lambda (%not-used viewport)
             (declare (ignore %not-used)
                      (viewport viewport))
             (funcall fn viewport))
           (viewports-of app)))


(defmethod on-refresh ((app application) (viewport viewport))
  "Put userdefined code here that is to be called when the entire client side
widget tree is completely rendered and updated after a page load or a page
refresh."
  (declare (ignore app viewport)))


(defmethod render-viewport ((viewport viewport) (app application))
  (declare (ignore app))
  ;; TODO: This is pretty stupid; but it gets rid of the "always loading" thing in Firefox when using random subdomains and the "page" is empty on initial load.
  (run "2+2" viewport))


(defmethod render-viewport :after ((viewport viewport) (app application))
  (if-let ((widget (last-focus-of app)))
    ;; Ensure that the widget that had focus before page refresh still has it.
    (when (visible-p-of widget :viewport viewport)
      (focus widget))
    ;; No widget is assigned focus; we'll try to find a default candidate.
    (block nil
      (with-each-widget-in-tree (:root (root-widget-of viewport))
        (when (focussable-p-of widget)
          (focus widget)
          (return))))))


(defun (setf do-at-end-of) (list-of-closures viewport)
  (declare (type viewport viewport))
  (with-recursive-lock-held ((do-at-end-mutex-of viewport))
    (setf (slot-value viewport 'do-at-end) list-of-closures)))


(defun handle-do-at-end-of (viewport)
  (declare (type viewport viewport))
  (with-recursive-lock-held ((do-at-end-mutex-of viewport))
    (when (do-at-end-of viewport)
      (dolist (to-do (do-at-end-of viewport))
        (funcall to-do))
      (nilf (slot-value viewport 'do-at-end))
      (unless (eq *request-type* :comet)
        ;; TODO: At least output a warning?
        (handler-case
            (funcall (comet-callback-of viewport))
          (error (c)
            (warn "HANDLE-DO-AT-END-OF: ~A" c)))))))


(defmethod response-data-of ((viewport viewport))
  (with-recursive-lock-held ((response-data-mutex-of viewport))
    (slot-value viewport 'response-data)))


(defun do-comet-response (viewport)
  (declare (viewport viewport))
  (when-let ((comet-callback (comet-callback-of viewport)))
    (funcall (the function comet-callback))))


#.(maybe-inline 'append-to-response-data-of)
(defun append-to-response-data-of (viewport js-str)
  (declare ;;(optimize (speed 3) (space 0) (safety 0) (debug 0) (compilation-speed 0))
           (type viewport viewport)
           (type string js-str))
  (with-recursive-lock-held ((response-data-mutex-of viewport))
    (setf (slot-value viewport 'response-data)
          ;; TODO: I'm guessing using an array with a fill-pointer etc. would be faster...
          (catstr (slot-value viewport 'response-data) js-str)))

  (if (and (equal viewport *viewport*)
           (eq *request-type* :ajax))
      (with-recursive-lock-held ((do-at-end-mutex-of viewport))
        (if (dirty-p-of viewport)
            (unless (do-at-end-of viewport)
              ;; FIXME: This is a nasty thing that seems to happen once in a while; I got to figure this out proper later, but for now
              ;; this works as a workaround.
              ;;(warn "(SETF RESPONSE-DATA-OF): DIRTY-P is T and DO-AT-END slot in VIEWPORT (~A) in ~A is empty! Setting DIRTY-P to NIL and waking up comet thread."
              ;;      (id-of viewport) (id-of (application-of viewport)))
              (nilf (slot-value viewport 'dirty-p))
              (do-comet-response viewport))
            (progn
              (tf (slot-value viewport 'dirty-p))
              (push (iambda (with-recursive-lock-held ((do-at-end-mutex-of viewport)) (nilf (slot-value viewport 'dirty-p))))
                    (do-at-end-of viewport)))))

      ;; When sending to other viewports we send a direct wake up call to the comet channel.
      ;; This is less than optimal, probably, but it's "safe". This will be called
      ;; when working from the REPL too.
      (do-comet-response viewport)))


#.(maybe-inline 'root)
(defun root ()
  (root-widget-of *viewport*))


(defmethod remove-viewport ((viewport viewport) (app application))
  (remhash (id-of viewport) (viewports-of app))
  (reload viewport)
  (with-each-widget-in-tree (:root (root-widget-of viewport))
    (remove-widget-from-viewport widget viewport)))


(defmethod reset (&optional (viewport *viewport*))
  "Call this to do a hard reset of any pending JS-code about to be sent to VIEWPORT.
This is useful while creating and debugging widgets or custom JS-code, as JS-code
which causes an exception on the client might trigger a continious
re-transmission of the same invalid JS-code to the client."
  (setf (slot-value viewport 'prev-response-data) nil
        (slot-value viewport 'response-data) nil))
