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

   (address-bar :reader address-bar-of)

   (long-poll-frequency :accessor long-poll-frequency-of
                        :type fixnum
                        :initform -default-long-poll-frequency-
                        :documentation "
Comet timeout or poll frequency in seconds.")

   (comet-callback :accessor comet-callback-of
                   :type (or null function)
                   :initform nil)

   (response-stream-mutex :reader response-stream-mutex-of
                          :type mutex
                          :initform (make-lock))

   (response-stream :reader response-stream-of
                    :type stream
                    :initform (make-string-output-stream))

   (response-stream-emptyp :accessor response-stream-emptyp-of
                           :type (member nil t)
                           :initform t)

   (last-ping-time :accessor last-ping-time-of
                   :type integer
                   :initform (get-universal-time))

   (last-user-activity-time :accessor last-user-activity-time-of
                            :type integer
                            :initform (get-universal-time)))

  (:documentation "
Each instance of VIEWPORT represents a browser window or tab."))


(defmethod initialize-instance :after ((viewport viewport) &key (id (error ":ID needed.")))
  (declare (ignore id))
  (with-slots (id root-widget address-bar application) viewport
    (assert (subtypep 'container root-widget)) ;; The reason for this is the (RENDER CONTAINER) :AROUND method in container.lisp. This is needed because of the (render *root*) call in comet.lisp. Update; I suppose a with-code-block here would work, but it doesn't matter much for now.
    (setf address-bar (make-instance 'address-bar :viewport viewport)
          root-widget (make-instance root-widget :id "sw-root")
          (viewport-of root-widget) viewport
          (gethash id (viewports-of application)) viewport)))


(defmethod print-object ((viewport viewport) stream)
  (print-unreadable-object (viewport stream :type t :identity t)
    (format stream "(ID ~A)" (id-of viewport))))


#.(maybe-inline 'find-or-create-viewport)
(defn find-or-create-viewport (((viewport-id string) (app application)))
  "Find an existing or create a new viewport based on the VIEWPORT-ID parameter
 sent from the client.
Returns two values; a VIEWPORT instance and whether a new one was created or not."
  (declare (optimize speed))
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
  (declare (application app)
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
  ;; TODO: This is pretty stupid, but it gets rid of the "always loading" thing in Firefox when using random
  ;; subdomains and the "page" is empty on initial load.
  (run "2+2;" viewport))


(defmethod render-viewport :after ((viewport viewport) (app application))
  (if-let ((widget (with (last-focus-of app)
                     (withp (etypecase it
                              (widget it)
                              (string (gethash it (widgets-of app)))
                              (null nil))
                       (visible-p-of it :viewport viewport)))))
    ;; Ensure that the widget that had focus before page refresh still has it.
    (focus widget)
    ;; No widget is assigned focus; we'll try to find a default candidate.
    (block nil
      (with-each-widget-in-tree (:root (root-widget-of viewport))
        (when (focussable-p-of widget)
          (focus widget)
          (return))))))


(defn do-comet-response (null ((viewport viewport)))
  (declare (optimize speed (safety 2)))
  (with-recursive-lock-held ((response-stream-mutex-of viewport))
    (unless (response-stream-emptyp-of viewport)
      (when-let ((comet-callback (comet-callback-of viewport)))
        (funcall (truly-the function comet-callback)))))
  (values))


#.(maybe-inline 'append-to-response-data-of)
(defn append-to-response-data-of (null ((viewport viewport) (js-str string)))
  (declare (optimize speed (safety 2)))
  (with-recursive-lock-held ((response-stream-mutex-of viewport))
    (write-string js-str (response-stream-of viewport))
    (nilf (response-stream-emptyp-of viewport))

    (when (and (not *bulk-update-p*)
               (or (eq *request-type* :unknown)     ;; From the REPL?
                   (not (eq *viewport* viewport)))) ;; Cross-viewport?
      ;; ..then send stuff right away.
      (do-comet-response viewport)))
  (values))


#.(maybe-inline 'root)
(defun root ()
  (root-widget-of *viewport*))


(defmethod remove-viewport ((viewport viewport) (app application))
  (remhash (id-of viewport) (viewports-of app))
  (reload viewport)
  (with-each-widget-in-tree (:root (root-widget-of viewport))
    (remove-widget-from-viewport widget viewport)))
