;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :code.lisp))


(defclass code ()
  ((code-id :reader code-id-of :initarg :code-id
            :type string
            :initform (princ-to-string (generate-id)))

   (code :initarg :code
         :type string
         :initform (error "Initarg :CODE not supplied."))

   (status :accessor status-of :initarg :status
           :initform nil)

   (return-value :accessor return-value-of
                 :initform nil)

   (exception-str :accessor exception-str-of
                  :initform nil)

   (sleeper :reader sleeper-of
            :initform (make-instance 'sleeper))))


(defmethod code-of ((code code) &optional %not-used)
  (declare (ignore %not-used))
  (slot-value code 'code))

(defmethod code-of ((code-id string) &optional (from-viewport *viewport*))
  (gethash code-id (code-id<->code-of from-viewport)))


#.(maybe-inline 'run-js)
(defun run-js (js-str viewport)
  "Send JS-STR (JavaScript code) to VIEWPORT for execution. You should probably use
RUN instead."
  (declare (string js-str)
           (viewport viewport)
           (optimize (speed 3) (space 0) (safety 0) (debug 0) (compilation-speed 0)))
  (append-to-response-data-of viewport js-str)
  js-str)


#.(maybe-inline 'run)
(defun run (code-str viewport-or-widget &key
            (async-p t)
            #|(timeout -timeout-)|#)
  (declare (string code-str)
           ((or viewport widget) viewport-or-widget)
           #|(fixnum timeout)|#
           (optimize speed))
  "Send JavaScript code to client(s) for execution.

CODE-STR: The JavaScript code.

VIEWPORT-OR-WIDGET: If supplied with a widget this will transmit the JS code to all
                contexts (browser sessions (users), tabs and windows) where that
                widget is visible; see WITH-VISIBLE-CONTEXTS-OF.
                If supplied with a viewport this will transmit the JS code to that
                single viewport for execution there.

ASYNC-P: If NIL, RUN will block until the client is done executing and RUN will
         return what the JS code returned. Example:
           (run \"return 2 + 2;\" *viewport* :async-p nil) => \"4\"

TIMEOUT: Used when ASYNC-P is NIL. This specifies the number of seconds to wait
         until the client is done executing the JS code."
  (unless async-p (error "ASYNC-P was NIL.."))
  (when (string= code-str "")
    (warn "RUN: (STRING= CODE-STR \"\") => T. Returning from RUN with no effect.")
    (return-from run))
  (flet ((js-code (code-id)
           (declare (string code-id)
                    (optimize speed))
           (if +transport-client-side-exceptions-to-server-p+
               ;; FIXME: swRun overlaps with swReturnValue below in function etc. ...
               (catstr "swRun(\"" code-id "\""
                       (if async-p ", 1" ", 0")
                       ", function(){" code-str "});"
                       (if +add-newlines-to-js-code-p+ +newline+ ""))
               (if +add-newlines-to-js-code-p+
                   (catstr code-str +newline+)
                   code-str))))
    (declare (inline js-code))
    (if *creating-code-block-p*
        (if async-p
            (push (js-code "0") *code-block*)
            #|(error "RUN: Currently creating code block (WITH-CODE-BLOCK) and RUN was called
with :ASYNC-P set to NIL.")|#)
        (if async-p
            ;; Going to run in "async mode."
            (if (typep viewport-or-widget 'viewport)
                (run-js (js-code "0") viewport-or-widget)
                (with-visible-contexts-of viewport-or-widget viewport
                  (run-js (js-code "0") viewport)))
            ;; Going to run in "sync mode" and/or we're (probably) expecting
            ;; to return something from the client (viewport).
            #|(if (typep viewport-or-widget 'viewport)
                (let ((code-obj (make-instance 'code :code code-str)))
                  (setf (gethash (code-id-of code-obj) (code-id<->code-of viewport-or-widget))
                        code-obj)
                  (unwind-protect
                       (retryable
                         (run-js (catstr "swReturnValue(\"" (code-id-of code-obj) "\""
                                         ", function(){" code-str "});"
                                         (if +add-newlines-to-js-code-p+ +newline+ ""))
                                 viewport-or-widget)
                         (with-timeout (timeout (format t "Timeout while trying to run:~% ( ~A )~%" code-str))
                           (go-to-sleep (sleeper-of code-obj))
                           (if (eq :failed (status-of code-obj))
                               (error (fmtn "[SW] Client side (~A) exception: ~A~%~A"
                                            (id-of *app*)
                                            (exception-str-of code-obj)
                                            (code-of code-obj)))
                               (values (return-value-of code-obj)
                                       code-obj))))
                    (remhash (code-id-of code-obj) (code-id<->code-of viewport-or-widget))))
                (error "RUN: :ASYNC-P is NIL and VIEWPORT-OR-WIDGET is not an instance of VIEWPORT."))|#))))


(defmacro with-code-block ((&key (execute-p t execute-p-supplied-p)
                                 (widget '*root* widget-supplied-p)
                                 (viewport nil viewport-supplied-p))
                           &body body)
  "Collect all JS code that would normally have been executed one \"instruction\"
at a time at the client via multiple calls to RUN in a \"code block\" and execute
it or send it to the client in one go on return of WITH-CODE-BLOCK if :EXECUTE-P
hasn't been given a NIL value.
If :EXECUTE-P is NIL a list of JS code \"snippets\" is returned which one can
store for execution later.
NOTE: Code executed within this dynamic scope is only sent to the visible
contexts of the single widget or viewport as specified by the :WIDGET or :VIEWPORT keyargs
\(this applies to the default value of :WIDGET also). This can lead to subtle
bugs in a shared widget scenario, but is very useful in some contexts."
  (when (and execute-p-supplied-p execute-p)
    (error ":EXECUTE-P should not be set to T."))
  (when (and widget-supplied-p viewport-supplied-p)
    (error ":VIEWPORT and :WIDGET should not be supplied at the same time."))
  `(if *creating-code-block-p*
       ;; Already creating a code block, so just let code in BODY fall through.
       (progn ,@body)
       ;; Setup context for creation of new code block, and start creating it.
       (let ((*creating-code-block-p* t)
             (*code-block* nil))
         ,@body
         ;; Done creating code block; might execute it now.
         (let ((*creating-code-block-p* nil))
           (when *code-block*
             ,(if execute-p
                  `(let ((code (locally (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
                                 (apply #'concatenate 'string (nreverse *code-block*)))))
                     (run code ,(if viewport-supplied-p viewport widget) :async-p t))
                  `(setf *code-block* (nreverse *code-block*))))))))


(defun client-side-event-exception (exception-str callback)
  (declare (ignore callback))
  (format t "[SW] Warning: Client JS exception: ~A~%"
          exception-str))
