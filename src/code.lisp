;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :code.lisp))


#.(maybe-inline 'run-js)
(defun run-js (js-str viewport)
  "Send JS-STR (JavaScript code) to VIEWPORT for execution. You should probably use
RUN instead."
  (declare (string js-str)
           (viewport viewport))
  (append-to-response-data-of viewport js-str)
  js-str)


#.(maybe-inline 'run)
(defun run (code-str viewport-or-widget &key)
  (declare (string code-str)
           ((or viewport widget) viewport-or-widget))
  "Send JavaScript code to client(s) for execution.

CODE-STR: The JavaScript code.

VIEWPORT-OR-WIDGET: If supplied with a WIDGET this will transmit the JS code to all
                contexts (browser sessions (users), tabs and windows) where that
                widget is visible; see WITH-VISIBLE-CONTEXTS-OF.
                If supplied with a VIEWPORT this will transmit the JS code to that
                single viewport for execution there."
  (when (string= code-str "")
    (warn "RUN: (STRING= CODE-STR \"\") => T. Returning from RUN with no effect.")
    (return-from run))
  (flet ((js-code ()
           (if +add-newlines-to-js-code-p+
               (catstr code-str +newline+)
               code-str)))
    (declare (inline js-code))
    (if *creating-code-block-p*
        (push (js-code) *code-block*)
        (if (typep viewport-or-widget 'viewport)
            (run-js (js-code) viewport-or-widget)
            (with-visible-contexts-of viewport-or-widget viewport
              (run-js (js-code) viewport))))))


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
                     (run code ,(if viewport-supplied-p viewport widget)))
                  `(nreversef *code-block*)))))))
