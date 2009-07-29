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
(defun run (code-str target &key except-viewport server-only-p)
  (declare (string code-str)
           ((or viewport widget) target)
           ((or viewport null) except-viewport)
           ((member t nil) server-only-p))
  "Send JavaScript code to client(s) for execution.

CODE-STR: The JavaScript code.

TARGET: If supplied with a WIDGET this will transmit the JS code to
all contexts (browser sessions (users), tabs and windows) where that widget is
visible. If supplied with a VIEWPORT this will transmit the JS code to that
single viewport for execution there.

:EXCEPT-VIEWPORT: This assumes the TARGET parameter was given a WIDGET and does
what you think it does (see doc. for the TARGET parameter)."
  (when (string= code-str "")
    (warn "RUN: (STRING= CODE-STR \"\") => T. Returning from RUN with no effect.")
    (return-from run))
  (when server-only-p
    (return-from run))
  (flet ((js-code ()
           (muffle-compiler-note
             (if +add-newlines-to-js-code-p+
                 (catstr code-str +newline+)
                 code-str))))
    (declare (inline js-code))
    (if (typep target 'viewport)
        (run-js (js-code) target)
        (when-let (viewport (viewport-of target))
          (run-js (js-code) viewport)))))
