;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :code.lisp))


#.(maybe-inline 'run)
(eval-now
  (proclaim '(ftype (function (string (or viewport widget) &key (:server-only-p (member t nil)) &allow-other-keys)
                              (values &optional))
                    run)))
(defun run (code-str target &key server-only-p &allow-other-keys)
  "Send JavaScript code to client(s) for execution.

CODE-STR: The JavaScript code.

TARGET: If supplied with a WIDGET this will transmit the JS code to
all contexts (browser sessions (users), tabs and windows) where that widget is
visible. If supplied with a VIEWPORT this will transmit the JS code to that
single viewport for execution there."
  (cond
    (server-only-p)

    ((zerop (length code-str))
     (warn "SW:RUN: Returning from RUN with no effect; no code in CODE-STR."))

    (t
     (etypecase target
       (viewport
        (append-to-response-data-of target code-str))

       (widget
        (when-let (viewport (viewport-of target))
          (append-to-response-data-of viewport code-str))))))
  (values))
