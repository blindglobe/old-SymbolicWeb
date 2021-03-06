;;;; http://nostdal.org ;;;;

(in-package #:sw)

(declaim #.(optimizations :comet.lisp))


(defmethod handle-comet-request ((server sw-http-server) (app application) (viewport viewport))
  (with-recursive-lock-held ((response-stream-mutex-of viewport))
    (when-let (comet-callback (comet-callback-of viewport))
      (warn "comet.lisp: Old Comet callback for session ~A (~A) is still around. I'll kill it." app viewport)
      (funcall (the function comet-callback) nil)
      (nilf (comet-callback-of viewport)))

    (when-let (do (sw-http:get-parameter "do"))
      (cond
        ((string= do "refresh")
         ;; TODO: Think about the WITH-SYNC here a bit.
         (with-context (context-of app)
           (unless (initialized-p-of app)
             (when-commit () ;; TODO: Using a REF for INITIALIZED-P would make sense.
               (tf (slot-value app 'initialized-p)))
             (main app))
           (render-viewport viewport app)
           #|(sync-widgets (sw-http:get-parameter "hash") t)|#
           (on-refresh app viewport)))

        #|((string= do "ack")
        (nilf (slot-value viewport 'prev-response-data)))|#))

    (flet ((do-comet-response ()
             (with-recursive-lock-held ((response-stream-mutex-of viewport))
               (append-to-response-data-of viewport "sw_comet_response = true;")
               (sw-http:response-add-chunk
                (sw-http:mk-response-message-body
                 (get-output-stream-string (response-stream-of viewport))))
               (tf (response-stream-emptyp-of viewport)))
             (handler-case (sw-http:done-generating-response)
               (t (c)
                 (warn "SW:HANDLE-COMET-REQUEST: ~A" c)))))

      (if (not (response-stream-emptyp-of viewport))
          (do-comet-response)
          (setf (comet-callback-of viewport)
                (mk-delay-callback -sw-comet-timeout-
                                   (let ((connection sw-http::*connection*))
                                     (lambda ()
                                       (with-timeout (10) ;; TODO: Ho, hum. Output a warning, at least, on timeout?
                                         (let ((sw-http::*connection* connection))
                                           (nilf (comet-callback-of viewport))
                                           (do-comet-response)))))))))))
