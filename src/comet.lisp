;;;; http://nostdal.org ;;;;

(in-package #:sw)

(declaim #.(optimizations :comet.lisp))


(defmethod handle-comet-request ((server sw-http-server) (app application) (viewport viewport))
  (when-let (comet-callback (comet-callback-of viewport))
    (warn "comet.lisp: Old Comet callback for session ~A (viewport ~A) is still around. I'll kill it." app viewport)
    (funcall (the function comet-callback) nil)
    (nilf (comet-callback-of viewport)))

  (when-let (do (sw-http:get-parameter "do"))
    (with-lock-held ((response-data-mutex-of viewport))
      (cond
        ((string= do "refresh")
         (let ((*replace-address-bar-p* t))
           (with-each-widget-in-tree (:root *root*)
             (setf (gethash (id-of viewport) (viewports-of widget)) viewport))

           (nilf (slot-value viewport 'response-data)
                 (slot-value viewport 'prev-response-data))

           (unless (initialized-p-of app)
             (tf (slot-value app 'initialized-p))
             (main app))

           (with-code-block (:viewport viewport)
             (render-viewport viewport app)
             (sync-widgets (sw-http:get-parameter "hash") t))
           (on-refresh app viewport)))

        ((string= do "ack")
         (nilf (slot-value viewport 'prev-response-data))))))

  ;; FIXME: This probably doesn't belong here, and it requires a lock of the viewport when it has been moved.
  (handle-do-at-end-of viewport)


  (flet ((do-comet-response (response-data-p)
           (sw-http:response-add-chunk
            (sw-http:mk-response-message-body
             (catstr (if response-data-p
                         (with-lock-held ((response-data-mutex-of viewport))
                           (let ((response-data (slot-value viewport 'response-data)))
                             (setf (slot-value viewport 'prev-response-data)
                                   (prog1 response-data
                                     (nilf (slot-value viewport 'response-data))))))
                         "")
                     "sw_comet_response = true;")))
           (handler-case (sw-http:done-generating-response)
             (t (c)
               ;;(declare (ignore c))
               (warn "SW:HANDLE-COMET-REQUEST: ~A" c)))))

    (when-let (prev-response-data (prev-response-data-of viewport))
      (with-lock-held ((response-data-mutex-of viewport))
        (append-to-response-data-of viewport prev-response-data)
        (nilf (slot-value viewport 'prev-response-data))))

    (if (response-data-of viewport)
        (do-comet-response (response-data-of viewport))
        (setf (comet-callback-of viewport)
              (mk-delay-callback -sw-comet-timeout-
                                 (let ((connection sw-http::*connection*))
                                   (lambda ()
                                     ;; TODO: Timeout should be dealt with somehow here.
                                     (with-timeout (10)
                                       (let ((sw-http::*connection* connection))
                                         (nilf (comet-callback-of viewport))
                                         (do-comet-response (response-data-of viewport)))))))))))
