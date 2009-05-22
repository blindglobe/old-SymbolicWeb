;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations))


;; TODO: This thing doesn't actually work yet ..
(defmethod on-invalid-callback-id ((server server) (app application) (viewport viewport))
  (reload viewport))


(defmethod handle-ajax-request ((server sw-http-server) (app application) (viewport viewport))
  (let ((event (sw-http:get-parameter "event")))
    (cond
      ((string= "dom-event" event)
       (let* ((callback-id (sw-http:get-parameter "callback-id"))
              (callback-box (or (find-callback-box callback-id viewport)
                               (progn
                                 (warn "[SW] HANDLE-AJAX-REQUEST: No callback-box with ID ~S found in viewport ~S." callback-id (id-of viewport))
                                 (on-invalid-callback-id server app viewport)
                                 (return-from handle-ajax-request))))
              (arguments sw-http::*post-parameters*))
         (setf (last-user-activity-time-of app) (get-universal-time))
         (dolist (arg arguments)
           (setf (car arg) (mksymf ":" (car arg))))
         (execute-callback callback-box :dom-event (flatten arguments))))

      ((string= "js-ack" event)
       (let* ((code-id (sw-http:get-parameter "code-id"))
              (code (code-of code-id)))
         (if code
             (progn
               (setf (return-value-of code) (sw-http:post-parameter "return-value")
                     (status-of code) :success)
               (wake-up (sleeper-of code)))
             (format t "[SW] (js-ack) Warning: code-id \"~A\" not found in app \"~A\"~%"
                     code-id (id-of app)))))
      
      ((string= "js-fail" event)
       (let ((code (code-of (sw-http:get-parameter "code-id"))))
         (if code
             (progn
               (setf (exception-str-of code) (sw-http:post-parameter "exception-str")
                     (status-of code) :failed)
               (wake-up (sleeper-of code)))
             (format t "[SW] (js-fail) Warning: Client JS exception: ~A~%"
                     (sw-http:post-parameter "exception-str")))))

      ((string= "url-hash-changed" event)
       (if-let (new-url-hash (sw-http:post-parameter "new-url-hash"))
         ;; SYNC-WIDGETS confirms that the widgets mentioned in the hash is a
         ;; part of the users session (by ways of the WIDGETS slot in APPLICATION).
         ;; So this is safe; no cross-session manipulation is possible.
         (sync-widgets new-url-hash nil)
         (warn "[SW]: Got an URL-HASH-CHANGED message but no NEW-URL-HASH argument.")))

      ;; FIXME: Test this.
      ((string= "event-exception" event)
       (let* ((exception-str (url-decode (sw-http:post-parameter "exception-str")))
              (callback-id (url-decode (sw-http:get-parameter "callback-id"))))
         ;; TODO: should add a way for the user to catch the exception higher in the call stack
         (error "A DOM event exception occurred for callback-id ~A~%~A"
                callback-id
                exception-str)))

      ((string= "terminate-session" event)
       (remove app))

      ((string= "display-session-info" event)
       (show-alert-box (who (:h3 "SymbolicWeb session info")
                            "(ID-OF APP) => " (str (id-of app)) :br
                            "(ID-OF VIEWPORT) => " (str (id-of viewport)) :br
                            "(COOKIE-VALUE-OF APP) => " (str (cookie-value-of app)))))
      
      (t
       (warn "Unknown event `~A' supplied to `handle-ajax-request'." event)))))


