;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :ajax.lisp))


(defmethod on-invalid-callback-id ((server server) (app application) (viewport viewport))
  (warn "SW:ON-INVALID-CALLBACK-ID: Reloading ~S" viewport)
  (reload viewport))


(defmethod handle-ajax-request ((server sw-http-server) (app application) (viewport viewport))
  (let ((event (sw-http:get-parameter "_sw_event")))
    (cond
      ((string= "dom-event" event)
       (let* ((widget-id (sw-http:get-parameter "_sw_widget-id"))
              (callback-id (sw-http:get-parameter "_sw_callback-id"))
              (callback-box (or (find-callback-box widget-id callback-id app)
                                (progn
                                  (warn "[SW] HANDLE-AJAX-REQUEST: No callback-box with ID ~S found in  ~S."
                                        callback-id viewport)
                                  (on-invalid-callback-id server app viewport)
                                  (return-from handle-ajax-request))))
              (arguments (ecase (sw-http:http-method)
                           (:get (remove-if (lambda (str) (char= #\_ (char str 0)))
                                            sw-http::*get-parameters*
                                            :key #'car))
                           (:post sw-http::*post-parameters*))))
         (setf (last-user-activity-time-of app) (get-universal-time))
         (execute-callback callback-box arguments)))

      ((string= "url-hash-changed" event)
       (if-let (new-url-hash (sw-http:post-parameter "new-url-hash"))
         ;; SYNC-WIDGETS confirms that the widgets mentioned in the hash is a
         ;; part of the users session (by ways of the WIDGETS slot in APPLICATION).
         ;; So this is safe; no cross-session manipulation is possible.
         ;;(sync-widgets new-url-hash nil)
         t ;;(dbg-princ new-url-hash "TODO: ajax.lisp/url-hash-changed")
         (warn "[SW]: Got an URL-HASH-CHANGED message but no NEW-URL-HASH argument.")))

      ((string= "terminate-session" event)
       (remove-application app))

      #| TODO: Add this back later.
      ((string= "display-session-info" event)
       (show-alert-box (who (:h3 "SymbolicWeb session info")
                            "(ID-OF APP) => " (str (id-of app)) :br
                            "(ID-OF VIEWPORT) => " (str (id-of viewport)) :br
                            "(COOKIE-VALUE-OF APP) => " (str (cookie-value-of app)))))
      |#

      (t
       (warn "Unknown event `~A' supplied to `handle-ajax-request'." event)))))
