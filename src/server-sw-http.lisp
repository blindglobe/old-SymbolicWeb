;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :server-sw-http.lisp))


#| TODO:
This thing is a messy copy/paste job. I'm getting sleepy just thinking about
fixing this.
|#


(defclass sw-http-server (server sw-http:server)
  ()

  (:default-initargs
   ;; TODO: This is not really used as a 404 response anymore, but if this in turn fails Lighttpd will send its
   ;; 404 page.
   :404-fn
   (lambda ()
     (sw-http:response-add-chunk
      (let ((header (catstr "X-Sendfile: "
                            (static-data-fs-path-of (or *app* *server*))
                            (sw-http:path))))
        (sw-http:combine-buffers
         #.(sw-http:mk-response-status-code 200)
         (sw-http:mk-response-header-field header)
         (sw-http:mk-response-message-body
          (who (:html
                (:body
                 (:h1 "SymbolicWeb")
                 (:p (str header)))))))))

     (sw-http:done-generating-response))

   :application-finder-fn
   (lambda (sw-http-server connection)
     (sw-http-server-request-handler sw-http-server connection))))


;; TODO: Move to SW-HTTP?
(defmethod host ((server sw-http-server) &key (include-subdomain-p t))
  (let ((host (sw-http:host)))
    (if include-subdomain-p
        host
        (if (< 1 (count #\. host))
            (mk-array-view host :start (1+ (position #\. host)))
            host))))


(defmethod stop-server ((server sw-http-server) &key)
  (sw-http:stop server))


(defmethod start-server ((server sw-http-server))
  (sw-http:start-listening server))


(defn sw-http-server-request-handler (((server sw-http-server) (connection sw-http:connection)))
  ;; TODO: This spawns a thread before the 404-fn is called; so even static content will spawn a (short-lived) thread for no reason.
  (with-thread ((prin1-to-string (cons server (sw-http::cn-socket connection))))
    (flet ((body ()
             (swh:with-swh-context connection (:parse-get-parameters-p t
                                               :get-parameters-url-decode-p t
                                               :parse-post-parameters-p t
                                               :post-parameters-url-decode-p t
                                               :parse-cookies-p t)
               (setf (sw-http:close-connection-p) -server-close-connection-p-)
               (let ((*server* server)
                     (*request-time* (get-universal-time)))
                 (setf (last-ping-time-of server) *request-time*)
                 (muffle-compiler-note
                   (incf (request-counter-of server)))
                 (if-let (app (with-locked-object server
                                (if-let ((cookie-value (sw-http:get-cookie (cookie-name-of server))))
                                  (if-let ((app (gethash cookie-value (cookie-value->app-of server))))
                                    app
                                    (progn
                                      (warn "User is trying to fetch a session that no longer exists.")
                                      (session-expired-response)
                                      (invoke-restart 'skip-body)))
                                  (create-new-session server))))
                   (let ((viewport (when-let* ((viewport-id (sw-http:get-parameter "_sw_viewport-id"))
                                               (viewport (find-or-create-viewport viewport-id app)))
                                     (setf (last-ping-time-of viewport) *request-time*)
                                     (when (and +auto-set-viewport-support-p+ -auto-set-viewport-p-)
                                       (setf *viewport* viewport))
                                     viewport)))
                     (setf (last-ping-time-of app) *request-time*)
                     (when (and +auto-set-app-support-p+ -auto-set-app-p-)
                       (setf *app* app))
                     (let ((*app* app))
                       (or (handle-request server app viewport)
                           (handle-request app server viewport)
                           (progn
                             (warn "404 @ viewport ~A and app ~A for ~S" viewport app (sw-http:path))
                             ;; FIXME: Slot 404-FN is missing from VIEWPORT and APPLICATION.
                             (funcall (the function (404-fn-of (or viewport app))))))))
                   (let ((*app* nil)) ;; Dodge stale *APP* bound by -AUTO-SET-APP-P-.
                     #|(warn "Server (global) HTTP-404 for (SW-HTTP:PATH): ~S" (sw-http:path))|#
                     (funcall (the function (404-fn-of server)))))))))
      (if (and (not (debug-p-of server)) ;; We do not want timeouts and roll-back
               -sw-request-timeout-)     ;; of state while browsing a stack-trace.
          (with-timeout (-sw-request-timeout-
                         (warn "SW-HTTP-SERVER-REQUEST-HANDLER: Timeout!")
                         (invoke-restart 'skip-body))
            (body))
          (with-simple-restart (skip-body "Skip handling of HTTP request. [SW-HTTP-SERVER-REQUEST-HANDLER]")
            (body))))))



(defmethod session-expired-response ()
  (let ((request-type (sw-http:get-parameter "_sw_request-type")))
    (cond
      ((or (string= "ajax" request-type)
           (string= "comet" request-type))
       (sw-http:response-add-chunk
        #.(sw-http:combine-buffers
           (sw-http:mk-response-status-code 200)
           (sw-http:mk-response-header-field "Content-Type: text/javascript; charset=utf-8")
           (sw-http:mk-response-header-field "Connection: keep-alive")))
       (sw-http:response-add-chunk
        (sw-http:mk-response-message-body
         (catstr
           (js-code-of (set-document-cookie :name (cookie-name-of *server*)
                                            :value nil))
           (js-code-of (reload)))))
       (sw-http:done-generating-response))

      (t
       (sw-http:response-add-chunk
        #.(sw-http:combine-buffers
           (sw-http:mk-response-status-code 200)
           (sw-http:mk-response-header-field "Content-Type: text/html; charset=utf-8")
           (sw-http:mk-response-header-field "Connection: keep-alive")
           (sw-http:mk-response-header-field "Expires: Mon, 26 Jul 1997 05:00:00 GMT")
           (sw-http:mk-response-header-field "Cache-Control: no-store, no-cache, must-revalidate, post-check=0, pre-check=0")
           (sw-http:mk-response-header-field "Pragma: no-cache")
           (sw-http:mk-response-header-field "Server: SBCL (Common Lisp), SW-HTTP, SymbolicWeb -- License: AGPL -- http://nostdal.org/")))

       (sw-http:response-add-chunk
        (sw-http:mk-response-header-field (catstr "Last-Modified: " (rfc-1123-date))))

       ;; NOTE: I think this is the only way to do this proper; HTTP headers will not include the URL hashes.
       (sw-http:response-add-chunk
        (sw-http:mk-response-message-body
         (who (:html
               (:body
                (:script
                 (str (catstr
                        (js-code-of (set-document-cookie :name (cookie-name-of *server*)
                                                         :value nil))
                        (js-code-of (reload)))))
                (:noscript (:p "JavaScript needs to be enabled.")))))))
       (sw-http:done-generating-response)))))


(defmethod handle-request ((server sw-http-server)
                           (app application)
                           (viewport (eql nil)))
  (prog1 t
    (sw-http:response-add-chunk
     #.(sw-http:combine-buffers
        (sw-http:mk-response-status-code 200)
        (sw-http:mk-response-header-field "Content-Type: text/html; charset=utf-8")
        (sw-http:mk-response-header-field "Connection: keep-alive")
        (sw-http:mk-response-header-field "Expires: Mon, 26 Jul 1997 05:00:00 GMT")
        (sw-http:mk-response-header-field "Cache-Control: no-store, no-cache, must-revalidate, post-check=0, pre-check=0")
        (sw-http:mk-response-header-field "Pragma: no-cache")
        (sw-http:mk-response-header-field "Server: SBCL (Common Lisp), SW-HTTP, SymbolicWeb -- License: AGPL -- http://nostdal.org/")))
    ;; NOTE: No `Set-Cookie' header; it is set by the RENDER method using JavaScript on the client.
    (sw-http:response-add-chunk
     (sw-http:mk-response-header-field (catstr "Last-Modified: " (rfc-1123-date))))
    (sw-http:response-add-chunk
     (sw-http:mk-response-message-body
      (with-context (context-of app)
        (render app))))
    (sw-http:done-generating-response)))



(defmethod handle-request ((server sw-http-server)
                           (app application)
                           (viewport viewport))
  (prog1 t
    (let ((*viewport* viewport)
          (*request-type* (let ((request-type (sw-http:get-parameter "_sw_request-type")))
                            (cond
                              ((string= request-type "comet") :comet)
                              ((string= request-type "ajax")  :ajax)
                              (t (error "Unknown request-type: ~S" request-type))))))
      (case *request-type*
        (:comet
         (sw-http:response-add-chunk
          #.(sw-http:combine-buffers
             (sw-http:mk-response-status-code 200)
             (sw-http:mk-response-header-field "Content-Type: text/javascript; charset=utf-8")
             (sw-http:mk-response-header-field "Connection: keep-alive")))
         (handle-comet-request server app viewport))

        (:ajax
         (setf (last-user-activity-time-of viewport) *request-time*)
         (with-context (context-of app)
           (handle-ajax-request server app viewport))

         (sw-http:response-add-chunk
          #.(sw-http:combine-buffers
             (sw-http:mk-response-status-code 200)
             #|(sw-http:mk-response-header-field "Content-Type: text/javascript; charset=utf-8")|#
             (sw-http:mk-response-header-field "Content-Type: text/plain; charset=utf-8")
             (sw-http:mk-response-header-field "Connection: keep-alive")
             (sw-http:mk-response-message-body "")))
         (sw-http:done-generating-response)
         (do-comet-response viewport))))))
