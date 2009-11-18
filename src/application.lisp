;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)
(declaim #.(optimizations :application.lisp))


(defclass application (id-mixin locked-object self-ref)
  ((server :reader server-of
           :type server)

   (break-http-connection-limit-p :accessor break-http-connection-limit-p-of
                                  :initarg :break-http-connection-limit-p
                                  :type (member nil t)
                                  ;; TODO/NOTE: Opera does not handle random subdomains well.
                                  :initform (when -break-http-connection-limit-p-
                                              (if (eq :presto (sw-http:browser-type))
                                                  nil
                                                  t)))

   (last-focus :reader last-focus-of
               :type (or null string widget)
               :initform nil)

   (cookie-value :reader cookie-value-of :initarg :cookie-value
                 :type string
                 :initform (error ":COOKIE-VALUE must be supplied."))

   (cookie-expires :reader cookie-expires-of
                   :type integer)

   (static-data-subdomain :accessor static-data-subdomain-of
                          :type (or string null))

   (static-data-fs-path :accessor static-data-fs-path-of
                        :type string)

   (initialized-p :reader initialized-p-of
                  :type (member nil t)
                  :initform nil
                  :documentation "
Has the MAIN method been executed?")

   (widgets :reader widgets-of
            :type hash-table
            :initform (make-hash-table :test #'equal :weakness :value)
            :documentation "
This contains all widgets currently or recently active or visible in a session.
It is not 100% accurate; it is a weak hash, and some of these widgets might not
have been GCed yet even though they are not active or visible anymore.")

   (viewports :reader viewports-of
              :type hash-table
              :initform (make-hash-table :test #'equal)
              :documentation "
This hash-table contains instances of VIEWPORT; server side representations of
browser windows or tabs.")

   (last-ping-time :accessor last-ping-time-of
                   :type integer
                   :initform (get-universal-time))

   (last-activity-time :accessor last-activity-time-of
                       :documentation "
Last time we had any real user _or_ server (server push) activity in the session."

                       :initform (get-universal-time))
   (last-user-activity-time :accessor last-user-activity-time-of
                            :initform (get-universal-time)
                            :documentation "
Last time we had any real user (DOM event or page refresh) activity in the session.")

   (http-meta-author :accessor http-meta-author-of
                     :initform "SymbolicWeb: http://nostdal.org/")

   (resources :reader resources-of
              :type hash-table
              :initform (make-hash-table :test #'equal))))
(export '(application))


(defmethod initialize-instance :around ((app application) &key
                                        (server (error ":SERVER needed."))
                                        (static-data-subdomain (static-data-subdomain-of server))
                                        (static-data-fs-path (static-data-fs-path-of server))
                                        (cookie-expires (cookie-expires-of server)))
  (let ((*app* app))
    (setf (slot-value app 'server) server
          (slot-value app 'static-data-subdomain) static-data-subdomain
          (slot-value app 'static-data-fs-path) static-data-fs-path
          (slot-value app 'cookie-expires) cookie-expires)
    (call-next-method)))


(defmethod initialize-instance :after ((app application) &key)
  (on-session-start app))


(defmethod generate-dynamic-subdomain ((app application))
  (declare (optimize speed (safety 1)))
  (if (break-http-connection-limit-p-of app)
      (catstr "sw.dyn-" (id-generator-next-str -id-generator-))
      nil))


(defmethod visible-p-of ((app application) &key)
  (> -app-visible-p-timeout-
     (- (get-universal-time) (last-ping-time-of app))))


(defmethod on-session-start ((app application))
  "Called before *ROOT* has been instantiated."
  (format t "session started: ~A (id: ~A)~%" app (id-of app)))


(defmethod on-session-end ((app application))
  (format t "session end: ~A (id: ~A)~%" app (id-of app)))


(defmethod on-session-timeout ((app application))
  "Called by HT."
  (format t "session timeout: ~A (id: ~A)~%" app (id-of app))
  (on-session-end app))


(defmacro defapp (name parents slots &rest options)
  `(progn
     (defclass ,name ,(or parents '(application))
       (,@slots)
       ,@options)))


(defmethod main ((app application))
  (declare (ignore app)))
(export 'main)


(defmethod render ((app application))
  "Defines the static skeleton HTML for an application. If you define your own
RENDER method you most likely want to include a 'sw-root' DIV element, and also
include the JS libraries required for SW in general."
  (declare (optimize speed))
  ;; :PROLOGUE set to T generates this (needed to trigger "standards mode" in IE7!):
  ;; <!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
  (with-html-output-to-string (ss nil :prologue t)
    (:html :xmlns "http://www.w3.org/1999/xhtml" :|xml:lang| "en" :lang "en"
     (:head
      (:title (str (string-capitalize (type-of app))))
      (:meta :name "Author" :content (http-meta-author-of app))
      ;; Do want; http://blog.chromium.org/2009/09/introducing-google-chrome-frame.html
      (:meta :http-equiv "X-UA-Compatible" :content "chrome=1")
      (:meta :http-equiv "X-UA-Compatible" :content "IE=edge") ;; For IE8 and up.
      (:meta :http-equiv "Content-Type" :content "text/html; charset=UTF-8")

      ;; TODO: Move this to a slot in APPLICATION.
      (:style :type "text/css"
       "html, body, #sw-root {"
       "  position: absolute;"
       "  width: 100%; height: 100%;"
       "  overflow: hidden;"
       "  margin: 0; padding: 0; border: 0;"
       "}"
       ".sw-hide {"
       "  display: none !important;"
       "}")

      ;; User-specified static CSS files.
      (maphash (lambda (signature url)
                 (when (eq :css (cdr signature))
                   (htm (:link :rel "stylesheet" :type "text/css" :href url))))
               (resources-of app)))

     (:body
      (:div :id "sw-root")
      (:div
       (:img :id "sw-loading-spinner" :alt ""
             :style "position: absolute; z-index: 1000; right: 0px; top: 0px;"
             :src (mk-static-data-url (server-of app) "gfx/sw-loader.gif"))

        (:a :accesskey 1 :href "javascript:swTerminateSession();")
        #|(:a :accesskey 2 :href "javascript:swDisplaySessionInfo();")|#

        (:noscript
         (:h2 "JavaScript needs to be enabled.")
         (:p (:a :href (sw-http:path)
                 #| TODO/NOTE: Something like this can be used to log users that do not have JS enabled. Send the
                 request to SW instead of Lighttpd. |#
                 (:img :src "gfx/guru-meditation.gif" :alt "Amiga 500"))))

        (str (js-sw-headers app)))))))


(defmethod remove-application ((app application) &optional (server *server*))
  (check-type server server)
  (with-each-viewport-in-app (:app app)
    (remove-viewport viewport app))
  (remhash (id-of app) (id->app-of server))
  (remhash (cookie-value-of app) (cookie-value->app-of server)))


(defun add-resource (app id type url)
  (declare (application app)
           (string id)
           ((member :css :js) type)
           (string url))
  (setf (gethash (cons id type) (resources-of app))
        url))
(export 'add-resource)
