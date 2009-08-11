;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :server.lisp))


(defclass server (id-mixin)
  ((debug-p :accessor debug-p-of :initarg :debug-p
            :initform t)

   (path->app-class :reader path->app-class-of
                    :type hash-table
                    :initform (make-hash-table :test #'equal))

   (static-data-path :accessor static-data-path-of :initarg :static-data-path
                     :type (or string null)
                     :initform -server-default-static-path-
                     :documentation "
A string like `sw-static' (no slash prefix) or NIL.")

   (static-data-subdomain :accessor static-data-subdomain-of :initarg :static-data-subdomain
                          :type (or string null)
                          :initform -server-default-static-data-subdomain-
                          :documentation "
A string like `sw-static' (no dot) or NIL.")

   (static-data-fs-path :accessor static-data-fs-path-of :initarg :static-data-fs-path
                        :type string
                        :initform -server-default-static-data-fs-path-)

   (last-ping-time :accessor last-ping-time-of
                   :type (or integer null)
                   :initform nil)

   (gc-thread :reader gc-thread-of
              :initform nil)

   (gc-frequency :accessor gc-frequency-of :initarg :gc-frequency
                 :type fixnum
                 :initform -server-default-gc-frequency-)

   (id->app :reader id->app-of
            :type hash-table
            :initform (make-hash-table :test #'equal)
            :documentation "
Instances of APPLICATION (sessions). This is the only place where a \"hard
link\" to these instances are stored (wrt. GC).")

   (cookie-name :reader cookie-name-of :initarg :cookie-name
                :type string
                :initform "symbolicweb")

   (cookie-value->app :reader cookie-value->app-of
                      :type hash-table
                      :initform (make-hash-table :test #'equal :weakness :value)
                      :documentation "
A \"hard link\" to APPLICATION instances is stored in the ID->APP slot.")

   (request-counter :accessor request-counter-of
                    :type integer
                    :initform 0)

   ;; TODO: This stuff sucks.
   (404-fn :accessor 404-fn-of :initarg :404-fn
           :initform (lambda () "HTTP 404")))

  (:default-initargs
   :port -server-default-port-))


(defmethod gc-viewports ((server server))
  (let ((*server* server))
    (maphash (lambda (%not-used app)
               (declare (ignore %not-used))
               (if (visible-p-of app)
                   (with-each-viewport-in-app (:app app)
                     (unless (visible-p-of viewport)
                       (remove-viewport viewport app)))
                   (remove-application app server)))
             (id->app-of server))))


(defmethod sw-http:maybe-debug ((server server) condition)
  (if (debug-p-of server)
      (invoke-debugger condition)
      (progn
        (warn "~S got condition: ~S~%Set DEBUG-P slot to T to debug in Lisp/Slime."
              server condition)
        (invoke-restart (or (find-restart 'sw-mvc:assign-condition)
                            #|(find-restart 'sw-mvc:user-feedback)|# ;; Try letting the user deal with the problem.
                            (find-restart 'sw-stm:abort-transaction)
                            (find-restart 'sw-http:continue-listening))))))


(defmethod cookie-expires-of ((server server))
  (+ (get-universal-time)
     (* 60 60 24 360)))


(defmethod start-server :around ((server server))
  (prog1 (call-next-method)
    (setf (slot-value server 'gc-thread)
          (with-thread ((fmtn "~A (GC-THREAD)" server))
            (handler-bind ((error (lambda (c)
                                    (warn "SW:START-SERVER: Condition, ~A, in GC-loop for server ~A" c server)
                                    (sw-http:maybe-debug server c))))
              (loop
                 (with-simple-restart (continue "SW:START-SERVER: Continue GC-loop for server ~A~%" server)
                   (with-time-limiter (gc-frequency-of server)
                     (gc-viewports server)))))))
    (unless *server*
      (setf *server* server))))


(defmethod stop-server :around ((server server) &key remove-sessions-p)
  (ignore-errors (destroy-thread (gc-thread-of server))) ;; TODO: This is probably not such a good idea.
  (prog1 (call-next-method)
    (ignore-errors (destroy-thread (gc-thread-of server)))
    (when remove-sessions-p
      (clrhash (id->app-of server))
      (nilf *app* *viewport*))
    (when (equal *server* server)
      (setf *server* nil))))


(let ((server-instance nil)
      (lock (make-lock)))

  (defun start-sw (&rest initargs &key
                   (server nil server-supplied-p)
                   (server-type -server-default-type-)
                   &allow-other-keys)
    (with-lock-held (lock)
      (when server-instance
        (error "Server already running."))
      (setf server-instance
            (if server-supplied-p
                server
                (apply #'make-instance server-type
                       (remove-from-plist initargs :server :server-type)))))
    (start-server server-instance)
    server-instance)


  (defun stop-sw (&key remove-sessions-p)
    (with-lock-held (lock)
      (unless server-instance
        (error "Server not running."))
      (stop-server server-instance :remove-sessions-p remove-sessions-p)
      (prog1 server-instance
        (setf server-instance nil))))


  (defun get-sw ()
    (with-lock-held (lock)
      server-instance)))


(defun set-uri (app-class-sym path &optional (server *server* server-supplied-p))
  (declare (symbol app-class-sym)
           (string path))
  (if (typep server 'server)
      (setf (gethash path (path->app-class-of server)) app-class-sym)
      (if server-supplied-p
          (error "This does not make sense:~%(SET-URI ~A ~S ~A)" app-class-sym path server)
          (warn "SW: (SET-URI ~A ~S) had no effect because *SERVER* was NIL." app-class-sym path))))


(defmethod remove-application ((app (eql t)) &optional (server *server*))
  "Removes all APPLICATION instances in SERVER."
  (check-type server server)
  (maphash (lambda (id app)
             (declare (ignore id))
             (remove app))
           (id->app-of server)))


(defmethod create-new-session ((server server))
  (when-let (app-class (gethash (sw-http:path) (path->app-class-of server)))
    (let* ((cookie-value (generate-random-cookie-value server))
           (app (make-instance app-class
                               :cookie-value cookie-value
                               :server server)))
      (setf (gethash (id-of app) (id->app-of server)) app
            (gethash cookie-value (cookie-value->app-of server)) app)
      app)))
