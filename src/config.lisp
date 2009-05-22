;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

;; Tell the Lisp world that we exist.
(pushnew :symbolicweb *features*)


(eval-now (defvar *sw-debug* t))
(export '*sw-debug*)



;; Make random random.
(setf *random-state*
      (make-random-state t))



;;; Stuff for SERVER
;;;;;;;;;;;;;;;;;;;;


(defvar *server-close-connection-p* nil
  "
* Lighttpd-1.4.x: this must be T.
* Lighttpd-1.5.x: this can be NIL.")


(defvar *sw-comet-timeout* 30)


(defvar *server-default-type*
  'sw-http-server)


(defvar *server-default-port*
  6001)


(defvar *server-default-static-data-subdomain* nil
  "A string like `sw-static' (no slash prefix) or NIL.")


(defvar *server-default-static-path* "sw-static"
  "A string like `sw-static' (no dot) or NIL.")


(defvar *server-default-static-data-fs-path*
  (catstr (namestring (user-homedir-pathname))
          "symbolicweb-data/")
  "This should probably point to the data/ directory of the SymbolicWeb source
code root.")


(defvar *server-default-gc-frequency* (* 60     ;; Seconds.
                                         1000))



(eval-now (use-package :sw-jquery))



(defconstant +transport-client-side-exceptions-to-server-p+ nil
  "If T this will attempt to transport client side JS exceptions back to the
 server and display them there in the Lisp (slime) condition handler. This
will generate extra JS code and have some extra overhead though. Use Firebug
when you can.")
(export '+transport-client-side-exceptions-to-server-p+)


(defconstant +add-newlines-to-js-code-p+ t)
(export '+add-newlines-to-js-code-p+)


(defparameter *timeout* 10
  "Time before a non-async call to RUN times out.")
(export '*timeout*)


(define-constant +global-object-access-p+ t
  :documentation "The GET-OBJ function can be used to access any object in SW when this is T.")


(define-constant +auto-set-viewport-support-p+ *sw-debug*)
(defparameter *auto-set-viewport-p* t) (export '*auto-set-viewport*)
(define-constant +auto-set-app-support-p+ *sw-debug*)
(defparameter *auto-set-app-p* t) (export '*auto-set-app*)



;;; GC stuff
;;;;;;;;;;;;

;; This must be less than *SERVER-DEFAULT-GC-FREQUENCY*.
(defparameter *default-long-poll-frequency* (/ *server-default-gc-frequency* 1000
                                               2))
(export '*default-long-poll-frequency*)

;; This must be larger than *DEFAULT-LONG-POLL-FREQUENCY*
(defparameter *app-visible-p-timeout* (+ 10 *default-long-poll-frequency*)) 
(export '*app-visible-p-timeout*)

(defparameter *viewport-visible-p-timeout* *app-visible-p-timeout*)
(export '*viewport-visible-p-timeout*)


