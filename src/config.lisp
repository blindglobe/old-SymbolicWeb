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


(define-global -server-close-connection-p- nil
  "
* Lighttpd-1.4.x: this must be T.
* Lighttpd-1.5.x: this can be NIL.")


(define-global -sw-comet-timeout- 30)


(define-global -server-default-type- 'sw-http-server)


(define-global -server-default-port- 6001)


(define-global -server-default-static-data-subdomain- nil
  "A string like `sw-static' (no slash prefix) or NIL.")


(define-global -server-default-static-path- "sw-static"
  "A string like `sw-static' (no dot) or NIL.")


(define-global -server-default-static-data-fs-path-
    (catstr (namestring (user-homedir-pathname))
            "symbolicweb-data/")
  "This should probably point to the data/ directory of the SymbolicWeb source
code root.")


(define-global -server-default-gc-frequency- (* 60     ;; Seconds.
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


(define-global -timeout- 10
  "Time before a non-async call to RUN times out.")


(define-constant +global-object-access-p+ t
  :documentation "The GET-OBJ function can be used to access any object in SW when this is T.")


(define-constant +auto-set-viewport-support-p+ *sw-debug*)
(define-global -auto-set-viewport-p- t) (export '-auto-set-viewport-)
(define-constant +auto-set-app-support-p+ *sw-debug*)
(define-global -auto-set-app-p- t) (export '-auto-set-app-)



;;; GC stuff
;;;;;;;;;;;;

;; This must be less than -SERVER-DEFAULT-GC-FREQUENCY-.
(define-global -default-long-poll-frequency- (/ -server-default-gc-frequency- 1000
                                                2))
(export '-default-long-poll-frequency-)

;; This must be larger than *DEFAULT-LONG-POLL-FREQUENCY*
(define-global -app-visible-p-timeout- (+ 10 -default-long-poll-frequency-))
(export '-app-visible-p-timeout-)


(define-global -viewport-visible-p-timeout- -app-visible-p-timeout-)
(export '-viewport-visible-p-timeout-)
