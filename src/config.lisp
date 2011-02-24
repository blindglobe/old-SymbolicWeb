;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)


;; Tell the Lisp world that we exist.
(pushnew :symbolicweb *features*)


#| TODO: Think about this; this is only used by code below in this file. |#
(define-variable *sw-debug* :value t)


;; Make random random.
(setf *random-state*
      (make-random-state t))


(define-variable -break-http-connection-limit-p-
    :value t
    :type (member t nil))



;;; Stuff for SERVER
;;;;;;;;;;;;;;;;;;;;


(define-variable -server-close-connection-p-
    :value nil
    :type (member t nil)
    :doc "
* Lighttpd-1.4.x: this must be T.
* Lighttpd-1.5.x: this can be NIL.")


(define-variable -sw-request-timeout-
    :value 10
    :type (or integer null))


(define-variable -sw-comet-timeout-
    :value 30
    :type fixnum)


(define-variable -server-default-type-
    :value 'sw-http-server
    :type symbol)


(define-variable -server-default-port-
    :value 6001
    :type fixnum)


(define-variable -server-default-static-data-subdomain-
    :value "static"
    :type (or null string))


(define-variable -server-default-static-path-
    :value "sw-http-root"
    :type (or string null))


(define-variable -server-default-static-data-fs-path-
    :value (namestring (asdf:system-relative-pathname (asdf:find-system 'symbolicweb) "data/sw-http-root/"))
    :type string
    :doc "This should probably point to the data/ directory of the SymbolicWeb source
code root.")


(define-variable -server-default-gc-frequency-
    :value (* 60 #|seconds|# 1000)
    :type fixnum)


(eval-now (use-package :sw-jquery))


(define-variable -timeout-
    :value 10
    :type fixnum
    :doc "Time before a non-async call to RUN times out.")


(define-variable +global-object-access-p+
    :value t
    :type (member nil t)
    :doc "The GET-OBJ function can be used to access any object in SW when this is T.")


(define-variable +auto-set-viewport-support-p+
    :value *sw-debug*
    :type (member t nil))
(define-variable -auto-set-viewport-p-
    :value t
    :type (member t nil))

(define-variable +auto-set-app-support-p+
    :value *sw-debug*
    :type (member t nil))
(define-variable -auto-set-app-p-
    :value t
    :type (member t nil))



;;; GC stuff
;;;;;;;;;;;;

;; This must be less than -SERVER-DEFAULT-GC-FREQUENCY-.
(define-variable -default-long-poll-frequency-
    :value (/ -server-default-gc-frequency- 1000
              2)
    :type fixnum)

;; This must be larger than -DEFAULT-LONG-POLL-FREQUENCY-
(define-variable -app-visible-p-timeout-
    :value (+ 10 -default-long-poll-frequency-)
    :type fixnum)


(define-variable -viewport-visible-p-timeout-
    :value -app-visible-p-timeout-
    :type fixnum)
