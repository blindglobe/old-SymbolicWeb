;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


;; Tell the Lisp world that we exist.
(pushnew :symbolicweb *features*)


#| TODO: Think about this; this is only used by code below in this file. |#
(eval-now (defvar *sw-debug* t))
(export '*sw-debug*)


;; Make random random.
(setf *random-state*
      (make-random-state t))


;;(setf who:*attribute-quote-char* #\')
(setf ps:*js-string-delimiter* #\") ;; CL-WHO defaults to #\' and Parenscript does too.



;;; Stuff for SERVER
;;;;;;;;;;;;;;;;;;;;


(define-variable -server-close-connection-p-
    :value nil
    :kind :global
    :doc "
* Lighttpd-1.4.x: this must be T.
* Lighttpd-1.5.x: this can be NIL.")


(define-variable -sw-comet-timeout-
    :value 30
    :kind :global)


(define-variable -server-default-type-
    :value 'sw-http-server
    :kind :global)


(define-variable -server-default-port-
    :value 6001
    :kind :global)


(define-variable -server-default-static-data-subdomain-
    :value nil
    :kind :global
    :doc "A string like `sw-static' (no slash prefix) or NIL.")


(define-variable -server-default-static-path-
    :value "sw-static"
    :kind :global
    :doc "A string like `sw-static' (no dot) or NIL.")


(define-variable -server-default-static-data-fs-path-
    :value (catstr (namestring (user-homedir-pathname))
                   "symbolicweb-data/")
    :kind :global
    :doc "This should probably point to the data/ directory of the SymbolicWeb source
code root.")


(define-variable -server-default-gc-frequency-
    :value (* 60 #|seconds|# 1000)
    :kind :global)


(eval-now (use-package :sw-jquery))


(define-variable +add-newlines-to-js-code-p+
    :value t
    :kind :constant)
(export '+add-newlines-to-js-code-p+)


(define-variable -timeout-
    :value 10
    :kind :global
    :doc "Time before a non-async call to RUN times out.")


(define-variable +global-object-access-p+
    :value t
    :kind :constant
    :doc "The GET-OBJ function can be used to access any object in SW when this is T.")


(define-variable +auto-set-viewport-support-p+ :value *sw-debug* :kind :constant)
(define-variable -auto-set-viewport-p- :value t :kind :global)
(export '-auto-set-viewport-)

(define-variable +auto-set-app-support-p+ :value *sw-debug* :kind :constant)
(define-variable -auto-set-app-p- :value t :kind :global)
(export '-auto-set-app-)



;;; GC stuff
;;;;;;;;;;;;

;; This must be less than -SERVER-DEFAULT-GC-FREQUENCY-.
(define-variable -default-long-poll-frequency-
    :value (/ -server-default-gc-frequency- 1000
              2)
    :kind :global)
(export '-default-long-poll-frequency-)

;; This must be larger than *DEFAULT-LONG-POLL-FREQUENCY*
(define-variable -app-visible-p-timeout-
    :value (+ 10 -default-long-poll-frequency-)
    :kind :global)
(export '-app-visible-p-timeout-)


(define-variable -viewport-visible-p-timeout-
    :value -app-visible-p-timeout-
    :kind :global)
(export '-viewport-visible-p-timeout-)
