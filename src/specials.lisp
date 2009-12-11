;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)


(define-variable *path->app-class*
    :value (make-hash-table :test #'equal)
    :type hash-table)


(define-variable *request-time*
    :value nil
    :type (or null unsigned-byte))

;; TODO: Is this really useful? It seems be bound in server-sw-http.lisp then only used in viewport.lisp.
(define-variable *request-type*
    :value :unknown
    :type (member :ajax :comet :regular :unknown)
    :doc ":AJAX, :COMET, :REGULAR or :UNKNOWN.")


(define-variable *server*
    :value nil
    :type (or null server))

(define-variable *app*
    :value nil
    :type (or null application))

(define-variable *viewport*
    :value nil
    :type (or null viewport))


;; Ok, this might be somewhat of a "wrong" thing to do.. TODO: Not a special variable.
(define-symbol-macro *root* (root-widget-of *viewport*))


(define-variable *js-code-only-p*
    :value nil
    :type (member t nil)
    :doc "Used by the macros JS-CODE-OF and WITH-JS-CODE-FROM (src/js/util.lisp).")


(define-variable *bulk-update-p*
    :value nil
    :type (or null cons))


;; :AROUND INITIALIZE-INSTANCE ((.. widget) ..)
(define-variable *currently-constructing-widget*
    :value nil
    :type (or null widget))


;; (ajax.lisp widgets/events.lisp)
(define-variable *current-event*
    :value nil
    :type (or null event)
    :doc "This will contain an instance of EVENT (widgets/events.lisp).")


;;; Stuff for HTML-CONTAINER (widgets/html-container.lisp).
(define-variable *creating-html-container-p*
    :value nil
    :type (or null html-container))

(define-variable *html-container-children*
    :value nil
    :type list)