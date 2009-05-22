;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defmacro with-visible-contexts-of (widget viewport-sym &body body)
  "Execute BODY in context of all viewports where WIDGET is visible.
VIEWPORT-SYM is bound to a new viewport on each iteration.

NOTE: *VIEWPORT* is still bound to the \"original\" viewport, unless VIEWPORT-SYM
is supplied with the symbol *VIEWPORT*."
  (once-only (widget)
    `(with-each-viewport-of-widget (:widget ,widget :viewport-sym ,viewport-sym)
       (when (visible-p-of ,widget :viewport ,viewport-sym)
         ,@body))))


(defmacro with-each-widget-in-tree ((&key (root (error ":ROOT required."))
                                          (widget-sym 'widget)) &body body)
  `(for-each-widget-in-tree ,root
                            (lambda (,widget-sym)
                              (declare (type widget ,widget-sym))
                              ,@body)))
(export '(with-each-widget-in-tree widget))


(defmacro with-each-viewport-of-widget ((&key (widget (error ":WIDGET needed."))
                                          (viewport-sym 'viewport))
                                    &body body)
  "BODY is executed with VIEWPORT-SYM bound to each viewport in WIDGET.
Also see WITH-EACH-VIEWPORT-IN-APP."
  `(for-each-viewport-of-widget ,widget
                            (lambda (,viewport-sym)
                              (declare (type viewport ,viewport-sym))
                              ,@body)))
(export '(with-each-viewport-of-widget viewport))


(defmacro with-each-viewport-in-app ((&key (viewport-sym 'viewport) (app '*app*)) &body body)
    "BODY is executed with VIEWPORT-SYM bound to each viewport in WIDGET.
Also see WITH-EACH-VIEWPORT-OF-WIDGET."
    `(for-each-viewport-in-app (lambda (,viewport-sym)
                             (declare (type viewport ,viewport-sym))
                             ,@body)
                           ,app))
(export '(with-each-viewport-in-app viewport))


(defmacro with-each-viewport-in-server ((&key (viewport-sym '*viewport*) (server '*server*) (app '*app*))
                                        &body body)
  `(maphash (lambda (id ,app)
              (declare (ignore id))
              (with-each-viewport-in-app (:viewport-sym ,viewport-sym :app ,app)
                ,@body))
            (id->app-of ,server)))
(export 'with-each-viewport-in-server)



;;; code.lisp
;;;;;;;;;;;;;

(defmacro js-code-of (form)
  "Returns the JS code of a single form instead of sending it to the client and
executing it there."
  `(let ((*js-code-only-p* t))
     ,form))
(export 'js-code-of)


(defmacro with-js-code-from (&body body)
  "All forms within BODY must return JS code.
\(setf (on-click-of button
                    :js-after
                    (with-js-code-from
                      (setf (background-color-of button) \"blue\")
                      (alert \"A button was clicked, it is now blue.\")))
       (iambda (write-line \"A button was clicked on the client, it is now blue.\")))"
  `(let ((*js-code-only-p* t))
     (catstr ,@body)))
(export 'with-js-code-from)


(defmacro with-js-code-only (&body body)
  `(let ((*js-code-only-p* t))
     ,@body))
(export 'with-js-code-only)
