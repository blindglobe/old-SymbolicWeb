;;;; http://nostdal.org/ ;;;;

(in-package :sw-jquery)

(declaim #.(optimizations))


(defvar *js-before* "return true;") (export '*js-before*)
(defvar *js-after* "") (export '*js-after*)


(declaim (inline js-callback-id-of))
(defun js-callback-id-of (widget-id event-type)
  (declare (string widget-id event-type))
  (catstr widget-id "-" event-type))
(export 'js-callback-id-of)


(declaim (inline js-unbind))
(defun js-unbind (widget-id &optional event-type)
  (declare (string widget-id))
  (catstr
   "$(\"#" widget-id "\").unbind("
   (if event-type
       (catstr "\"" event-type "\"")
       "")
   ");"))
(export 'js-unbind)


(defun js-bind (widget-id event-type callback-js)
  (declare (string widget-id event-type callback-js))
  (catstr "$('#" widget-id "').bind('" event-type "', " callback-js ");"))
(export 'js-bind)


(declaim (inline js-trigger))
(defun js-trigger (widget-id event-type)
  (declare (string widget-id event-type))
  (catstr "$(\"#" widget-id "\").trigger(\"" event-type "\");"))
(export 'js-trigger)


(declaim (inline js-trigger-handler))
(defun js-trigger-handler (widget-id event-type)
  (declare (string widget-id event-type))
  (catstr "$(\"#" widget-id "\").triggerHandler(\"" event-type "\");"))
(export 'js-trigger-handler)


(defun js-blur (widget-id)
  (declare (string widget-id))
  (catstr "$(\"#" widget-id "\").blur();"))
(export 'js-blur)
