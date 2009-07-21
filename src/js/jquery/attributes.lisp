;;;; http://nostdal.org/ ;;;;

(in-package :sw-jquery)

(declaim #.(optimizations))


(declaim (inline js-get-attribute))
(defun js-get-attribute (widget-id attribute)
  (declare (string widget-id attribute))
  (catstr "return $(\"#" widget-id "\").attr(\"" attribute "\");"))
(export 'js-get-attribute)


(declaim (inline js-set-attribute))
(defun js-set-attribute (widget-id attribute new-value)
  (declare (string widget-id attribute new-value))
  (catstr "$(\"#" widget-id "\").attr(\"" attribute "\", "
          "decodeURIComponent(\"" (url-encode new-value) "\"));"))
(export 'js-set-attribute)


(declaim (inline js-remove-attribute))
(defun js-remove-attribute (widget-id attribute)
  (declare (string widget-id attribute))
  (catstr "$(\"#" widget-id "\").removeAttr(\"" attribute "\");"))
(export 'js-remove-attribute)
