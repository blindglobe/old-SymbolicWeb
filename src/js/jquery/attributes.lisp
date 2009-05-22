;;;; http://nostdal.org/ ;;;;

(in-package :sw-jquery)

(declaim (optimize speed))


(declaim (inline js-get-attribute))
(defun js-get-attribute (selector attribute)
  (declare (string selector attribute))
  (catstr "return $(\"#" selector "\").attr(\"" attribute "\");"))
(export 'js-get-attribute)


(declaim (inline js-set-attribute))
(defun js-set-attribute (selector attribute new-value)
  (declare (string selector attribute new-value))
  (catstr "$(\"#" selector "\").attr(\"" attribute "\", "
          "decodeURIComponent(\"" (url-encode new-value) "\"));"))
(export 'js-set-attribute)

