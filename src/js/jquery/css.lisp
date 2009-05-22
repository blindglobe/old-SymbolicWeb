;;;; http://nostdal.org/ ;;;;

(in-package :sw-jquery)

(declaim #.(optimizations))


(declaim (inline js-get-css))
(defun js-get-css (selector style-property)
  (declare (string selector style-property))
  (catstr "return $(\"#" selector "\").css(\"" style-property "\");"))
(export 'js-get-css)


(declaim (inline js-set-css))
(defun js-set-css (selector style-property new-value)
  (declare (string selector style-property new-value))
  (catstr "$(\"#" selector "\").css(\"" style-property "\", \"" new-value "\");"))
(export 'js-set-css)


(declaim (inline js-set-css*))
(defun js-set-css* (selector properties)
  (declare (string selector))
  (catstr "$(\"#" selector "\").css(" properties ");"))
(export 'js-set-css*)


(declaim (inline js-add-class))
(defun js-add-class (selector class)
  (declare (string selector class))
  (catstr "$(\"#" selector "\").addClass(\"" class "\");"))
(export 'js-add-class)


(declaim (inline js-remove-class))
(defun js-remove-class (selector class)
  (declare (string selector class))
  (catstr "$(\"#" selector "\").removeClass(\"" class "\");"))
(export 'js-remove-class)


(declaim (inline js-toggle-class))
(defun js-toggle-class (selector class)
  (declare (string selector class))
  (catstr "$(\"#" selector "\").toggleClass(\"" class "\");"))
(export 'js-toggle-class)
