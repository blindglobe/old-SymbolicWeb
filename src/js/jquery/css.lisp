;;;; http://nostdal.org/ ;;;;

(in-package :sw-jquery)

(declaim #.(optimizations))


(declaim (inline js-get-css))
(defun js-get-css (widget-id style-property)
  (declare (string widget-id style-property))
  (catstr "return $(\"#" widget-id "\").css(\"" style-property "\");" +lf+))
(export 'js-get-css)


(declaim (inline js-set-css))
(defun js-set-css (widget-id style-property new-value)
  (declare (string widget-id style-property new-value))
  (catstr "$(\"#" widget-id "\").css(\"" style-property "\", \"" new-value "\");" +lf+))
(export 'js-set-css)


(declaim (inline js-set-css*))
(defun js-set-css* (widget-id properties)
  (declare (string widget-id))
  (catstr "$(\"#" widget-id "\").css(" properties ");" +lf+))
(export 'js-set-css*)


(declaim (inline js-add-class))
(defun js-add-class (widget-id class)
  (declare (string widget-id class))
  (catstr "$(\"#" widget-id "\").addClass(\"" class "\");" +lf+))
(export 'js-add-class)


(declaim (inline js-remove-class))
(defun js-remove-class (widget-id class)
  (declare (string widget-id class))
  (catstr "$(\"#" widget-id "\").removeClass(\"" class "\");" +lf+))
(export 'js-remove-class)


(declaim (inline js-toggle-class))
(defun js-toggle-class (widget-id class)
  (declare (string widget-id class))
  (catstr "$(\"#" widget-id "\").toggleClass(\"" class "\");" +lf+))
(export 'js-toggle-class)
