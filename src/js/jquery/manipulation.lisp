;;;; http://nostdal.org/ ;;;;

(in-package :sw-jquery)

(declaim #.(optimizations))


(declaim (inline (setf js-html-of)))
(defun (setf js-html-of) (new-html widget-id)
  (declare (string new-html widget-id))
  (catstr "$(\"#" widget-id "\").html(decodeURIComponent(\"" (the string (url-encode new-html)) "\"));"))
(export 'js-html-of)


(declaim (inline js-html-of))
(defun js-html-of (widget-id)
  (declare (string widget-id))
  (catstr "return $(\"#" widget-id "\").html();"))
(export 'js-html-of)


(declaim (inline js-iappend))
(defun js-iappend (html widget-id)
  (declare (string html widget-id))
  ;; append
  "inside append"
  (catstr "$(\"#" widget-id "\").append(decodeURIComponent(\"" (the string (url-encode html)) "\"));"))
(export 'js-iappend)


(declaim (inline js-oappend))
(defun js-oappend (html widget-id)
  (declare (string html widget-id))
  ;; after
  "outside append"
  (catstr "$(\"#" widget-id "\").after(decodeURIComponent(\"" (the string (url-encode html)) "\"));"))
(export 'js-oappend)


(declaim (inline js-iprepend))
(defun js-iprepend (html widget-id)
  (declare (string html widget-id))
  ;; prepend
  "inside prepend"
  (catstr "$(\"#" widget-id "\").prepend(decodeURIComponent(\"" (the string (url-encode html)) "\"));"))
(export 'js-iprepend)


(declaim (inline js-oprepend))
(defun js-oprepend (html widget-id)
  (declare (string html widget-id))
  ;; before
  "outside prepend"
  (catstr "$(\"#" widget-id "\").before(decodeURIComponent(\"" (the string (url-encode html)) "\"));"))
(export 'js-oprepend)


(declaim (inline js-remove))
(defun js-remove (widget-id)
  (declare (string widget-id))
  (catstr "$(\"#" widget-id "\").remove();"))
(export 'js-remove)


(declaim (inline js-replace-with))
(defun js-replace-with (widget-id new-content)
  (declare (string widget-id new-content))
  (catstr "$(\"#" widget-id "\").replaceWith(decodeURIComponent(\"" (the string (url-encode new-content)) "\"));"))
(export 'js-replace-with)


(declaim (inline js-empty))
(defun js-empty (widget-id)
  (declare (string widget-id))
  (catstr "$(\"#" widget-id "\").empty();"))
(export 'js-empty)


(declaim (inline js-exchange))
(defun js-exchange (widget-id-a widget-id-b)
  (declare (string widget-id-a widget-id-b))
  (catstr "(function(){"
          "var first = $(\"#" widget-id-a "\").clone(true);"
          "var second = $(\"#" widget-id-b "\").clone(true);"

          "$(\"#" widget-id-a "\").attr(\"id\",  \"js-exchange-first\");"
          "$(\"#" widget-id-b "\").attr(\"id\", \"js-exchange-second\");"

          "$(\"#js-exchange-first\").replaceWith(second);"
          "$(\"#js-exchange-second\").replaceWith(first);"
          "})();"))
(export 'js-exchange)
