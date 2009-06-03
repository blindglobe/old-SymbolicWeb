;;;; http://nostdal.org/ ;;;;

(in-package :sw-jquery)

(declaim #.(optimizations))


(declaim (inline (setf js-html-of)))
(defun (setf js-html-of) (new-html selector)
  (declare (string new-html selector))
  (catstr "$(\"#" selector "\").html(decodeURIComponent(\"" (the string (url-encode new-html)) "\"));"))
(export 'js-html-of)


(declaim (inline js-html-of))
(defun js-html-of (selector)
  (declare (string selector))
  (catstr "return $(\"#" selector "\").html();"))
(export 'js-html-of)


(declaim (inline js-iappend))
(defun js-iappend (html selector)
  (declare (string html selector))
  ;; append
  "inside append"
  (catstr "$(\"#" selector "\").append(decodeURIComponent(\"" (the string (url-encode html)) "\"));"))
(export 'js-iappend)


(declaim (inline js-oappend))
(defun js-oappend (html selector)
  (declare (string html selector))
  ;; after
  "outside append"
  (catstr "$(\"#" selector "\").after(decodeURIComponent(\"" (the string (url-encode html)) "\"));"))
(export 'js-oappend)


(declaim (inline js-iprepend))
(defun js-iprepend (html selector)
  (declare (string html selector))
  ;; prepend
  "inside prepend"
  (catstr "$(\"#" selector "\").prepend(decodeURIComponent(\"" (the string (url-encode html)) "\"));"))
(export 'js-iprepend)


(declaim (inline js-oprepend))
(defun js-oprepend (html selector)
  (declare (string html selector))
  ;; before
  "outside prepend"
  (catstr "$(\"#" selector "\").before(decodeURIComponent(\"" (the string (url-encode html)) "\"));"))
(export 'js-oprepend)


(declaim (inline js-remove))
(defun js-remove (selector)
  (declare (string selector))
  (catstr "$(\"#" selector "\").remove();"))
(export 'js-remove)


(declaim (inline js-replace-with))
(defun js-replace-with (selector new-content)
  (declare (string selector new-content))
  (catstr "$(\"#" selector "\").replaceWith(decodeURIComponent(\"" (the string (url-encode new-content)) "\"));"))
(export 'js-replace-with)


(declaim (inline js-empty))
(defun js-empty (selector)
  (declare (string selector))
  (catstr "$(\"#" selector "\").empty();"))
(export 'js-empty)


(declaim (inline js-exchange))
(defun js-exchange (selector-a selector-b)
  (declare (string selector-a selector-b))
  (catstr "(function(){"
          "var first = $(\"#" selector-a "\").clone(true);"
          "var second = $(\"#" selector-b "\").clone(true);"

          "$(\"#" selector-a "\").attr(\"id\",  \"js-exchange-first\");"
          "$(\"#" selector-b "\").attr(\"id\", \"js-exchange-second\");"

          "$(\"#js-exchange-first\").replaceWith(second);"
          "$(\"#js-exchange-second\").replaceWith(first);"
          "})();"))
(export 'js-exchange)
