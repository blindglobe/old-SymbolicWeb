;;;; http://nostdal.org/ ;;;;

(in-package :sw-jquery)

(declaim #.(optimizations))


(defvar *js-before* "return true;") (export '*js-before*)
(defvar *js-after* "") (export '*js-after*)


(declaim (inline js-callback-id-of))
(defun js-callback-id-of (selector event-type)
  (declare (string selector event-type))
  (catstr selector "-" event-type))
(export 'js-callback-id-of)


(declaim (inline js-unbind))
(defun js-unbind (selector &optional event-type)
  (declare (string selector))
  (catstr
   "$(\"#" selector "\").unbind("
   (if event-type
       (catstr "\"" event-type "\"")
       "")
   ");"))
(export 'js-unbind)


(declaim (inline js-bind))
(defun js-bind (selector event-type callback-id &key client-side-only-p
                callback-data js-before js-after
                browser-default-action-p)
  (declare (string selector event-type callback-id))
  (unless js-before (setf js-before *js-before*)) ;; Because NIL is always supplied from the stuff in dom-cache.lisp
  (unless js-after (setf js-after *js-after*))
  ;; TODO: Code here isn't very "nice".
  ;; Convert (("key" . "value")) => "&key=value"
  (let ((callback-data
         (if callback-data
             (subseq (with-output-to-string (s)
                       (dolist (name-value-pair callback-data)
                         (princ "&" s)
                         (princ (url-encode (let ((name (car name-value-pair)))
                                              (if (keywordp name)
                                                  (string-downcase (string name))
                                                  name)))
                                s)
                         (princ "=" s)
                         ;; Not url-encoding this because it is usually JS code.
                         (princ (catstr "\" + encodeURIComponent((function(){" (cdr name-value-pair) "})()) + \"") s)))
                     1)
             "")))
    (catstr "
$(\"#" selector "\").unbind(\"" event-type "\");
$(\"#" selector "\").bind(\"" event-type "\", function(event)
{
" (if client-side-only-p
      ;; TODO: Need to add a try/catch wrapper for this, and it needs to be moved to sw-ajax.js.
      (catstr "if((function(){" js-before "})()) " client-side-only-p js-after)
      (catstr "swHandleEvent(\"" callback-id "\","
              "function(){ " js-before "},"
              "\"" callback-data "\","
              "function(data, text_status){" js-after "});"))
" return " (if browser-default-action-p "true" "false") "; });")))
(export 'js-bind)


(declaim (inline js-trigger))
(defun js-trigger (selector event-type)
  (declare (string selector event-type))
  (catstr "$(\"#" selector "\").trigger(\"" event-type "\");"))
(export 'js-trigger)


(declaim (inline js-trigger-handler))
(defun js-trigger-handler (selector event-type)
  (declare (string selector event-type))
  (catstr "$(\"#" selector "\").triggerHandler(\"" event-type "\");"))
(export 'js-trigger-handler)


(defun js-blur (selector)
  (declare (string selector))
  (catstr "$(\"#" selector "\").blur();"))
(export 'js-blur)
