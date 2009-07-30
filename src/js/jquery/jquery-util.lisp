;;;; http://nostdal.org/ ;;;;

(in-package :sw-jquery)

(declaim #.(optimizations))


(let ((counter 0))
  (declare (integer counter))
  (defun js-genid (&optional str)
    (declare (optimize (speed 3) (debug 0) (safety 0) (space 0))
             (sb-ext:muffle-conditions sb-ext:compiler-note))
    (if str
        (catstr str "-" (princ-to-string (incf counter)))
        (princ-to-string (incf counter)))))
(export 'js-genid)


(declaim (inline js-callback-data))
(defun js-callback-data (callback-data)
  "Convert from Lisp ((\"key\" . \"value\"))
to URL type string; \"key=value&other-key=other-value\""
  (declare (list callback-data))
  (unless callback-data (return-from js-callback-data ""))
  ;; TODO: Try to make this faster/nicer.
  (subseq (with-output-to-string (s)
            (dolist (name-value-pair callback-data)
              (princ "&" s)
              (princ (url-encode (let ((name (car name-value-pair)))
                                   (if (keywordp name)
                                       (string-downcase name)
                                       name)))
                     s)
              (princ "=" s)
              ;; The values must really be JS expressions like "return 42;" because this allows us to also return
              ;; results of evaluating JS code on the client-side.
              (princ (catstr "\" + encodeURIComponent((function(){ "
                             (cdr name-value-pair)
                             " })()) + \"")
                     s)))
          1))
(export 'js-callback-data)


(declaim (inline js-msg))
(defun js-msg (widget-id callback-id &key
               (js-before *js-before*)
               (callback-data nil)
               (js-after *js-after*)
               (browser-default-action-p t)
               (context-sym 'context))
  (declare (string widget-id callback-id))
  (catstr "(function(" (string-downcase context-sym) "){"
          "swMsg(\"" widget-id "\""
            ", \"" callback-id "\""
            ", (function(){" js-before "})"
            ", \"" (js-callback-data callback-data) "\""
            ", (function(data, text_status){" js-after "}));"
            (if browser-default-action-p
                "return true;"
                "return false;")
          "})"))
(export 'js-msg)


(declaim (inline js-focus))
(defun js-focus (selector)
  (declare (string selector))
  ;; TODO: Firefox has a bug that requires one to add a silly delay like this in some cases. I need to research this further.
  ;; https://bugzilla.mozilla.org/show_bug.cgi?id=53579
  ;; https://bugzilla.mozilla.org/show_bug.cgi?id=297134
  ;; TODO: Ok, seems Firefox-3.x has fixed this but, uh, IE6.x still requires this in some cases ...
  (catstr "setTimeout(function(){ $(\"#" selector "\").focus(); }, 100);"))
(export 'js-focus)


(defun js-blur (widget-id)
  (declare (string widget-id))
  (catstr "$(\"#" widget-id "\").blur();" +lf+))
(export 'js-blur)


(declaim (inline js-scroll-to-bottom))
(defun js-scroll-to-bottom (selector)
  (declare (string selector))
  (catstr "$(\"#" selector "\").get(0).scrollTop = $(\"" selector "\").get(0).scrollHeight + 1000;" +lf+))
(export 'js-scroll-to-bottom)
