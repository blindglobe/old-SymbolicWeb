;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :js/util.lisp))


(defun alert (msg &optional (viewport-or-widget *viewport*))
  (let ((js-code (catstr "alert(decodeURIComponent(\"" (url-encode msg) "\"));" +lf+)))
    (if *js-code-only-p*
        js-code
        (run js-code viewport-or-widget))))
(export 'alert)


#|(defun window-location-href (&optional (viewport *viewport*))
  (declare (type viewport viewport))
  (let ((js-code "return window.location.href;"))
    (if *js-code-only-p*
        js-code
        (run js-code viewport :async-p nil))))|#
#|(export 'window-location-href)|#


#|(defun (setf window-location-href) (new-href &optional (viewport *viewport*))
  (declare (type viewport viewport))
  (let ((js-code (catstr "window.location.href=\""
                         new-href ;;(cl-ppcre:regex-replace-all " " new-href "_")
                         "\";")))
    (if *js-code-only-p*
        js-code
        (run js-code viewport))))|#
#|(export 'window-location-href)|#


#|(defun get-title (&optional (viewport *viewport*))
  (declare (type viewport viewport))
  (let ((js-code "return document.title;"))
    (if *js-code-only-p*
        js-code
        (run js-code viewport :async-p nil))))|#
#|(export 'get-title)|#


#|(defun set-title (new-title &optional (viewport *viewport*))
  (declare (type viewport viewport))
  (let ((js-code (catstr "document.title = decodeURIComponent(\""
                         (url-encode new-title)
                         "\");")))
    (if *js-code-only-p*
        js-code
        (run js-code viewport))))|#
#|(export 'set-title)|#


(defun reload (&optional (viewport *viewport*))
  "Do a page reload equal to the user pressing F5 or Ctrl-R in the browser."
  (let ((js-code (catstr "window.location.reload();" +lf+)))
    (if *js-code-only-p*
        js-code
        (run js-code viewport))))
(export 'reload)


(defun window-location-href (url &optional (viewport *viewport*))
  (declare (string url))
  (let ((js-code (catstr "window.location.href(decodeURIComponent(\"~A\"));"
                         (url-encode url)
                         +lf+)))
    (if *js-code-only-p*
        js-code
        (run js-code viewport))))
(export 'window-location-href)


(defmethod redraw ((p (eql t)))
  "Redraw all elements on the page without reloading (see RELOAD) the page."
  (declare (ignore p))
  (render (root)))
(export 'redraw)


(defun fade-in (widget &key (speed 400))
  (declare (type widget widget))
  (flet ((js-code ()
           (catstr "$(\"#" (id-of widget) "\")"
                   ".css('opacity', 0)" ;; We do not store the opacity server side (page refresh).
                   ".removeClass('sw-hide')"
                   ".fadeTo(" (princ-to-string speed)
                   ", 1" ;; Opacity goal.
                   ");" +lf+)))
    (declare (inline js-code))
    (if *js-code-only-p*
        (js-code)
        (progn
          (show widget :server-only-p t)
          (run (js-code) widget)
          widget))))
(export 'fade-in)


(defun fade-out (widget &key (speed 400))
  (declare (type widget widget))
  (flet ((js-code ()
           (catstr "$(\"#" (id-of widget) "\")"
                   ".fadeTo(" (princ-to-string speed)
                   ", 0" ;; Opacity goal.
                   ", function(){ $(this).addClass('sw-hide'); }"
                   ");" +lf+)))
    (declare (inline js-code))
    (if *js-code-only-p*
        (js-code)
        (progn
          (run (js-code) widget)
          (hide widget :server-only-p t)
          widget))))
(export 'fade-out)


(defun set-loading-p (pred &optional (viewport *viewport*))
  (let ((js-code
         (if pred
             (js-set-css "sw-loading-spinner" "display" "block")
             (js-set-css "sw-loading-spinner" "display" "none"))))
    (if *js-code-only-p*
        js-code
        (progn
          (run js-code viewport)
          pred))))
(export 'set-loading-p)


(defun set-document-cookie (&key
                            (app *app*)
                            (name (cookie-name-of (server-of app)))
                            (value (cookie-value-of app))
                            (viewport *viewport*))
  (declare (string name)
           ((or null string) value)
           (optimize speed))
  (let ((js-code
         (flet ((inner (domain-p)
                  (catstr
                    "document.cookie = \""
                    name "=" (if value value "") ";"
                    (if domain-p
                        "domain=.\" + window.location.hostname + \";"
                        "")
                    (if value
                        "expires=\" + (function(){ var date = new Date(); date.setFullYear(date.getFullYear()+1); return date.toUTCString(); })() + \";"
                        "expires=Fri, 27 Jul 2001 02:47:11 UTC;")
                    "path=\" + window.location.pathname + \";"
                    "\";" +lf+)))
           (if value
               (inner t)
               #| NOTE: Firefox seems to have trouble deleting cookies when domain is included, but, uh, sometimes
               not -- so we do both! x) |#
               (catstr (inner t) (inner nil))))))
    (if *js-code-only-p*
        js-code
        (run js-code viewport))))
(export 'set-document-cookie)
