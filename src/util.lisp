;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

(declaim #.(optimizations :util.lisp))


(defun js-sw-headers (application)
  (declare (application application)
           (optimize speed))
  (catstr
    ;; NOTE: This is a patched version of jQuery!
    "<script type='text/javascript' src='"
    (mk-static-data-url *server* "javascript/jquery-1.3.2-min.js")
    "'></script>"

    ;;"<script type='text/javascript' src='"
    ;;(mk-static-data-url *server* "javascript/jquery.address-1.0.min.js")
    ;;"'></script>"

    "<script type='text/javascript' src='"
    (mk-static-data-url *server* "javascript/jquery-debounce/jquery.debounce.js")
    "'></script>"

    ;; It is important that this is done before sw-ajax.js (below) is loaded+evaled.
    "<script type='text/javascript'>"
    "sw_viewport_id = \"" (id-generator-next-str -id-generator-) "\";"
    "sw_dynamic_subdomain = \""
    (if-let ((dynamic-data-subdomain (generate-dynamic-subdomain application)))
      (catstr (the string dynamic-data-subdomain) ".")
      "")
    "\";"
    (js-code-of (set-document-cookie))
    "</script>"

    "<script type='text/javascript' defer='defer' src='"
    (mk-static-data-url *server* "javascript/sw/sw-ajax.js")
    "'></script>"))
(export 'js-sw-headers)


(defmethod mk-static-data-url ((server server) (last-part string))
  (declare (optimize speed))
  (let ((static-data-subdomain (static-data-subdomain-of server))
        (static-data-path (static-data-path-of server)))
    ;; TODO: The logic here should perhaps be moved to SW-HTTP.
    (catstr (or (sw-http:get-header "X-FORWARDED-PROTO") "http") "://"
            (if static-data-subdomain
                (catstr static-data-subdomain ".")
                "")
            (host server :include-subdomain-p nil)
            (if static-data-path
                (catstr "/" static-data-path)
                "")
            "/"
            last-part)))
(export 'mk-static-data-url)


(defmethod mk-static-data-url ((app application) (last-part string))
  (mk-static-data-url *server* last-part))
(export 'mk-static-data-url)


(defmacro who (&body body)
  `(with-html-output-to-string (,(gensym "who-string"))
     ,@body))
(export 'who)


(defmethod convert-tag-to-string-list ((tag (eql :sw)) attr-list body body-fn)
  (declare (ignore tag attr-list body-fn))
  ;; TODO: Optimize this. Check if body is in fact an atom. Check if it is
  ;; constant and whether it is a widget or not.
  `((str (shtml-of (let ((maybe-widget (progn ,@body)))
                     (typecase maybe-widget
                       (widget maybe-widget)
                       (single-value-model (mk-elt (:span :model maybe-widget)))
                       (t (error "CL-WHO syntax for :SW doesn't know what to do with: ~S" maybe-widget))))))))


;; NOTE/TODO: Placed here because of bootstrapping issues.. move to some bootstrap*.lisp file?
#.(maybe-inline 'for-each-widget-in-tree)
(defun for-each-widget-in-tree (widget func)
  (declare ((function (widget-base)) func)
           (widget-base widget)
           (optimize speed)
           (notinline for-each-widget-in-tree))
  (funcall func widget)
  (when (typep widget 'container-base)
    (dolist (child (children-of widget))
      (for-each-widget-in-tree child func)))
  (values))
(export 'for-each-widget-in-tree)


(defun sw-logo (title)
  (who
   (:b (:pre (str (mkstr
" __.        .     .    .  .   .
\(__   .._ _ |_  _ |* _.|  | _ |_
.__)\\_|[ | )[_)(_)||(_.|/\\|(/,[_) v0.3
====._|=========================> " title "
<a href=\"http://nostdal.org/\">http://sw.nostdal.org/</a>"
))))))
(export 'sw-logo)


(defun sw-heading (&key title)
  (who
   (:div :id "layout-heading"
     (:table :style "width: 100%"
       (:tr
        (:td (str (sw-logo title)))
        (:td
         (:ul (:li "(ID-OF *APP*): " (str (id-of *app*)))
              (:li "sw-viewport-id: " (:span :id "sw-viewport-id"))))))
     :hr)))
(export 'sw-heading)


(declaim (inline js-str))
(defn js-str (string ((str string)))
  (format nil "\"~A\"" str))


;; TODO: This belongs in SW-MVC.
(defun remove-and-return-closest (item sequence &key
                                  (test #'eql)
                                  (return-if-none-left nil))
  "Returns (VALUES closest new-sequence)"
  (if-let (siblings (cl:remove item sequence :test test))
    (let ((pos-of-removed (position item sequence :test test)))
      (values (or (nth (1+ pos-of-removed) sequence)
                  (nth (1- pos-of-removed) sequence))
              siblings))
    return-if-none-left))


(defun generate-backtrace ()
  (escape-for-html
   (with-output-to-string (ss)
     (sb-debug:backtrace most-positive-fixnum ss))))
(export 'generate-backtrace)


(defun generate-random-cookie-value (server)
  (declare (server server))
  (with-open-file (s "/dev/urandom" :direction :input :element-type '(unsigned-byte 8))
    (loop
       (let ((cookie-value (apply #'concatenate 'string
                                  (loop :repeat 50 :collecting (write-to-string (read-byte s)
                                                                                :pretty nil
                                                                                :base 36)))))
         (multiple-value-bind (app found-p) (gethash cookie-value (cookie-value->app-of server))
           (declare (ignore app))
           (unless found-p
             (return-from generate-random-cookie-value cookie-value)))))))


(defun get-widget (id &key (app *app*))
  (declare (string id)
           (application app))
  (gethash id (widgets-of app)))
(export 'get-widget)


(defun load-css (id url &key force-p (viewport *viewport*))
  (run (format nil "$('head').append(\"<link id='~A' rel='stylesheet' type='text/css' href='~A'></link>\");"
               id
               (if force-p
                   (catstr url "?_=" (id-generator-next-str -id-generator-))
                 url))
       viewport))


(defun unload-css (id &optional (viewport *viewport*))
  (run (js-remove id) viewport))
