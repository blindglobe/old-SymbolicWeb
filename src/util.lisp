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

    ;; User-specified static JavaScript files.
    (with-output-to-string (ss)
      (maphash (lambda (signature url)
                 (when (eq :js (cdr signature))
                   (format ss "<script type='text/javascript' src='~A'></script>" url)))
               (resources-of application)))

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


#| NOTE: Trying to push a lot of the work here to the client is tricky; i.e. using JavaScript for HTML attributes
won't work:

  <script type="text/javascript" src=window.location.protocol + ...></script>
|#
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


(defmethod mk-static-data-url ((app application) (last-part string))
  (mk-static-data-url (server-of app) last-part))


(defmacro who (&body body)
  `(with-html-output-to-string (,(gensym "who-string"))
     ,@body))


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


(defun sw-logo (title)
  (who
   (:b (:pre (str (mkstr
" __.        .     .    .  .   .
\(__   .._ _ |_  _ |* _.|  | _ |_
.__)\\_|[ | )[_)(_)||(_.|/\\|(/,[_) v0.3
====._|=========================> " title "
<a href='http://nostdal.org/'>http://sw.nostdal.org/</a>"
))))))


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


(declaim (inline js-str))
(defn js-str (string ((str string)))
  (format nil "'~A'" str))


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


(flet ((%css (id css-code)
             (format nil "$('head').append(\"<style id='~A' type='text/css'>\" + decodeURIComponent(\"~A\") + \"</style>\");~%"
                     id (url-encode css-code))))
  (defun load-resource (id type source &key force-p (viewport *viewport*))
    (declare (string id)
             ((member :css :js) type)
             (string source)
             ((member nil t) force-p)
             (viewport viewport))
    (when-commit ()
      (with (resources-of viewport)
        (let ((signature (cons id type)))
          (sb-ext:with-locked-hash-table (it)
            (multiple-value-bind (value found-p) (gethash signature it)
              (declare (ignore value))
              (unless (and found-p (not force-p))
                (unless found-p
                  (setf (gethash signature it) signature))
                (ecase type
                  (:css
                   (when found-p
                     (run (js-remove id) viewport)) ;; Remove old stylesheet first.
                   (run (%css id source) viewport))

                  (:js
                   (run source viewport)))))))))))


(defun unload-resource (id type &optional (viewport *viewport*))
  (declare (string id)
           ((member :css) type)
           (viewport viewport))
  (ecase type
    (:css
     (when-commit ()
       (with (resources-of viewport)
         (let ((signature (cons id type)))
           (sb-ext:with-locked-hash-table (it)
             (multiple-value-bind (value found-p) (gethash signature it)
               (declare (ignore value))
               (when found-p
                 (remhash signature it)
                 (run (js-remove id) viewport))))))))))
