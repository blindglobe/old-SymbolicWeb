(in-package #:sw)


(defclass tabs-app (application)
  ())

(set-uri 'tabs-app "/tabs-app")


(defmethod initialize-instance :after ((app tabs-app) &key)
  ;; jQuery UI core.
  (add-resource app "jquery-ui-core" :css
                "jquery-ui/themes/base/jquery-ui.css")
  (add-resource app "jquery-ui-core" :js
                "jquery-ui/ui/minified/jquery-ui.min.js")

  ;; jQuery UI Tabs widget.
  (add-resource app "jquery-ui-tabs" :css
                "jquery-ui/themes/base/ui.tabs.css")
  (add-resource app "jquery-ui-tabs" :js
                "jquery-ui/ui/minified/ui.tabs.min.js"))


(defmethod render-viewport ((viewport viewport) (app tabs-app))
  (with (make-instance 'tab)
    (dotimes (i 3)
      (insert (mk-pair λv(fmtn "label-~D" i) (dlist λv(fmtn "content-~D" i)))
              :in it))

    (with (mk-html ()
            (:div
              (:h1 "TABS-APP")

              (:sw it)

              (:a :href "http://gitorious.org/symbolicweb/symbolicweb/blobs/raw/master/examples/tabs.lisp"
                  "source code") :br
                  "PS: Hosted on a crummy home ADSL line..."))
      (insert it :in (root)))))
