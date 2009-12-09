(in-package sw)
(in-readtable symbolicweb)


(defclass tabs-app (application)
  ())

(set-uri 'tabs-app "tabs-app")


(defmethod initialize-instance :after ((app tabs-app) &key)
  (add-jquery-ui-resources :minified-p t))


(defmethod render-viewport ((viewport viewport) (app tabs-app))
  (with (make-instance 'tab)
    (dotimes (i 3)
      (insert (mk-pair λV(fmtn "label-~D" i) (dlist λV(fmtn "content-~D" i)))
              :in it))

    (with (mk-html ()
            (:div
              (:h1 "TABS-APP")

              (:sw it)

              (:a :href "http://gitorious.org/symbolicweb/symbolicweb/blobs/raw/master/examples/tabs.lisp"
                  "source code")))
      (insert it :in (root)))))
