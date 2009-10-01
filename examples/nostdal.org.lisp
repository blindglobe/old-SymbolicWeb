;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass nostdal-app (application)
  ())

(set-uri 'nostdal-app "nostdal.org")


(defmethod render-viewport ((viewport viewport) (app nostdal-app))
  (with
   (mk-html ()
     (:div
      (:h1 "nostdal.org")

      (:ul
       (:li (:a :href "http://gitorious.org/~lnostdal" "gitorious") ": collection of source code repositories.")
       (:li (:a :href "text-input" "text-input-app") ": dataflow test thing")
       (:li (:a :href "comet-test-app" "comet-test-app") ": comet test thing")
       (:li (:a :href "vecto-2" "vecto-2") ": "
            (:a :href "http://gitorious.org/symbolicweb/symbolicweb/blobs/raw/master/examples/vecto-2.lisp"
                "source code"))
       (:li (:a :href "vecto-3" "vecto-3") ": "
            (:a :href "http://gitorious.org/symbolicweb/symbolicweb/blobs/raw/master/examples/vecto-3.lisp"
                "source code"))
       (:li (:a :href "tabs-app" "tabs") ": tab widget test thing"))


      (:hr :width "100%")
      (:pre "λ(:linux :lighttpd :sbcl :sw-http :sw-mvc :sw-stm :sw-db :symbolicweb)λ" :br
            "NOTE: This is being served from an ADSL connection; things will be slow or down at random.")))
   (setf (margin-of it) "1%")
   (insert it :in (root))))
