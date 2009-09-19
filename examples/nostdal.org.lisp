;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass nostdal-app (application)
  ()

  (:metaclass mvc-class))

(set-uri 'nostdal-app "/nostdal.org")


(defmethod render-viewport ((viewport viewport) (app nostdal-app))
  (with
      (mk-html ()
        (:div
          (:h1 "nostdal.org")
          (:em "PS: This is being served from an ADSL connection."
               "This'll be very slow or down at random.")

          (:ul
           (:li (:a :href "http://gitorious.org/~lnostdal" "gitorious") ": collection of source code repositories")
           (:li (:a :href "text-input" "text-input-app") ": dataflow test thing")
           (:li (:a :href "comet-test-app" "comet-test-app") ": comet test thing")
           (:li (:a :href "vecto-1" "vecto-1")
                ": "
                (:a :href "http://gitorious.org/symbolicweb/symbolicweb/blobs/raw/master/examples/vecto-1.lisp"
                    "source code"))
           (:li (:a :href "vecto-2" "vecto-2")
                ": "
                (:a :href "http://gitorious.org/symbolicweb/symbolicweb/blobs/raw/master/examples/vecto-2.lisp"
                    "source code"))
           (:li (:a :href "resize-event-app" "resize-event-app")
             ": scalable vector graphics, fonts, text and labels (try resizing your browser window)")
           (:li (:a :href "http://nostdal.org/lnostdal/always-here/sw/vector/" "scalable vector fonts and text")
             ": videos of vector based graphics, fonts and text that scale to any screen size in all browsers"))


          (:hr :width "100%")
          (:pre "λ(:linux :lighttpd :sbcl :sw-http :sw-mvc :sw-stm :sw-db :symbolicweb)λ")))

    (setf (margin-of it) "1%")
    (insert it :in (root))))
