;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass nostdal-app (application)
  ()

  (:metaclass mvc-class))

(set-uri 'nostdal-app "/nostdal.org")


(defmethod render-viewport ((viewport viewport) (app nostdal-app))
  (let ((main
         (mk-html ()
           (:div


            (:h1 "nostdal.org")
            (:em "NOTE: This is being served from an ADSL connection. "
                 "This'll be very slow or even down at random.")
            (:ul
             (:li (:a :href "http://gitorious.org/~lnostdal" "gitorious") ": collection of source code repositories")
             (:li (:a :href "text-input" "text-input-app") ": silly test application"))


            (:h2 "Chat")
            (:iframe :src "http://webchat.freenode.net/?randomnick=1&channels=nostdal.org&prompt=1"
                     :width 647 :height 400)


            (:h2 "Firefox addons")
            (:p "I keep forgetting the names of these, so as a note-to-self (and maybe others) here is "
                "some of the must-have stuff:")
            (:ul
             (:li "Firebug")
             (:li "Firecookie")
             (:li "Flashblock")
             (:li "Google Gears")
             (:li "Page Speed (maybe..)")
             (:li "Tabs Open Relative")
             (:li "Tiny Menu"))


            (:hr :width "100%")
            (:pre "λ(:linux :lighttpd :sbcl :sw-http :sw-mvc :sw-stm :sw-db :symbolicweb)λ")))))
    (setf (margin-of main) "1%")
    (add-to (root) main)))
