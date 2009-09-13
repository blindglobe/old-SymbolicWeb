;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defclass blink-app (application)
  ((state :allocation :class
          :initform nil))

  (:metaclass mvc-class))

(set-uri 'blink-app "/blink-app")


(defmethod main ((app blink-app))
  (defvar blink-app-process
    (with-sthread
      (with-object app
        (loop
           (with-bulk-update
             (with-sync ()
               (setf ¤state (not ¤state))))
           (sleep 1))))))


(defmethod render-viewport ((viewport viewport) (app blink-app))
  (with-object app
    (insert
      (mk-html ()
        (:div
         (:h1 "BLINK-APP")

         (:sw (with1 (h2 "WEB 2.0 is here! ♥")
                (with-formula it
                  (if ¤state
                      (show it)
                      (hide it)))))

         :hr
         (:a :href "http://gitorious.org/symbolicweb/symbolicweb/blobs/raw/master/examples/blink-app.lisp"
             "source code")
         :br
         "PS: Hosted on a crummy home ADSL line..."))
      :in (root))))
