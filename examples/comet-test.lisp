;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(defvar x #λ0)
(defvar square #λ(* ~x ~x))


(defclass comet-test-app (application)
  ())

(set-uri 'comet-test-app "/comet-test-app")


(defmethod main ((app comet-test-app))
  (defvar comet-test-app-process
    (with-sthread
      (loop
         ;; Make sure we don't waste bandwith; try to group updates caused by the INCF operation together.
         (with-bulk-update
           ;; Sync; for thread safety (STM).
           (with-sync ()
             (incf ~x)))
         (sleep 1)))))


(defmethod render-viewport ((viewport viewport) (app comet-test-app))
  (with-object app
    (insert
      (mk-html ()
        (:div
         (:h1 "COMET-TEST-APP")

          (:b "X: ") (:sw x) :br
          (:b "SQUARE: ") (:sw square)

          :hr
          (:a :href "http://gitorious.org/symbolicweb/symbolicweb/blobs/raw/master/examples/comet-test.lisp"
              "source code")
          :br
          "PS: Hosted on a crummy home ADSL line..."))
      :in (root))))
