;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)


(defclass empty-page-app (application)
  ())

(set-uri 'empty-page-app "empty-page")



#|(defmethod render-viewport ((viewport viewport) (app empty-page-app))
  ;;(dotimes (i 10)
  (add (make-instance 'html-element :model square-of-x) (root)))|#



(defmethod generate-dynamic-subdomain ((app empty-page-app))
  ;; To break the 2 connection limit of HTTP do something like
  ;;   (catstr "sw.dyn-" (generate-id))
  ;; ..here (in your application subclass).
  nil)
  ;;(catstr "sw.dyn-" (generate-id)))
