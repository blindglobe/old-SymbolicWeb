(in-package #:sw)

(start-sw :port 6001
          :static-data-fs-path (catstr (sb-posix:getcwd) "/data/"))


(setf (debug-p-of *server*) nil)
;;(setf *server-close-connection-p* t) ;; This must be T when running behind Lighttpd-1.4.x

;(set-uri 'sw-examples::mvc-container-app "/mvc-container-app")
;(set-uri 'sw-examples::chat-app "/chat")
;(set-uri 'sw-examples::tab-app "/tab-app")
;(set-uri 'sw-examples::radio-button "/radio-button")
;(set-uri 'sw-examples::counter-app "/counter")
;(set-uri 'sw-examples::radio-button-app "/radio-button-app")
;(set-uri 'sw-examples::combo-box-app "/combo-box-app")
;(set-uri 'sw-examples::counter-app "/counter")
;(set-uri 'sw-examples::checkbox-app "/checkbox-app")
;(set-uri 'sw-examples::history-app "/history-app")
;(set-uri 'sw-examples::pg-888-app "/pg-888")
;(set-uri 'sw-examples::show-hide-app "/show-hide-app")
;(set-uri 'sw-examples::urlizing-app "/urlizing-app")
;(set-uri 'sw-examples::spin-button-app "/spin-button-app")

(set-uri 'sw::nostdal-app "/")
(set-uri 'sw::simple-event-flow-app "/simple-event-flow")
(set-uri 'sw::text-input-app "/text-input")
