;;;; http://nostdal.org/ ;;;;

(in-package :sw)


(start-sw :port 6001)


(nilf (debug-p-of *server*))

(allf :warn
      sw-stm:-on-read-outside-transaction-
      sw-stm:-on-write-outside-transaction-)

;;(setf *server-close-connection-p* t) ;; This must be T when running behind Lighttpd-1.4.x
