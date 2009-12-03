;;;; http://nostdal.org/ ;;;;


(defpackage :symbolicweb
  (:use :sw-db)
  (:nicknames :sw))
(in-package sw)


(do-external-symbols (sym (find-package :cl-who)) ;; HTM, STR, ...
  (shadowing-import sym))


(shadow 'container)
(shadow 'container-insert)
(shadow 'container-remove)
(shadow 'container-remove-all)
(shadow 'container-exchange)
(shadow 'optimizations)