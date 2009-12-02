;;;; http://nostdal.org/ ;;;;


(defpackage :symbolicweb
  (:use :sw-mvc)
  (:nicknames :sw))
(in-package sw)


(do-external-symbols (sym (find-package :cl-who)) ;; HTM, STR, ...
  (shadowing-import sym))


(shadow 'value-of)
(shadow 'left-of)
(shadow 'right-of)
(shadow 'container)
(shadow 'container-insert)
(shadow 'container-remove)
(shadow 'container-remove-all)
(shadow 'container-exchange)
(shadow 'optimizations)