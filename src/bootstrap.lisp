;;;; http://nostdal.org/ ;;;;

(in-package :sw)


(eval-now
  (shadow '=common-headers=))

(eval-now
  (define-symbol-macro =common-headers=
      (progn
        (in-readtable symbolicweb))))
