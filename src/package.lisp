;;;; http://nostdal.org/ ;;;;

(amx:define-package :symbolicweb
    :use (:amx :sw-mvc :sw-stm)
    :nicknames '(:sw))
(in-package sw)


(do-external-symbols (sym (find-package :cl-who)) ;; HTM, STR, ...
  (shadowing-import sym))


(unintern 'sw-mvc:value-of)
(unintern 'sw-mvc:left-of)
(unintern 'sw-mvc:right-of)
(unintern 'sw-mvc:container)
(unintern 'sw-mvc:container-insert)
(unintern 'sw-mvc:container-remove)
(unintern 'sw-mvc:container-remove-all)
(unintern 'sw-mvc:container-exchange)