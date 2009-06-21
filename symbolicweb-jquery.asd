;;;; http://nostdal.org/ ;;;;

(defsystem symbolicweb-jquery
  :description "
SymbolicWeb - jQuery backend
----------------------------

  Note that by default code in this system is declaimed (speed 3) and (safety 0).
This means that users should not use this system directly, but instead define
their own wrappers which do type-checking.

  If this is not done, there is a risk of random heap corruption."

  :author "See the file AUTHORS"
  :licence "See the file LICENCE"

  :depends-on (:aromyxo)

  :serial t
  :components
  ((:module "src/js/jquery"
    :serial t
    :components
    ((:file "package")
     (:file "config")
     ;;(:file "sw-jquery-mapping")
     (:file "jquery-util")
     (:file "selectors")
     (:file "manipulation")
     (:file "attributes")
     (:file "events")
     (:file "css")
     ))))