;;;; http://nostdal.org/ ;;;;

(defsystem symbolicweb-jquery
  :description "SymbolicWeb - jQuery backend"
  :author "See the file AUTHORS"
  :licence "See the file LICENCE"
  
  :depends-on (:aromyxo)
  
  :serial t
  :components
  ((:module "src/js/jquery"
    :serial t
    :components
    ((:file "package")
     ;;(:file "sw-jquery-mapping")
     (:file "jquery-util")
     (:file "selectors")
     (:file "manipulation")
     (:file "attributes")
     (:file "events")
     (:file "css")
     ))))