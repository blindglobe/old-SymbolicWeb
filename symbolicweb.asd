;;;; http://nostdal.org/ ;;;;


(defsystem symbolicweb
  :description "SymbolicWeb"
  :author "See the file AUTHORS"
  :licence "See the file LICENSE"

  :depends-on (:sw-http
               :sw-mvc
               :cl-utilities
               :alexandria
               :cl-who
               :aromyxo
               :sw-stm
               :symbolicweb-jquery ;; Default JS generator backend.
               :vecto
               )

  :serial t
  :components
  ((:module src
    :serial t
    :components
    ((:file "package")
     (:file "bootstrap")
     (:file "read-macros")

     (:file "class-bootstrap")
     (:file "macro-bootstrap")
     (:file "specials")
     (:file "config-compilation")
     (:file "config")
     (:file "util")
     (:file "code")
     (:file "server")
     #|(:file "object")|#
     (:file "base-classes")
     (:file "viewport")

     (:module js
       :serial t
       :components
       ((:file "util")
        ))

     (:module widgets
       :serial t
       :components
       (#|(:file "form")|#
        (:file "dom-cache")
        (:file "widget")
        ;;(:file "manipulation")
        (:file "attributes")
        (:file "css")
        (:file "events")
        (:file "focussable")
        (:file "html-element")
        (:file "abstract-container")
        (:file "container")
        (:file "size-tracker")
        (:file "button")
        (:file "html-container")
        (:file "text-input")
        (:file "image")
        (:file "vecto")
        (:file "tab")
        (:file "tooltip")
        (:file "combo-box")
        (:file "check-box")
        #|(:file "dialog")|#
        #|
        (:file "container-with-1-active-item")
        (:file "table-container")
        (:file "location-callback")
        (:file "location-container")
        (:file "spin-button")
        (:file "text-area")
        (:file "toggle-button")
        (:file "radio-button")
        (:file "link")
        (:file "slider")
        (:file "cursor")

        (:file "window")
        (:file "alert-box")
        |#
        ))

     #|(:file "address-bar")|#
     (:file "application")
     (:file "server-sw-http")
     (:file "comet")
     (:file "ajax")
     ;;(:file "pagination")
     (:file "package-export")
     ))
   ))
