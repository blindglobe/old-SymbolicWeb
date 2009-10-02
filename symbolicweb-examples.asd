;;;; http://nostdal.org/ ;;;;

(defsystem symbolicweb-examples
  :author "Lars Rune Nøstdal <larsnostdal@gmail.com> http://nostdal.org/"
  :depends-on (:symbolicweb)

  :serial t
  :components
  ((:module examples
    :serial t
    :components
    (;(:file "package")
     ;(:file "chat")
     ;(:file "tab")
     ;(:file "combo-box")
     ;(:file "mvc-container")
     ;(:file "radio-button")
     ;(:file "counter")
     ;(:file "checkbox")
     ;(:file "history")
     ;(:file "phidgets-888")
     ;(:file "show-hide-widgets")
     ;(:file "urlizing")
     ;(:file "spin-button")
     (:file "nostdal.org")
     ;;(:file "simple-event-flow")
     (:file "ex-text-input")
     (:file "comet-test")
     (:file "blink-app")
     #|(:file "resize-event")|#
     #|(:file "vecto-1")|#
     (:file "vecto-2")
     (:file "vecto-3")
     (:file "tabs")
     (:file "mvc-validation")
     ))))