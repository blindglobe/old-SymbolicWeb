;;;; http://nostdal.org/ ;;;;

(in-package #:sw)

;; TODO: This is epic fail in a meta-class context.

(class-forward-reference server)
(class-forward-reference application)
(class-forward-reference widget-base
  (:metaclass mvc-stm-class))
(class-forward-reference widget
  (:metaclass mvc-stm-class))
(class-forward-reference container
  (:metaclass mvc-stm-class))
(class-forward-reference viewport)
(class-forward-reference callback-box)
;;(class-forward-reference hunchentoot-server)
(class-forward-reference sw-http-server)
