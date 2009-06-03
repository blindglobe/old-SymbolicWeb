;;;; http://nostdal.org/ ;;;;


(defpackage #:symbolicweb
  (:use #:cl
        #:cl-utilities
        #:alexandria
        #:aromyxo
        #:sw-stm
        #:sw-mvc
        ;;#:sw-db
        #:cl-ppcre
        #:cl-who
        #:closer-mop)

  (:nicknames #:sw)


  ;; Fix conflicts between Alexandria and cl-utilities.
  (:shadowing-import-from #:alexandria
    #:with-gensyms
    #:compose
    #:with-unique-names
    #:copy-array
    #:once-only)


  (:shadow
    ;; From CL.
    #:remove #:replace

    ;; From AROMYXO.
    #:insert #:exchange #:get-obj

    ;; From SW-MVC.
    #:value-of
    #:right-of #:left-of
    #:container
    #:add #:add-to
    #:remove
    )



  ;;; SERVER
  ;;;;;;;;;;

  (:export
   ;; Server types.
   #:server
   #:hunchentoot-server

   ;; Starting and stopping the server(s).
   #:start-server
   #:stop-server
   #:start-sw
   #:stop-sw #:get-sw

   #:set-uri
   #:id-of
   #:port-of
   #:static-data-subdomain-of
   #:static-data-fs-path-of
   #:ht-server-instance-of
   )



  ;;; ID-MIXIN
  ;;;;;;;;;;;;
  (:export
   #:id-mixin
   #:id-of
   #:get-obj
   )



  ;;; APPLICATION
  ;;;;;;;;;;;;;;;
  (:export
   #:application
   #:server-of
   #:cookie-value-of
   #:cookie-domain-of
   #:cookie-expires-of
   #:static-data-subdomain-of
   #:static-data-fs-path-of
   #:initialized-p-of
   #:widgets-of
   #:viewports-of
   #:last-ping-time-of
   #:defapp
   #:main
   #:render
   #:remove
   #:generate-dynamic-subdomain
   )



  ;;; VIEWPORT
  ;;;;;;;;;;;;

  (:export
   #:viewport
   #:application-of
   #:root-widget-of
   #:widgets-of
   #:callbacks-of
   #:address-bar-of
   #:last-ping-time-of
   #:visible-p-of
   #:for-each-viewport-in-app
   #:on-refresh
   #:render-viewport
   #:root
   #:remove
   #:reset
   )



  ;;; OBJECT
  ;;;;;;;;;;

  (:export
   #:object
   #:urlized-p-of
   )



  ;;; ADDRESS-BAR
  ;;;;;;;;;;;;;;;

  (:export
   #:address-bar
   #:viewport-of
   #:objects-of
   #:dirty-p-of
   #:replace-p-of
   #:add-to-address-bar
   #:add-to-address-bar*
   #:remove-from-address-bar
   #:remove-from-address-bar*
   #:handle-address-bar
   #:add-history-antry
   #:mk-href
   #:uri-varue<-state
   #:state<-uri-value
   )



  ;;; CODE
  ;;;;;;;;

  (:export
   #:run-js
   #:run
   #:with-visible-contexts-of
   #:with-code-block
   )



  ;;; WIDGET
  ;;;;;;;;;;

  (:export
   #:widget
   #:viewports-of
   #:on-visibility-change-fns-of
   #:on-render-fns-of
   #:mk-on-visibility-change-fn
   #:on-visibility-change-fn
   #:currently-constructing-p
   #:shtml-of
   #:html<-
   #:defwidget
   #:widget-p
   #:visible-p-of
   #:render
   #:mk-on-render-fn
   #:on-render-fn
   #:focus ;; TODO: Probably belongs in util.lisp
   #:scroll-to-bottom
   #:for-each-viewport-of-widget
   #:shared-p-of
   )


   ;;; CONTAINER
   ;;;;;;;;;;;;;

   (:export
    #:container
    #:num-children-of
    #:add
    #:remove
    #:add-to
    #:oadd
    #:insert
    #:prepend
    #:oprepend
    #:prepend-to
    #:replace
    #:remove-all
    #:children-of
    #:child-of
    #:html-of
    #:mk-container
    #:exchange
    )



   ;;; MODEL stuff
   ;;;;;;;;;;;;;;;

   (:export
    #:model #:view
    #:model-of #:views-of
    #:view-in-context-of
    #:model-add #:model-remove
    #:mk-view
    #:container-list-model #:content-of
    #:single-value-model #:mk-single-value #:value-of
    #:with-bulk-update-of
    #:view-constructor-fn-of)

   (:export
    #:filtered-model #:proxied-model #:proxied-model-of)

   (:export
    #:filtered-container-model #:filter-fn #:filter-fn-of)
   )
