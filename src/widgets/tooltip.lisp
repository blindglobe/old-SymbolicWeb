;;;; http://nostdal.org/ ;;;;

(in-package #:sw)


(define-dom-property 'tooltip-of
    :dom-client-writer (lambda (new-value widget &rest args &key show-p)
                         (apply #'run (catstr "$('#" (id-of widget) "').tooltip({ "
                                              "content: function(){ return \"" new-value "\" }, "
                                              "tooltipClass: 'ui-state-error'"
                                              " })" (if show-p
                                                        ".tooltip('show');"
                                                        ";")
                                              +lf+)
                                widget args))

    :dom-client-reader (lambda (widget)
                         (declare (ignore widget))
                         (warn "TOOLTIP-OF: :DOM-CLIENT-READER, TODO"))

    :dom-client-remover (lambda (widget &rest args)
                          (apply #'run (catstr "$('#" (id-of widget) "').tooltip('destroy');")
                                 widget args)))
(export 'tooltip-of)
