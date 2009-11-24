;;;; http://nostdal.org/ ;;;;

(in-package sw)
(in-readtable symbolicweb)


;; TODO: Think about HTMLIZE and ESCAPEP (see widgets/html-element.lisp).

(define-dom-property 'tooltip-of
    :dom-client-writer (lambda (new-value widget &rest args &key show-p)
                         (declare (string new-value)
                                  (widget widget)
                                  (dynamic-extent args))
                         (let ((js-code (catstr "$('#" (id-of widget) "').tooltip({ "
                                                "content: function(){ return decodeURIComponent(\""
                                                (url-encode new-value) "\"); }, "
                                                "tooltipClass: 'ui-state-error'"
                                                " })" (if show-p
                                                          ".tooltip('show');"
                                                          ";")
                                                +lf+)))
                           (if *js-code-only-p*
                               js-code
                               (apply #'run js-code widget args))))

    :dom-client-reader (lambda (widget)
                         (declare (ignore widget))
                         (warn "TOOLTIP-OF: :DOM-CLIENT-READER, TODO"))

    :dom-client-remover (lambda (widget &rest args)
                          (declare (widget widget)
                                   (dynamic-extent args))
                          (apply #'run (catstr "$('#" (id-of widget) "').tooltip('destroy');")
                                 widget args)))
(export 'tooltip-of)


(defun add-on-feedback (view fn)
  "FN is a function taking one argument; the FEEDBACK-EVENT value from SW-MVC."
  (declare (view-base view)
           (function fn))
  (let ((cell (cell-of ~view))
        (old nil))
    λI(if-let ((fe (feedback-event-of cell)))
        (when-commit ()
          (setf old fe)
          (funcall fn fe))
        (when-commit ()
          (when old
            (funcall fn nil))))))
